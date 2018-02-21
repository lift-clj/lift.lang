(ns lift.lang.util
  (:refer-clojure :exclude [macroexpand])
  (:require
   [clojure.spec.alpha :as s]
   [riddley.compiler :as cmp]
   [lift.lang.type.base :as base])
  (:import
   [lift.lang.type.base Mark]))

(defn ns-qualify
  "Qualify symbol s by resolving it or using the current *ns*."
  [s]
  (if-let [ns-sym (some-> s namespace symbol)]
    (or (some-> (get (ns-aliases *ns*) ns-sym) str (symbol (name s))) s)
    (symbol (str (.name *ns*)) (str s))))


(defn ->sym [v]
  (if (class? v)
    (symbol (.getSimpleName v))
    (symbol (name (.getName (:ns (meta v))))
            (name (:name (meta v))))))

(defn resolve-sym
  ([ns s]
   (or (some-> s resolve ->sym) (ns-qualify s)))
  ([s]
   (resolve-sym *ns* s)))

(defn unification-failure [a b]
  (throw
   (Exception.
    (format "Cannot unify %s and %s"
            (pr-str a)
            ;; (type a)
            (pr-str b)
            ;; (type b)
            )
            ;;{:type :unification-failure :a a :b b}
               )))

(defn unbound-variable-error [x]
  (throw
   (ex-info (format "UnboundVariable %s" (pr-str x))
            {:type :unbound-variable :value x})))

(defn arity-error [a b]
  (throw
   (ex-info (format "Arities do not match %s %s" (pr-str a) (pr-str b))
            {:type :arity :a a :b b})))

(defn map-keys [f x]
  (into x (for [[k v] x] [(f k) v])))

(defn map-vals [f x]
  (into x (for [[k v] x] [k (f v)])))

(defn curry [op [a b & tail]]
  (if (seq tail)
    (op a (curry op (cons b tail)))
    (op a b)))

(defn assert-conform [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      (throw
       (Exception. (format "Value %s does not conform to spec %s\n%s"
                           x spec (with-out-str (s/explain spec x)))))
      c)))

(defn macroexpand
  "Expands both macros and inline functions. Optionally takes a `special-form?` predicate which
   identifies first elements of expressions that shouldn't be macroexpanded, and honors local
   bindings."
  ([x]
     (macroexpand x nil))
  ([x special-form?]
     (cmp/with-base-env
       (if (seq? x)
         (let [frst (first x)]
           (if (or (and special-form? (special-form? frst))
                   (contains? (cmp/locals) frst))
             ;; might look like a macro, but for our purposes it isn't
             x
             (let [x' (macroexpand-1 x)]
               (if (identical? x x')
                 x'
                 (macroexpand x' special-form?)))))
         x))))

(defn- do-handler [f [_ & body]]
  (list* 'do
    (doall
      (map f body))))

(defn- fn-handler [f x]
  (let [prelude (take-while (complement sequential?) x)
        remainder (drop (count prelude) x)
        remainder (if (vector? (first remainder))
                    (list remainder) remainder)
        body-handler (fn [x]
                       (cmp/with-lexical-scoping
                         (doseq [arg (first x)]
                           (cmp/register-arg arg))
                         (doall
                           (list* (first x)
                             (map f (rest x))))))]

    (cmp/with-lexical-scoping

      ;; register a local for the function, if it's named
      (when-let [nm (second prelude)]
        (cmp/register-local nm
          (list* 'fn* nm
            (map #(take 1 %) remainder))))

      (concat
        prelude
        (if (seq? (first remainder))
          (doall (map body-handler remainder))
          [(body-handler remainder)])))))

(defn- def-handler [f x]
  (let [[_ n & r] x]
    (cmp/with-lexical-scoping
      (cmp/register-local n '())
      (list* 'def (f n) (doall (map f r))))))

(defn- let-bindings [f x]
  (->> x
    (partition-all 2)
    (mapcat
      (fn [[k v]]
        (let [[k v] [k (f v)]]
          (cmp/register-local k v)
          [k v])))
    vec))

(defn- reify-handler [f x]
  (let [[_ classes & fns] x]
    (list* 'reify* classes
      (doall
        (map
          (fn [[nm args & body]]
            (cmp/with-lexical-scoping
              (doseq [arg args]
                (cmp/register-arg arg))
              (list* nm args (doall (map f body)))))
          fns)))))

(defn- deftype-handler [f x]
  (let [[_ type resolved-type args _ interfaces & fns] x]
    (cmp/with-lexical-scoping
      (doseq [arg args]
        (cmp/register-arg arg))
      (list* 'deftype* type resolved-type args :implements interfaces
        (doall
          (map
            (fn [[nm args & body]]
              (cmp/with-lexical-scoping
                (doseq [arg args]
                  (cmp/register-arg arg))
                (list* nm args (doall (map f body)))))
            fns))))))

(defn- let-handler [f x]
  (cmp/with-lexical-scoping
    (doall
      (list*
        (first x)
        (let-bindings f (second x))
        (map f (drop 2 x))))))

(defn- case-handler [f [_ ge shift mask default imap switch-type check-type skip-check]]
  (let [prefix  ['case* ge shift mask]
        suffix  [switch-type check-type skip-check]]
    (concat
      prefix
      [(f default)]
      [(let [m (->> imap
                 (map
                   (fn [[k [idx form]]]
                     [k [idx (f form)]]))
                 (into {}))]
         (if (every? number? (keys m))
           (into (sorted-map) m)
           m))]
      suffix)))

(defn- catch-handler [f x]
  (let [[_ type var & body] x]
    (cmp/with-lexical-scoping
      (when var
        (cmp/register-arg (with-meta var (merge (meta var) {:tag type}))))
      (list* 'catch type var
        (doall (map f body))))))

(defn- dot-handler [f x]
  (let [[_ hostexpr mem-or-meth & remainder] x]
    (list* '.
           (f hostexpr)
           (if (seq? mem-or-meth)
             (list* (first mem-or-meth)
                    (doall (map f (rest mem-or-meth))))
             (f mem-or-meth))
           (doall (map f remainder)))))

(defn walk-exprs
  "A walk function which only traverses valid Clojure expressions.  The `predicate` describes
   whether the sub-form should be transformed.  If it returns true, `handler` is invoked, and
   returns a transformed form.

   Unlike `clojure.walk`, if the handler is called, the rest of the sub-form is not walked.
   The handler function is responsible for recursively calling `walk-exprs` on the form it is
   given.

   Macroexpansion can be halted by defining a set of `special-form?` which will be left alone.
   Including `fn`, `let`, or other binding forms can break local variable analysis, so use
   with caution."
  ([predicate handler x]
     (walk-exprs predicate handler nil x))
  ([predicate handler special-form? x]
     (cmp/with-base-env
       (let [x (try
                 (macroexpand x special-form?)
                 (catch ClassNotFoundException _
                   x))
             walk-exprs' (partial walk-exprs predicate handler special-form?)
             x' (cond

                  (instance? Mark x)
                  (let [x'' (walk-exprs' (:a x))]
                    (if (instance? clojure.lang.IObj x'')
                      (vary-meta x'' assoc :mark true)
                      x''))

                  (and (seq? x) (= 'var (first x)) (predicate x))
                  (handler (eval x))

                  (and (seq? x) (= 'quote (first x)) (not (predicate x)))
                  x

                  (predicate x)
                  (handler x)

                  (seq? x)
                  ((condp = (first x)
                     'do     do-handler
                     'def    def-handler
                     'fn*    fn-handler
                     'let*   let-handler
                     'loop*  let-handler
                     'letfn* let-handler
                     'case*  case-handler
                     'catch  catch-handler
                     'reify* reify-handler
                     'deftype* deftype-handler
                     '.      dot-handler
                     #(doall (map %1 %2)))
                   walk-exprs' x)

                  (instance? java.util.Map$Entry x)
                  (clojure.lang.MapEntry.
                    (walk-exprs' (key x))
                    (walk-exprs' (val x)))

                  (or
                    (set? x)
                    (vector? x))
                  (into (empty x) (map walk-exprs' x))

                  (instance? clojure.lang.IRecord x)
                  x

                  (map? x)
                  (into (empty x) (map walk-exprs' x))

                  ;; special case to handle clojure.test
                  (and (symbol? x) (-> x meta :test))
                  (vary-meta x update-in [:test] walk-exprs')

                  :else
                  x)]
         (if (instance? clojure.lang.IObj x')
           (with-meta x' (merge (meta x) (meta x')))
           x')))))

;;;

(defn macroexpand-all
  "Recursively macroexpands all forms, preserving the &env special variables."
  [x]
  (walk-exprs (constantly false) nil x))
