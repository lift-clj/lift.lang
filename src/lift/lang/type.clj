(ns lift.lang.type
  (:refer-clojure :exclude [case def])
  (:require
   [clojure.set :refer [difference union]]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.f.functor :as f]
   [lift.lang.util :refer :all]
   [clojure.walk :as walk]
   [clojure.java.io :as io]
   [clojure.core :as c])
  (:import
   [clojure.lang Indexed IPersistentVector]))

(alias 'c 'clojure.core)

(defprotocol Ftv (ftv [x]))
(defprotocol Show (show [_]))
(defprotocol Substitutable (substitute [x s]))

(defrecord Unit  []
  Indexed (nth [_ i _] (nth [] i nil)))
(defrecord Const [x]
  Indexed (nth [_ i _] (nth [x] i nil)))
(defrecord Var   [val]
  Indexed (nth [_ i _] (nth [val] i nil)))
(defrecord Arrow [in out]
  Indexed (nth [_ i _] (nth [in out] i nil)))

(defrecord Vargs [a]
  Indexed (nth [_ i _] (nth [a] i nil))
  Show    (show [_] (format "& %s" (pr-str a))))

(defrecord Env []
  Functor
  (-fmap [x f] (map-vals f x)))

(defrecord VarSet [vs]
  Ftv
  (ftv [x] (set (mapcat ftv (vals vs))))
  Functor
  (-fmap [x f] (assoc x :vs (map-vals f vs))))

(defrecord Scheme [t vars])

(deftype Container [tag args]
  clojure.lang.IHashEq
  (equals [_ other] (and (instance? Container other) (= tag (.-tag other))))
  (hasheq [_] (hash tag))
  (hashCode [_] (.hashCode tag))
  Indexed
  (nth [_ i _] (nth [tag args] i nil))
  Ftv
  (ftv [_] (set (mapcat ftv args)))
  Substitutable
  (substitute [_ s]
    (Container. tag (map #(substitute % s) args)))
  Show
  (show [x]
    (if args
      (format "(%s %s)" (str tag) (string/join ", " (map pr-str args)))
      (str tag))))

(deftype RowEmpty []
  Indexed
  (nth [_ i _] (nth [] i nil))
  clojure.lang.IHashEq
  (equals [_ other] (instance? RowEmpty other))
  (hasheq [_] (hash {}))
  (hashCode [_] (.hashCode {}))
  Ftv
  (ftv [_] #{})
  Substitutable
  (substitute [x s] x)
  Show
  (show [_] "{}"))

(deftype Row [k v tail]
  Indexed
  (nth [_ i _] (nth [k v tail] i nil))
  clojure.lang.IHashEq
  (equals [_ other]
    (and (instance? Row other) (= k (.-k other)) (= v (.-v other))))
  (hasheq [_] (hash [k v]))
  (hashCode [_] (.hashCode [k v]))
  Ftv
  (ftv [_] (union (ftv v) (ftv tail)))
  Substitutable
  (substitute [_ s]
    (Row. (substitute k s) (substitute v s) (substitute tail s)))
  Show
  (show [x] (format "%s : %s, %s" (pr-str k) (pr-str v) (pr-str tail))))

(deftype Record [row]
  Indexed
  (nth [_ i _] (nth [row] i nil))
  clojure.lang.IHashEq
  (equals [_ other] (and (instance? Record other) (= row (.-row other))))
  (hasheq [_] (hash row))
  (hashCode [_] (.hashCode row))
  Ftv
  (ftv [_] (ftv row))
  Substitutable
  (substitute [_ s] (Record. (substitute row s)))
  Show
  (show [x] (format "{%s}" (pr-str row))))

(deftype Constraint [tag a]
  clojure.lang.IHashEq
  (equals [_ other] (and (instance? Constraint other)
                         (= tag (.-tag other))
                         (= a (.-a other))))
  (hasheq [_] (hash a))
  (hashCode [_] (.hashCode a))
  Indexed
  (nth [_ i _] (nth [a] i nil))
  Ftv
  (ftv [_] (ftv a))
  Substitutable
  (substitute [_ s] (Constraint. tag (substitute a s)))
  Show
  (show [_] (format "%s %s" (name tag) (pr-str a))))

(deftype Quantified [constraints a]
  Indexed
  (nth [_ i _] (nth [constraints a] i nil))
  Ftv
  (ftv [_] (difference (ftv a) (set (mapcat ftv constraints))))
  Substitutable
  (substitute [_ s]
    (Quantified. constraints (substitute a s)))
  Show
  (show [_]
    (format "%s => %s" (string/join "," (map pr-str constraints)) (pr-str a))))

(defrecord Literal [a]
  Indexed (nth [_ i _] (c/case i 0 a nil))
  f/Functor (f/-map [x f] x))

(defrecord Symbol [a]
  Indexed (nth [_ i _] (c/case i 0 a nil))
  f/Functor (f/-map [x f] x))

(defrecord Variadic [vfns]
  Indexed (nth [_ i _] (nth [vfns] i nil))
  f/Functor (f/-map [_ f] (Variadic. (f/map f vfns)))
  Show      (show [_]
              (->> vfns
                   (map (fn [[xs e]] (format "(%s %s)" (pr-str xs) (pr-str e))))
                   (string/join "\n ")
                   (format "(fn\n %s)"))))

(defrecord Lambda [x e]
  Indexed (nth [_ i _] (c/case i 0 x 1 e nil))
  f/Functor (f/-map [_ f] (Lambda. x (f e))))

(defrecord Apply [e1 e2]
  Indexed (nth [_ i _] (c/case i 0 e1 1 e2 nil))
  f/Functor (f/-map [_ f] (Apply. (f e1) (f e2))))

(defrecord Let [x e1 e2]
  Indexed (nth [_ i _] (c/case i 0 x 1 e1 2 e2 nil))
  f/Functor (f/-map [_ f] (Let. x (f e1) (f e2))))

(defrecord If [cond then else]
  Indexed (nth [_ i _] (c/case i 0 cond 1 then 2 else nil))
  f/Functor (f/-map [_ f] (If. (f cond) (f then) (f else))))

;; Records
(defrecord Select [rec label]
  Indexed (nth [_ i _] (c/case i 0 rec 1 label nil))
  f/Functor (f/-map [_ f] (Select. (f rec) (f label))))

(defrecord Extend   [rec label value])
(defrecord Restrict [rec label])

(defrecord Vector [xs]
  Indexed       (nth [_ i _] (nth [xs] i nil))
  f/Functor     (f/-map [_ f] (Vector. (f/-map xs f)))
  Show          (show [_] (format "[%s]" (string/join " " (f/map pr-str xs))))
  Substitutable (substitute [_ s] (Vector. (f/map #(substitute % s) xs)))
  Ftv           (ftv [_] (set (mapcat ftv xs))))

(defrecord Map [r]
  f/Functor (f/-map [_ f] (Map. (f/-map r f)))
  Show (show [_] (->> (map (fn [[k v]] (str k " " (pr-str v))) r)
                      (string/join ", ")
                      (format "{%s}"))))

(defrecord Tuple [xs]
  Indexed       (nth [_ i _] (nth [xs] i nil))
  f/Functor     (f/-map [_ f] (Tuple. (f/-map xs f)))
  Show          (show [_] (format "[%s]" (string/join " " (f/map pr-str xs))))
  Substitutable (substitute [_ s] (Tuple. (f/map #(substitute % s) xs)))
  Ftv           (ftv [_] (set (mapcat ftv xs))))

(extend-protocol Ftv
  Unit   (ftv [x] #{})
  Const  (ftv [x] #{})
  Var    (ftv [x] #{(:val x)})
  Arrow  (ftv [x] (union (ftv (:in x)) (ftv (:out x)))))

(extend-protocol Substitutable
  Unit   (substitute [x s] x)
  Const  (substitute [x s] x)
  Var    (substitute [x s] (get s (:val x) x))
  Arrow  (substitute [x s]
           (Arrow. (substitute (:in x) s) (substitute (:out x) s))))

(extend-protocol Ftv
  Env               (ftv [x] (->> x :type vals (mapcat ftv) set))
  Scheme            (ftv [x] (difference (ftv (:t x)) (set (:vars x))))
  IPersistentVector (ftv [x] (set (mapcat ftv x)))
  nil               (ftv [x] #{}))

(extend-protocol Substitutable
  Env    (substitute [x s] (update x :type f/-map #(substitute % s)))
  Scheme (substitute [{:keys [vars t] :as x} s]
           (update x :t substitute (apply dissoc s vars)))
  IPersistentVector
  (substitute [x s]
    (f/map #(substitute % s) x)))

(defrecord Substitution []
  Functor
  (-fmap [x f] (map-vals f x))
  Show
  (show [x] (str (->> x
                      (map (fn [[k v]] (str (pr-str v) \/ (pr-str k))))
                      (string/join ", ")
                      (str "S[")) \])))

(defmethod print-method Unit      [x w] (.write w (show x)))
(defmethod print-method Const     [x w] (.write w (show x)))
(defmethod print-method Var       [x w] (.write w (show x)))
(defmethod print-method Arrow     [x w] (.write w (show x)))
(defmethod print-method Container [x w] (.write w (show x)))
(defmethod print-method Vargs     [x w] (.write w (show x)))

(defmethod print-method Env    [x w]
  (.write w
          (format "[Sub %s\nType %s]"
                  (pr-str (:sub x))
                  (pr-str (:type x)))))

(defmethod print-method Scheme [x w] (.write w (show x)))

(defmethod print-method Substitution [x w] (.write w (show x)))

(defmethod print-method Literal  [x w] (.write w (show x)))
(defmethod print-method Symbol   [x w] (.write w (show x)))
(defmethod print-method Variadic [x w] (.write w (show x)))
(defmethod print-method Lambda   [x w] (.write w (show x)))
(defmethod print-method Apply    [x w] (.write w (show x)))
(defmethod print-method Let      [x w] (.write w (show x)))
(defmethod print-method If       [x w] (.write w (show x)))

(defmethod print-method Vector  [x w] (.write w (show x)))
(defmethod print-method Map     [x w] (.write w (show x)))
(defmethod print-method Tuple   [x w] (.write w (show x)))

(extend-protocol Show
  Unit      (show [_] "()")
  Const     (show [x] (str (:x x)))
  Var       (show [x] (pr-str (:val x)))
  Arrow     (show [x] (format "(%s -> %s)" (pr-str (:in x)) (pr-str (:out x))))
  Env       (show [x] (format "Γ %s" (string/join (map pr-str x))))
  Scheme    (show [x] (format "(Scheme %s %s)" (pr-str (:t x)) (pr-str (:vars x)))))

(extend-protocol Show
  Literal (show [x] (str (:a x)))
  Symbol  (show [x] (str (:a x)))
  Lambda  (show [[x e]] (format "(fn %s %s)" (pr-str x) (pr-str e)))
  Apply   (show [x] (format "(%s %s)" (pr-str (:e1 x))
                            (string/join " " (map pr-str (:xs (:e2 x))))))
  Let     (show [x] (format "(let [%s %s] %s)"
                            (pr-str (:x x))
                            (pr-str (:e1 x))
                            (pr-str (:e2 x))))
  If      (show [x] (format "(if %s %s %s)"
                            (pr-str (:cond x))
                            (pr-str (:then x))
                            (pr-str (:else x)))))

(defn arrow [in out]
  (Arrow. in out))

(defn sub [m]
  (merge (Substitution.) m))

(defn env [m]
  (merge (Env.) m))

(def type-env (atom (env {})))

(defn ex-unknown-type [t]
  (throw (ex-info (format "Unknown Type %s" (pr-str t))
                  {:type ::unknown-type :t t})))

(s/def ::unit #{()})

(s/def ::type-name
  (s/and symbol? #(re-matches #"^[A-Z][A-z0-9]*$" (name %))))

(s/def ::type-const qualified-keyword?)

(s/def ::type-var
  (s/and simple-symbol? #(re-matches #"^[a-z\-]+$" (name %))))

(s/def ::arrow-regex
  (s/cat :type ::retype :more (s/+ (s/cat :-> #{'->} :type ::retype))))

(s/def ::arrow-parens
  (s/and seq? ::arrow-regex))

(s/def ::arrow
  (s/alt :noparens ::arrow-regex
         :parens   ::arrow-parens))

(s/def ::parameterized-regex
  (s/cat :type-name ::type-name :args (s/+ ::type)))

(s/def ::parameterized
  (s/alt :parens   (s/and seq? ::parameterized-regex)
         :noparens ::parameterized-regex))

(s/def ::rec-cons
  (s/alt :noparens (s/cat :type-name ::type-name
                          :recmap (s/map-of keyword? ::type))
         :parens (s/and seq? (s/cat :type-name ::type-name
                                    :recmap (s/map-of keyword? ::type)))))

(s/def ::row
  (s/map-of (s/or :type-var ::type-var :| #{'|}) ::type))

(s/def ::type
  (s/or :unit          ::unit
        :type-name     ::type-name
        :type-const    ::type-const
        :type-var      ::type-var
        :arrow         ::arrow-parens
        :parameterized ::parameterized
        :rec-cons      ::rec-cons
        :row           ::row))

(s/def ::retype
  (s/alt :unit          ::unit
         :type-name     ::type-name
         :type-const    ::type-const
         :type-var      ::type-var
         :arrow         ::arrow-parens
         :parameterized ::parameterized
         :rec-cons      ::rec-cons
         :row           ::row))

(s/def ::product
  (s/cat :type-cons ::type-cons := #{'=} :value-cons ::parameterized))

(s/def ::type-cons
  (s/alt :simple-type ::type-name
         :parameterized ::parameterized))

(s/def ::sum-cons
  (s/cat :type ::retype
         :more (s/+ (s/cat :| #{'|} :type ::retype))))

(s/def ::sum
  (s/cat :type-cons ::type-cons := #{'=} :sum-cons ::sum-cons))

(s/def ::record
  (s/cat :type-cons ::type-cons := #{'=} :rec-cons ::rec-cons))

(s/def ::data
  (s/or :product ::product :sum ::sum :record ::record))


(def construct nil)
(defmulti construct first)

(defmethod construct :unit  [[_ _]] (Unit.))

(defmethod construct :type-name [[_ ast]]
  (let [env @type-env]
    (or (get env ast)
        (let [sym (resolve-sym ast)
              t (Const. sym)]
          #_(or (when (contains? env sym) t)
                (when (contains? env t) t))
          t)
        ;(ex-unknown-type ast)
        )))

(defmethod construct :type-const [[_ ast]] (Const. ast))

(defmethod construct :type-var [[_ ast]] (Var. ast))

(defmethod construct :arrow [[_ ast]]
  (->> (map :type (:more ast))
       (cons (:type ast))
       (map construct)
       (curry arrow)))

(defmethod construct :product [[_ ast]]
  (let [{:keys [value-cons]} ast]
    (-> ast
        (update :type-cons construct)
        (assoc :value-cons (construct [:parameterized value-cons]))
        (dissoc :=))))

(defmethod construct :sum [[_ ast]]
  (let [{:keys [sum-cons]} ast]
    (-> ast
        (update :type-cons construct)
        (assoc :sum-cons (map construct (cons (:type sum-cons)
                                              (map :type (:more sum-cons)))))
        (dissoc :=))))

(defmethod construct :rec-cons [[_ [_ {:keys [type-name recmap]}]]]
  (Container. (resolve-sym type-name) [(f/map construct (VarSet. recmap))]))

(defmethod construct :record [[_ ast]]
  (-> ast
      (update :type-cons construct)
      (assoc :rec-cons (construct [:rec-cons (:rec-cons ast)]))))

(defmethod construct :simple-type [[_ type-name]]
  (Container. (resolve-sym type-name) nil))

(defmethod construct :parameterized [[_ [_ x]]]
  (let [tag (resolve-sym (:type-name x))]
    (let [args (map construct (:args x))]
      (Container. tag args))))

(defmethod construct :non-constrained [[_ ast]]
  (construct ast))

(defmethod construct :sub-arrow [[_ ast]]
  (construct ast))

(defmethod construct :noparens [[_ ast]]
  (construct [:arrow ast]))

(defmethod construct :row [[_ ast]]
  (let [r (some-> ast (get '|) second (Var.))]
    (->> (dissoc ast '|)
         (map (fn [[k v]] [(construct [:type-var k]) (construct v)]))
         (reduce (fn [row [k v]] (Row. k v row)) (or r (RowEmpty.)))
         (Record.))))

(defn type-signature [sig]
  (let [spec (if (and (seq? sig) (= (count sig) 1)) ::retype ::type)
        conformed (s/conform spec sig)]
    (if (s/invalid? conformed)
      (do
        ;; (s/explain spec sig)
        (throw (Exception. (format "Could not parse type signature %s" sig))))
      (construct (s/conform spec sig)))))

(defmacro prim [t]
  `(let [t# (Const. '~t)]
     (swap! type-env assoc '~t t#)
     t#))

(defn record-keyword [r k]
  (let [ns (or (namespace k) (name (.getName *ns*)))
        nm (name k)]
    (keyword ns (if (.startsWith nm (str r \.)) nm (str r \. nm)))))

(defmacro def
  ([type]
   `(let [type# (type-signature '~type)]
      (swap! type-env assoc type# (Scheme. type# (ftv type#)))
      type#))
  ([type sig]
   (let [m (meta sig)]
     (cond
       (symbol? type)
       `(let [type# '~(resolve-sym type)
              ts# (type-signature '~sig)]
          (swap! type-env assoc type# (with-meta (Scheme. ts# (ftv ts#)) ~m))
          type#)
       (keyword? type)
       `(let [type# ~type
              ts# (type-signature '~sig)]
          (swap! type-env assoc type# (with-meta (Scheme. ts# (ftv ts#)) ~m))
          type#)))))

(defmacro def-tuples []
  (let [fst (int \a)]
    (cons 'do
          (apply concat
                 (for [n (range 1 16)]
                   (let [ns    (range 0 n)
                         vars  (mapv (comp symbol str char (partial + fst)) ns)
                         tuple (apply list (symbol (str "Tuple" n)) vars)]
                     `((lift.lang.type/def ~tuple)
                       ~@(apply concat
                                (for [[n2 a] (map vector ns vars)]
                                  (let [proj (symbol (str "proj-" n \- n2))]
                                    `((lift.lang.type/def ~proj ~(list tuple '-> a))
                                      (defn ~proj [~'x]
                                        (nth ~'x ~n2)))))))))))))
(def-tuples)

(defn unmatched-case-error []
  (throw (ex-info "Unmatched Case" {:type :unmatched-case-error})))

(defn type-rkeyword [rkw]
  (get @type-env rkw))

(defn record-type [tagstr]
  `(deftype ~(symbol (str "__" tagstr)) [x#]
     clojure.lang.ILookup
     (valAt [_# key#]
       (get x# (record-keyword ~tagstr key#)))
     clojure.lang.Associative
     (assoc [_# key# val#]
       ;; TODO: some rule amout same ns keywords?
       (__Record. (assoc x# (record-keyword ~tagstr key#) val#)))
     Show
     (show [_#]
       (format "(%s %s)" ~tagstr (pr-str x#)))))

(defn record-defn [tagstr margs]
  (let [arglist (vec (repeatedly (count margs) gensym))]
    `(defn ~(symbol tagstr) {:style/indent :defn} ~arglist
       (new ~(symbol (str "__" tagstr))
            ~(into {} (map vector (keys margs) arglist))))))

;; (defn record-constuct [tag rec-cons]
;;   (let [tagstr (name (.-tag rec-cons))
;;         tag' (symbol tagstr)
;;         args' (:vs (first (.-args rec-cons)))
;;         val-types (vec (mapcat -destructure (vals args')))
;;         margs (zipmap (map (partial record-keyword tagstr) (keys args'))
;;                       val-types)
;;         type-expr (list tag (into {} (map (juxt key (comp :val val)) args')))]
;;     `((t/def ~type-expr)
;;       (t/def ~tag ~(interpose '-> (conj val-types type-expr)))
;;       ~(record-type tagstr)
;;       ~(record-defn tagstr margs)
;;       ~@(map (fn [[k v]] `(t/def ~k ~(concat type-expr ['-> v]))) margs)
;;       (list '~(resolve-sym (first type-expr))
;;        '~@(rest type-expr)))))

;; (defmacro data
;;   {:style/indent :defn}
;;   [& decl]
;;   (let [{:keys [type-cons] :as parsed} (construct (s/conform ::data decl))
;;         args (mapv :val (.-args type-cons))
;;         tag (symbol (name (.-tag type-cons)))
;;         type-expr (apply list tag args)]
;;     `(do
;;        ~@(if-let [rec-cons (:rec-cons parsed)]
;;            (record-constuct tag rec-cons)
;;            (apply list
;;                   `(t/def ~type-expr)
;;                   (concat
;;                    (mapcat
;;                     (fn [part]
;;                       (let [container? (instance? Container part)
;;                             tag (if container? (symbol (name (.-tag part))) (:x part))
;;                             args (if container? (mapv :val (.-args part)) [])
;;                             size (count args)
;;                             tupl (symbol "type" (str "Tuple" size))
;;                             __name (str "__" (name tag))
;;                             __type-name (symbol __name)
;;                             destructor (symbol (str __name "-destructure"))
;;                             projs-syms (mapv #(symbol "type" (str "proj-" size \- %))
;;                                              (range 0 size))
;;                             projs (symbol (str __name "-projs"))]
;;                         `((t/def ~tag ~(if container?
;;                                          (interpose '-> (conj args type-expr))
;;                                          type-expr))
;;                           ~@(when container?
;;                               `((deftype ~__type-name ~args
;;                                   Show
;;                                   (show [_#]
;;                                     (format "(%s %s)" ~(str tag)
;;                                             (string/join " " (map pr-str ~args)))))))
;;                           ~@(if container?
;;                               `((defn ~(with-meta tag {:ctor __type-name})
;;                                   ~args (new ~__type-name ~@args)))
;;                               `((def ~tag (reify Show (show [_#] ~(name tag))))))
;;                           ~@(when container?
;;                               `((def ~projs '~projs-syms)
;;                                 (t/def ~destructor
;;                                   ~(interpose '-> (into [type-expr] [(cons tupl args)])))
;;                                 (defn ~destructor [~'x]
;;                                   ~(mapv (fn [a] `(~(symbol (str ".-" a)) ~'x)) args)))))))
;;                     (or (:sum-cons parsed)
;;                         [(:value-cons parsed)]))
;;                    [type-cons]))))))

(defmethod print-method Object [x w]
  (if (satisfies? Show x)
    (.write w (show x))
    (#'clojure.core/print-object x w)))

(defn case-form [x & [pattern expr & more]]
  (if (vector? pattern)
    (let [[c & as] pattern
          __name (str "__" c)
          destructor (symbol (str __name "-destructure"))
          projs @(resolve (symbol (str __name "-projs")))
          dsym (gensym)]
      `(if (instance? ~(symbol __name) ~x)
         (let* [~@(mapcat  (fn [a p] [a `(~p (~destructor ~x))]) as projs)]
           ~expr)
         ~(if (seq more)
            (apply case-form x more)
            `(t/unmatched-case-error))))
    `(if (= ~x ~pattern)
       ~expr
       ~(if (seq more)
          (apply case-form x more)
          `(t/unmatched-case-error)))))

(defn case [x decl]
  (let [xsym (gensym)]
    `(let [~xsym ~x]
       ~(apply case-form xsym decl))))

(defn lang [x]
  (prn "#clojure/lang" x)
  (if (= 'lift x)
    (do
      (prn "<3")
      )))

(defn require-typed [& [decl & more]]
  (loop [[ns & kwargs] decl]
    (let [path (-> (name ns)
                   (string/split #"\.")
                   (->> (string/join "/"))
                   (str ".cljt"))]
      (prn (io/resource path)))
    (when (seq more) (recur more))))

(prim Symbol)
(lift.lang.type/def t/unmatched-case-error a)
(lift.lang.type/def def (Symbol -> a -> ()))
(lift.lang.type/def restrict (l -> {l a | r} ->  {| r}))
