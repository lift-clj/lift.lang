(ns lift.lang.interface
  (:refer-clojure :exclude [type])
  (:require
   [clojure.core :as c]
   [lift.lang.signatures :as sig]
   [lift.lang.type :as type]
   [lift.lang.type.base :as base]
   [lift.lang.unification :as unify]
   [lift.lang.util :as u]
   [lift.lang.analyze :as ana]
   [lift.lang.rewrite :as rewrite]
   [lift.lang.inference :as infer]
   [lift.lang.type.def :as def]
   [lift.lang.defn :as defn]
   [lift.lang.case :as case]
   [lift.lang.type.spec :as spec]
   [clojure.spec.alpha :as s]
   [lift.lang.type.data :as data]))

(base/import-container-types)
(base/import-syntax-types)
(base/import-type-types)
;; requirements of interfaces:
;; type syntax parser & ast
;; pattern matching for impl?
;; what is the type of a thing at runtime?
;; does it matter that the underlying system has a different type?

(s/def ::signature
  (s/and seq? (s/cat :f simple-symbol? :sig ::spec/type-re)))

(s/def ::default-impl
  (s/and seq? (s/cat :f simple-symbol? :arglist vector? :expr any?)))

(s/def ::match-impl
  (s/and seq?
         (s/cat :f simple-symbol?
                :impls (s/+ (s/and seq? (s/cat :arglist vector? :expr any?))))))

(s/def ::impl
  (s/or :default ::default-impl :match ::match-impl))

(s/def ::interface
  (s/cat :fn-list (s/+ ::signature)
         :default (s/? (s/and seq? (s/cat :default #{'default} :impls (s/+ ::impl))))))

(defn parse [expr]
  (let [c (s/conform ::interface expr)]
    (if (s/invalid? c)
      (throw
       (Exception. (str "Invalid interface signature\n"
                        (with-out-str (pprint expr))
                        \newline
                        (with-out-str (s/explain ::interface expr)))))
      (reduce (fn [i {:keys [f arglist expr] :as x}]
                (if (contains? i f)
                  (assoc-in i [f :impl] (list 'fn arglist expr))
                  (throw (Exception. (str "Default impl not in interface " f)))))
              (->> (:fn-list c)
                   (map (comp (juxt :f identity) #(update % :sig spec/parse)))
                   (into {}))
              (-> c :default :impls)))))

(def types
  {Long 'Integer})

(defn type [x]
  (let [t (c/type x)]
    (or (types t) t)))

(defn uncurry [f]
  (fn [& args]
    (reduce #(% %2) f args)))

(defn get-impl [d vars f sig arglist]
  (let [sub (or (some->> arglist
                         (map (fn [s a]
                                (and (instance? Var s)
                                     (contains? vars (:a s))
                                     (type/sub {(:a s) (ana/type (Literal. a))})))
                              (sig/arrseq sig))
                         (filter identity)
                         (seq)
                         (apply unify/compose))
                type/id)
        d' (type/substitute d sub)]
    (uncurry
     (eval
      (rewrite/emit
       (rewrite/rewrite
        @type/env
        nil
        (get (get @type/env d') (u/resolve-sym f))))))))

(defn default-impl [f sig pred t impl]
  (let [{:keys [arglist args]} (sig/arglist sig)]
    `(defn ~f ~arglist
       (let [impl# (or (get-impl ~pred
                                 '~(set (rest t))
                                 '~f
                                 ~sig
                                 ~(vec (remove #{'&} arglist)))
                       ~impl
                       )]
         (apply impl# (apply concat ~args))))))

(defn type-sig-impl [f sig pred]
  `(infer/intern '~(u/resolve-sym f)
                 (Forall. (type/ftv ~sig) (Predicated. [~pred] ~sig))))

(defn interface*
  {:style/indent :defn}
  [t fn-list-defaults?]
  (let [[class & as] t
        pred (Predicate. class (mapv #(Var. %) as))
        fns  (parse fn-list-defaults?)]
    `(do
       ~@(map (fn [[f _]] `(declare ~f)) fns)
       ~@(mapcat
          (fn [[f {:keys [sig impl]}]]
            `[~(default-impl f sig pred t impl)
              ~(type-sig-impl f sig pred)])
          fns)
       (infer/intern '~class ~pred)
       '~t)))

(defmacro interface
  {:style/indent :defn}
  [type & decl]
  (interface* type decl))

(def clojure-imports
  '#{Keyword Ratio Symbol})

(def literal-containers
  '#{List Vector})

(defn resolve-type-param [a]
  (let [r (resolve a)]
    (cond (contains? clojure-imports a)
          a
          (contains? literal-containers a)
          (symbol "lift.lang" (name a))
          (class? r)
          (symbol (.getSimpleName r))
          :else
          (def/resolve-sym a))))

(defn check-impl [pred sub f code]
  (let [f        (u/resolve-sym f)
        _Gamma        (assoc @type/env pred ::temp)
        code     (u/macroexpand-all code)
        [s1 syn] (infer/checks _Gamma code)
        [e t]    (type/substitute syn s1)
        [as pt]  (get _Gamma f)
        _        (assert pt (format "Symbol %s not found in env" f))
        [ps t']  pt
        _        (assert t')
        sigma    (infer/instantiate (Forall. as (type/substitute t' sub)))
        [s p]    (infer/rel-unify _Gamma t sigma)
        [_ t]    (infer/release t)]
    (type/substitute (base/$ e t) s)))

(defn non-match-impl [pred sub {:keys [f arglist expr]}]
  (check-impl pred sub f (list 'fn arglist expr)))

(defn match-impl [pred sub {:keys [f impls]}]
  (let [n    (count (:arglist (first impls)))
        vs   (defn/vars n)
        code `(fn [~@vs]
                   ~(case/case*
                     (case/tuple n vs)
                     (mapcat (fn [{:keys [arglist expr]}]
                               `[~(case/tuple n arglist) ~expr])
                             impls)))]
    (check-impl pred sub f code)))

(defn q1 [x] (list 'quote x))

(defn impl-dict [pred sub impls]
  (let [conformed (u/assert-conform (s/coll-of ::impl) impls)
        f     (fn [[t c]]
                (case t
                  :default (non-match-impl pred sub c)
                  :match   (match-impl pred sub c)))]
    (into {} (map (juxt (comp q1 u/resolve-sym :f) f) conformed))))

(defn impl [[tag & as] impls]
  (let [as     (map resolve-type-param as)
        consts (mapv #(Const. %) as)
        tag-ts (map #(or (some-> (type/find-type @type/env %) infer/instantiate)
                         (Const. %)) as)
        [_ bs] (get @type/env tag)
        pred   (Predicate. tag consts)
        sub    (->> (map (fn [a [b]] [b a]) tag-ts bs) (into {}) type/sub)]
    `(do
       (infer/intern ~pred ~(impl-dict pred sub impls))
       '~pred)))

(interface (Madeup a b)
  (madeup (a -> b -> Boolean)))

(defn no-impl-error [impl a]
  (throw (ex-info (format "No impl for %s %s" impl (pr-str a))
                  {:ex-type ::no-impl :impl impl :type a})))

(def interfaces (ref {}))

(defn definterface* [name arglist])

(def RuntimeFallback (Object.))
(require '[lift.lang :refer [Just Nothing Left Right True False]])
(def = nil)
(defmulti = identity)
(defmethod = :default [_])
(defmethod = 'Long  [_] (fn [x y] (c/= x y)))
(defmethod = 'String  [_] (fn [x y] (c/= x y)))
(defmethod = 'lift.lang/Maybe [_]
  (fn [a]
    (fn [x y]
      (case/pcase [x y]
        [(Just x) (Just y)] ((= a) x y)
        [Nothing   Nothing] true
        [_         _      ] false))))

(defmethod = 'lift.lang/Either [_]
  (fn [a b]
    (fn [x y]
      (case/pcase [x y]
        [(Left  x) (Left  y)] ((= a) x y)
        [(Right x) (Right y)] ((= b) x y)
        ;; TODO: this ^^ implies there must be some type analysis to see which
        ;; a/b are involved
        ;; There will be type analysis done here, when we type-check the
        ;; definition.
        [_         _        ] false))))

(infer/resolve `madeup)

(defmethod = RuntimeFallback [_]
  (fn [x y]
    (let [tx (data/type x)]
      (cond (instance? Const tx)
            (if-let [= (= (:x tx))]
              (= x y)
              (no-impl-error '= tx))
            (instance? Container tx)
            (let [{:keys [tag args]} tx]
              (if-let [tag= (= tag)]
                (let [argtypes (map (fn [a] (if (= a) a RuntimeFallback)) args)]
                  ((apply tag= argtypes) x y))
                (no-impl-error '= tx)))
            :else
            (no-impl-error '= tx)))))

((= RuntimeFallback) (Just 1) (Just 1))
((= RuntimeFallback) (Just (Just (Just Nothing))) (Just (Just (Just 1))))

((= RuntimeFallback) (Right "Error") (Right "Error1"))
((= RuntimeFallback) (Right 0) (Right 0))
((= RuntimeFallback) (Left 1) (Left 1))
((= RuntimeFallback) (Left 2) (Left 1))
((= RuntimeFallback) (Left 1) (Right 1))
((= RuntimeFallback) (Right 1) (Left 1))


;; (((= 'Maybe) 'Long) (Just 1) (Just 1))
