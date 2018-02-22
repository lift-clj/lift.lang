(ns lift.lang.signatures
  (:require
   [clojure.core :as c]
   [clojure.pprint :refer [pprint]]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.lang.analyze :as ana]
   [lift.lang.defn :as defn]
   [lift.lang.inference :as infer :refer [check rel-unify]]
   [lift.lang.pattern :as p]
   [lift.lang.type :as t]
   [lift.lang.type.base :as base]
   [lift.lang.type.impl :as impl]
   [lift.lang.unification :refer [unify]]
   [lift.lang.util :as u]
   [lift.lang.case :as case]
   [lift.lang.type :as type]))

(base/import-syntax-types)
(base/import-type-types)
(base/import-container-types)

;; a, b, c, etc.
(s/def ::var
  (s/and simple-symbol? #(re-matches #"^[a-z][a-z0-9]*$" (name %))))

;; a...
(s/def ::vargs
  (s/cat :& #{'&} :v ::var))

;; String, Functor, etc.
(s/def ::type-name
  (s/and simple-symbol? #(re-matches #"[A-Z][a-zA-Z0-9]*" (name %))))

;; Functor f, (Functor f), Either (Maybe a) b
(s/def ::type-app
  (s/cat :op   (s/or :var ::var :type-name ::type-name)
         :args (s/+ (s/& ::simple-expr))))

(comment

  (s/invalid? (s/conform ::type-app '[a & a]))

  (s/valid? ::type-app '[a a])

  )

(s/def ::tuple
  (s/and vector? (s/+ ::type-expr)))

;; a -> (a -> b) -> c... -> b, Functor f -> f a
(s/def ::arrow
  (s/cat :a ::type-expr :-> #{'->} :b ::retype-expr))

(s/def ::simple-expr
  (s/alt :var       ::var
         :type-name ::type-name
         :type-app  ::type-app
         :tuple     ::tuple
         :arrow     (s/and seq? ::arrow)))

(s/def ::type-expr
  (s/alt :var       ::var
         :vargs     ::vargs
         :type-name ::type-name
         :type-app  ::type-app
         :tuple     ::tuple
         :arrow     (s/and seq? ::arrow)))

(s/def ::retype-expr
  (s/alt :var       ::var
         :vargs     ::vargs
         :type-name ::type-name
         :type-app  ::type-app
         :tuple     ::tuple
         :arrow     ::arrow))

;; x.y/z
(s/def ::name symbol?)

;; (= a -> a... -> Boolean)
(s/def ::signature
  (s/and seq? (s/cat :f ::name :sig ::type-expr)))

(s/def ::default-impl
  (s/and seq? (s/cat :f symbol? :arglist vector? :expr any?)))

(s/def ::match-impl
  (s/and seq?
         (s/cat :f symbol?
                :impls (s/+ (s/and seq? (s/cat :arglist vector? :expr any?))))))

(s/def ::interface-fn-list-default
  (s/cat :fn-list (s/+ ::signature)
         :default (s/? (s/and seq? (s/cat :default #{'default}
                                          :impls (s/+ ::default-impl))))))

(defn parse-type-expr [[t x]]
  (c/case t
    :var       (Var. x)
    :vargs     (Vargs. (:v x))
    :type-name (Const. x)
    :type-app  (Container. (parse-type-expr (:op x)) (mapv parse-type-expr (:args x)))
    :tuple     (Tuple. (mapv parse-type-expr x))
    :arrow     (Arrow. (parse-type-expr (:a x)) (parse-type-expr (:b x)))))

(defn parse [texpr]
  (let [c (s/conform ::signature texpr)]
    (if (s/invalid? c)
      (throw (Exception. (str "Invalid type signature " (pr-str texpr))))
      (-> c
          (assoc :type :sig)
          (update :sig parse-type-expr)))))

(defn parse-fn-list-default [expr]
  (let [c (s/conform ::interface-fn-list-default expr)]
    (if (s/invalid? c)
      (throw
       (Exception. (str "Invalid interface signature\n"
                        (with-out-str (pprint expr))
                        \newline
                        (with-out-str (s/explain ::interface-fn-list-default expr)))))
      (reduce (fn [i {:keys [f arglist expr] :as x}]
                (if (contains? i f)
                  (assoc-in i [f :impl] (list 'fn arglist expr))
                  (throw (Exception. (str "Default impl not in interface " f)))))
              (->> (:fn-list c)
                   (map (comp (juxt :f identity)
                              #(update % :sig parse-type-expr)))
                   (into {}))
              (-> c :default :impls)))))

(comment
  (parse-fn-list-default
  '(
    (=    ([a & a] -> Boolean))
    (not= ([a & a] -> Boolean))
    (default
     (=    [x & xs] (apply c/= x xs))
     (not= [x & xs] (apply c/not= x xs)))))

  )

(p/defn curried-arglist
  ([[Arrow a b]]
   (merge-with into
               (let [gs (gensym)]
                 (if (instance? Vargs a)
                   {:arglist ['& gs] :args [gs]}
                   {:arglist [gs]    :args [[gs]]}))
               (curried-arglist b)))
  ([_] []))

(p/defn tuple-arglist
  ([[Arrow [Tuple xs]]]
   (->> xs
        (map #(let [gs (gensym)]
                (if (instance? Vargs %)
                  {:arglist ['& gs] :args [gs]}
                  {:arglist [gs]    :args [[gs]]})))
        (apply merge-with into)))
  ([x] (throw (Exception. (format "Not a tupled function %s" (pr-str x))))))

(defn arglist [t]
  (or (try (tuple-arglist t) (catch Throwable _))
      (curried-arglist t)))

(defn q1 [x] (list 'quote x))

(defn arrseq [f]
  (if (instance? Arrow f)
    (cons (:a f) (arrseq (:b f)))
    ()))

(defn check-lambda [_Gamma bindings types e]
  (let [types' (map (fn [a] (Forall. (base/ftv a) a)) types)
        _Gamma (apply assoc _Gamma (interleave bindings types'))
        [s [_ t :as e]] (infer/checks _Gamma e)
        [p1 t1] (infer/release t)
        pts (map #(infer/release (t/substitute % s)) (reverse types))
        ps  (cons p1 (map first pts))
        t2  (reduce #(Arrow. %2 %) t1 (map second pts))
        e2  (reduce #(Lambda. (Symbol. %2) %) e bindings)]
    [s (base/$ e2 (infer/with-pred _Gamma ps t2))]))

(defn check-impl [pred sub f code]
  (let [f        (u/resolve-sym f)
        _Gamma        (assoc @t/type-env pred ::temp)
        code     (u/macroexpand-all code)
        [s1 syn] (infer/checks _Gamma code)
        [e t]    (t/substitute syn s1)
        [as pt]  (get _Gamma f)
        _        (assert pt (format "Symbol %s not found in env" f))
        [ps t']  pt
        _        (assert t')
        sigma    (infer/instantiate (Forall. as (t/substitute t' sub)))
        [s p]    (rel-unify _Gamma t sigma)
        [_ t]    (infer/release t)]
    (t/substitute (base/$ e t) s)))

(defn default-impl [pred sub {:keys [f arglist expr]}]
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

(defn impl [pred sub impls]
  (let [[t c] (u/assert-conform (s/or :default (s/coll-of ::default-impl)
                                      :match   (s/coll-of ::match-impl))
                                impls)
        f     (case t
                :default (partial default-impl pred sub)
                :match   (partial match-impl pred sub))]
    (into {} (map (juxt (comp q1 u/resolve-sym :f) f) c))))

;; (@t/type-env 'Pair)
