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
   [lift.lang.type :as type]
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
         :args (s/+ (s/or :var       ::var
                          :type-name ::type-name
                          :type-app  (s/and seq? ::type-app)
                          :tuple     ::tuple
                          :arrow     (s/and seq? ::arrow)
                          :set       ::set))))

(s/def ::tuple
  (s/and vector? (s/+ (s/or :var       ::var
                            :type-name ::type-name
                            :type-app  (s/and seq? ::type-app)
                            :tuple     ::tuple
                            :arrow     (s/and seq? ::arrow)
                            :set       ::set))))

;; a -> (a -> b) -> c... -> b, Functor f -> f a
(s/def ::arrow
  (s/cat :a ::type-expr :-> #{'->} :b ::retype-expr))

(s/def ::pred
  (s/cat :pred ::type-app :=> #{'=>} :expr ::retype-expr))

(s/def ::set
  (s/and set? (s/cat :a ::type-expr)))

(s/def ::simple-expr
  (s/alt :var       ::var
         :type-name ::type-name
         :type-app  ::type-app
         :tuple     ::tuple
         :arrow     (s/and seq? ::arrow)
         :set       ::set))

(s/def ::type-expr
  (s/alt :var       ::var
         :vargs     ::vargs
         :type-name ::type-name
         :type-app  ::type-app
         :tuple     ::tuple
         :arrow     (s/and seq? ::arrow)
         :set       ::set
         :pred      ::pred))

(s/def ::retype-expr
  (s/alt :var       ::var
         :vargs     ::vargs
         :type-name ::type-name
         :type-app  ::type-app
         :tuple     ::tuple
         :arrow     ::arrow
         :set       ::set
         :pred      ::pred))

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
    :set       (Container. (Const. 'Set) [(parse-type-expr (:a x))])
    :arrow     (Arrow. (parse-type-expr (:a x)) (parse-type-expr (:b x)))
    :pred      (let [{:keys [pred expr]} x]
                 (Predicated.
                  [(Predicate. (second (:op pred))
                               (mapv parse-type-expr (:args pred)))]
                  (parse-type-expr expr)))))

(defn parse [texpr]
  (let [c (s/conform ::signature texpr)]
    (if (s/invalid? c)
      (throw (Exception. (str "Invalid type signature " (pr-str texpr))))
      (-> c
          (assoc :type :sig)
          (update :sig parse-type-expr)))))

(defn parse-tsig [sig]
  (u/assert-conform ::type-expr sig))

(defn type-signature [expr]
  (parse-type-expr (parse-tsig expr)))

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

(defn arrseq [f]
  (if (instance? Arrow f)
    (cons (:a f) (arrseq (:b f)))
    ()))
