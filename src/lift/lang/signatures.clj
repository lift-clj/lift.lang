(ns lift.lang.signatures
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.lang.type :refer :all]
   [lift.lang.util :as u]
   [clojure.core :as c]
   [lift.lang.pattern :as p]))

(import-type-types)
(import-container-types)

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
                        (with-out-str (clojure.pprint/pprint expr))
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

(defn arglist [fn-sig]
  (if (instance? Arrow fn-sig)
    (merge-with into
                (let [{{a :a :as in} :a} fn-sig
                      gs (gensym)]
                  (if (instance? Vargs in)
                    {:arglist ['& gs] :args [gs]}
                    {:arglist [gs]    :args [[gs]]}))
                (arglist (:b fn-sig)))
    []))

(p/defn tuple-arglist
  ([[Arrow [Tuple xs]]]
   (->> xs
        (map #(let [gs (gensym)]
                (if (instance? Vargs %)
                  {:arglist ['& gs] :args [gs]}
                  {:arglist [gs]    :args [[gs]]})))
        (apply merge-with into)))
  ([x] (throw (Exception. (format "Not a tupled function %s" (pr-str x))))))

(comment
  (tuple-arglist
   (Arrow. (Tuple. [(Var. 'a) (Vargs. 'a)]) (Const. 'Bool))))


(defn impl-type [type]
  (parse-type-expr [:type-app (u/assert-conform ::type-app type)]))

(defn q1 [x] (list 'quote x))

(defn impl [path impls]
  (letfn [(f [{:keys [f arglist expr]}] `(fn ~f ~arglist ~expr))]
    (let [c (u/assert-conform (s/coll-of ::default-impl) impls)]
      (assoc-in {} (map q1 path) (into {} (map (juxt (comp q1 :f) f) c))))))

(defn arrseq [f]
  (if (instance? Arrow f)
    (cons (:a f) (arrseq (:b f)))
    ()))
