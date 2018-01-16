(ns lift.t.signatures
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.type :refer :all]))

;; a, b, c, etc.
(s/def ::var
  (s/and simple-symbol? #(re-matches #"^[a-z][a-z0-9]*$" (name %))))

;; a...
(s/def ::vargs
  (s/and simple-symbol?
         #(re-matches #"^[a-zA-Z][a-z0-9]*\.\.\.$" (name %))))

;; String, Functor, etc.
(s/def ::type-name
  (s/and simple-symbol? #(re-matches #"[A-Z][a-zA-Z0-9]*" (name %))))

;; Functor f, (Functor f), Either (Maybe a) b
(s/def ::type-app
  (s/cat :op   (s/or :var ::var :type-name ::type-name)
         :args (s/+ (s/and ::type-expr #(not (s/valid? ::vargs %))))))

;; a -> (a -> b) -> c... -> b, Functor f -> f a
(s/def ::lambda
  (s/cat :a ::type-expr :-> #{'->} :b ::retype-expr))

(s/def ::type-expr
  (s/or :var       ::var
        :vargs     ::vargs
        :type-name ::type-name
        :type-app  ::type-app
        :lambda    ::lambda))

(s/def ::retype-expr
  (s/alt :var       ::var
         :vargs     ::vargs
         :type-name ::type-name
         :type-app  ::type-app
         :lambda    ::lambda))

;; x.y/z
(s/def ::name symbol?)

;; (= a -> a... -> Boolean)
(s/def ::signature
  (s/cat :f ::name :sig ::type-expr))

(s/def ::default-impl
  (s/and seq? (s/cat :f symbol? :arglist vector? :expr any?)))

(s/def ::interface-fn-list-default
  (s/cat :fn-list (s/+ ::signature)
         :default (s/? (s/and seq? (s/cat :default #{'default}
                                          :impls (s/+ ::default-impl))))))

(defn trim-vargs [x]
  (symbol (string/replace (name x) #"...$" "")))

(defn parse-type-expr [[t x]]
  (case t
    :var       (Var x)
    :vargs     (Vargs (trim-vargs x))
    :type-name (Name x)
    :type-app  (App (parse-type-expr (:op x)) (mapv parse-type-expr (:args x)))
    :lambda    (Fn (parse-type-expr (:a x)) (parse-type-expr (:b x)))))

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
      (throw (Exception. (str "Invalid interface signature " (pr-str expr))))
      (reduce (fn [i {:keys [f arglist expr] :as x}]
                (if (contains? i f)
                  (assoc-in i [f :impl] (list 'fn arglist expr))
                  (throw (Exception. (str "Default impl not in interface " f)))))
              (->> (:fn-list c)
                   (map (comp (juxt :f identity)
                              #(update % :sig parse-type-expr)))
                   (into {}))
              (-> c :default :impls)))))

(parse-fn-list-default '(
             =    (a -> a... -> Boolean)
             not= (a -> a... -> Boolean)
             (default
              (=    [x y] (not (!= x y)))
              (not= [x y] (not (= x y))))))


(defn arglist [fn-sig]
  (if (instance? lift.type.Fn fn-sig)
    (into (if (instance? lift.type.Vargs (.-a fn-sig))
            ['& (gensym)]
            [(gensym)])
           (arglist (.-b fn-sig)))
    []))

(parse-fn-list-default '(show (a -> String)))

{=
 {:f =,
  :sig
  (lift.type/Fn
   (lift.type/Var a)
   (lift.type/Fn (lift.type/Vargs a) (lift.type/Name Boolean))),
  :impl (fn [x y] (not (!= x y)))},
 not=
 {:f not=,
  :sig
  (lift.type/Fn
   (lift.type/Var a)
   (lift.type/Fn (lift.type/Vargs a) (lift.type/Name Boolean))),
  :impl (fn [x y] (not (= x y)))}}
