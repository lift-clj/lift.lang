(ns lift.lang.interface
  (:refer-clojure :exclude [type])
  (:require
   [clojure.core :as c]
   [lift.lang.signatures :as sig]
   [lift.lang.type :as t :refer [import-type-types]]
   [lift.lang.type.impl :as impl]
   [lift.lang.util :as u]))

(import-type-types)
;; requirements of interfaces:
;; type syntax parser & ast
;; pattern matching for impl?
;; what is the type of a thing at runtime?
;; does it matter that the underlying system has a different type?

(def types
  {Long 'Integer})

(defn type [x]
  (let [t (c/type x)]
    (or (types t) t)))

(def interfaces (atom {}))

(defn get-impl [d vars f sig arglist]
  (let [path (->> arglist
                  (map (fn [s a]
                         (and (instance? Var s)
                              (contains? vars (:a s))
                              (type a))) ;; TODO: and vargs?
                       (impl/-vec sig))
                  (filterv identity))]
    (get-in d (into path [f]))))

(defn default-impl [f sig pred t impl]
  (let [{:keys [arglist args]} (sig/arglist sig)]
    `(defn ~f ~arglist
       (let [impl# (or (get-impl ~pred
                                 '~(set (rest t))
                                 '~f
                                 ~sig
                                 ~(vec (remove #{'&} arglist)))
                       ~impl)]
         (apply impl# (apply concat ~args))))))

(defn type-sig-impl [f sig pred]
  `(swap! t/type-env assoc
          '~(u/resolve-sym f)
          (Forall. (t/ftv ~sig) (Predicated. [~pred] ~sig))))

(defmacro interface
  {:style/indent :defn}
  [t & fn-list-defaults?]
  (let [[class v] t
        pred (Predicate. class (Var. v))
        fns (sig/parse-fn-list-default fn-list-defaults?)]
    `(do
       ~@(mapcat
          (fn [[f {:keys [sig impl]}]]
            `[~(default-impl f sig pred t impl)
              ~(type-sig-impl f sig pred)])
          fns)
       '~t)))

(defmacro impl {:style/indent :defn}
  [type & impls]
  (let [pred (Predicate. (first type) (Const. (second type)))]
    `(do
       (swap! t/type-env assoc ~pred ~(sig/impl impls))
       '~type)))


;; (interface (Show a)
;;   show (a -> String))

;; (impl (Show Int)
;;   (show [a] (str a)))

;; (interface (Read a)
;;   read (String -> a))

;; (impl (Read Int)
;;   (read [s] (Integer. s)))

;; (interface (Functor f)
;;   map ((a -> b) -> f a -> f b))

;; (interface (Functor m => Monad m)
;;            (>>=    (m a -> (a -> m b) -> m b))
;;   >>     (m a -> m b -> m b)
;;   return (a -> m a)
;;   (default
;;    (return [a] (pure a))))

;; (impl (Functor Maybe)
;;   (map [_  Nothing] Nothing)
;;   (map [f (Just a)] (Just (f a))))

;; (interface (Eq a)
;;   =    (a -> a... -> Boolean)
;;   not= (a -> a... -> Boolean)
;;   (default
;;    (=  [x y] (not (!= x y)))
;;    (!= [x y] (not (= x y)))))

;; (impl (Eq Int)
;;   (= [a b] (c/= a b)))

;; (interface (Num n)
;;   (+ [n...])
;;   )

;; (defn eq [x & xs]
;;   (let [impl (get-impl 'eq x)]
;;     (apply impl x xs)))
