(ns lift.lang.interface
  (:require
   [lift.lang.signatures :as sig]
   [clojure.core :as c]))

;; requirements of interfaces:
;; type syntax parser & ast
;; pattern matching for impl?
;; what is the type of a thing at runtime?
;; does it matter that the underlying system has a different type?

(def interfaces (atom {}))

(defn- add-interface! [type fn-list])
(defn- add-impl! [type impl fn-list])

(defn- get-impl [d f sig]
  ;; (get @interfaces f)
  )

(defmacro interface
  {:style/indent :defn}
  [t & fn-list-defaults?]
  (let [dict (first t)
        fns (sig/parse-fn-list-default fn-list-defaults?)]
    `(do
       (def ~dict (atom {}))
       ~@(->> fns
              (mapcat
               (fn [[f {:keys [sig impl]}]]
                 (let [{:keys [arglist args]} (sig/arglist sig)]
                   `((defn ~f ~arglist
                       (let [impl# (or (get-impl (deref ~dict) ~f ~sig)
                                       ~impl)]
                         (apply impl# (apply concat ~args)))))))))
       '~t)))

(interface (Eq a)
  (=    (a -> & a -> Boolean))
  (not= (a -> & a -> Boolean))
  (default
   (=    [x & xs] (apply c/= x xs))
   (not= [x & xs] (apply c/not= x xs))))

(defmacro impl {:style/indent :defn}
  [type & impls]
  (let [dict (first type)
        itype (sig/impl-type type)]
    `(do
       (swap! ~dict merge ~(sig/impl (rest type) impls))
       '~type)))

(impl (Eq Int)
  (=    [x & xs] (apply c/= x xs))
  (not= [x & xs] (apply c/= x xs)))

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
