(ns lift.lang.interface
  (:require
   [lift.lang.signatures :as sig]))

;; requirements of interfaces:
;; type syntax parser & ast
;; pattern matching for impl?
;; what is the type of a thing at runtime?
;; does it matter that the underlying system has a different type?

(def interfaces (atom {}))

(defn- add-interface! [type fn-list])
(defn- add-impl! [type impl fn-list])

(defn- get-impl [f arglist]
  (get @interfaces f)
  )

(defmacro interface
  {:style/indent :defn}
  [t & fn-list-defaults?]
  (let [fns (sig/parse-fn-list-default fn-list-defaults?)]
    (->> fns
         (map (fn [[f {:keys [sig impl]}]]
                (let [arglist (sig/arglist sig)
                      dict (symbol (str f "-dispatch"))]
                  `(do
                     (def ~dict (atom {}))
                     (defn ~f ~arglist
                      (let [impl# (or (get-impl (deref ~dict) ~f ~arglist) ~impl)]
                        (apply impl# ~@arglist)))))))
         (cons 'do))))

(defmacro impl {:style/indent :defn} [& decl])

(interface (Show a)
  show (a -> String))

(impl (Show Int)
  (show [a] (str a)))

(interface (Read a)
  read (String -> a))

(impl (Read Int)
  (read [s] (Integer. s)))

(interface (Functor f)
  map ((a -> b) -> f a -> f b))

(interface (Functor m => Monad m)
           (>>=    (m a -> (a -> m b) -> m b))
  >>     (m a -> m b -> m b)
  return (a -> m a)
  (default
   (return [a] (pure a))))

(impl (Functor Maybe)
  (map [_  Nothing] Nothing)
  (map [f (Just a)] (Just (f a))))

(interface (Eq a)
  =    (a -> a... -> Boolean)
  not= (a -> a... -> Boolean)
  (default
   (=  [x y] (not (!= x y)))
   (!= [x y] (not (= x y)))))

(impl (Eq Int)
  (= [a b] (c/= a b)))

(interface (Num n)
  (+ [n...])
  )

(defn eq [x & xs]
  (let [impl (get-impl 'eq x)]
    (apply impl x xs)))
