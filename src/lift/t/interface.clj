(ns lift.t.interface)

;; requirements of interfaces:
;; type syntax parser & ast
;; pattern matching for impl?

(def interfaces (atom {}))

(defn- add-interface! [type fn-list])
(defn- add-impl! [type impl fn-list])
(defn- get-impl [f x])

(defmacro interface
  {:style/indent :defn}
  [t & fn-list-defaults?]
  (let [sig-map  (partition-all 2 fn-list-defaults?)
        default? (last sig-map)
        defaults (and (= 1 (count default?))
                      (seq? (first default?))
                      (= 'default (ffirst default?))
                      (nfirst default?)) ;; not best parsing here
        sig-map  (if defaults (butlast sig-map) sig-map)])
  )

(nfirst '((default (= []) (not=))))

(defmacro impl {:style/indent :defn} [& decl])

(interface (Show a)
  show (a -> String))

(impl (Show Int)
  (show [a] (str a)))

(interface (Read a)
  (read [String] -> a))

(impl (Read Int)
  (read [s] (Integer. s)))

(interface (Functor f)
  map ((a -> b) -> f a -> f b))

(interface (Functor m => Monad m)
  >>=    (m a -> (a -> m b) -> m b)
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
