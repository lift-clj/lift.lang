(ns demo
  {:lang :lift/clojure}
  (:refer-clojure :exclude [+ * - / = case defn name read not=])
  (:require [lift.lang :refer :all]))

;; (not= 1 (coerce "1"))

;; (case [Nothing 2]
;;   [(Just a) b] a
;;   [Nothing  _] 0)

;; (t/def (List a))
;; (t/def (Vector a))
;; (t/def instance? (Class -> a -> Boolean))
;; (t/def identity  (a -> a))
;; (t/def partial   ((a -> b -> c) -> a -> (b -> c)))
;; (t/def inc       (Int -> Int))
;; (t/def pos?      (Int -> Boolean))
;; (t/def str       (a -> String))
;; (t/def nth       (Vector a -> Int -> a))
;; (t/def name      (Keyword -> String))
;; (t/def =         (a -> a -> Boolean))
;; (t/def +         (Int -> Int -> Int))
;; (t/def *         (Int -> Int -> Int))
;; (t/def /         (Int -> Int -> Ratio))
;; (t/def double    (Ratio -> Double))
;; (t/def list      (List a))
;; (t/def cons      (a -> (List a) -> (List a)))
;; (t/def first     ((List a) -> a))
;; (t/def vector    (Vector a))
;; (t/def conj      (Vector a -> a -> Vector a))
;; (t/def map.      {})
;; (t/def get       ({l a | r} -> l -> a))
;; (t/def assoc     ({| r} -> l -> a -> {l a | r}))
;; (t/def dissoc    ({l a | r} -> l ->  {| r}))
;; (t/def keyword   (String -> Keyword))
;; (t/def reverse   (List a -> List a))
;; (t/def map       ((a -> b) -> (List a) -> (List b)))

;; (fn [a b] a)

;; (get {:a 1 :b 2} :a)

(assoc {} :a 2 :b 2 :c "test")

;; (:a {:a 1})
