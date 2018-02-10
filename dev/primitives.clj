(ns primitives
  (:require [lift.lang.type :as t]))

(t/def =-Int->Int (Int -> Int -> Boolean))
(defn =-Int->Int [^Integer x ^Integer y]
  (.equals x y))

(t/def =-Long->Long (Long -> Long -> Boolean))
(defn =-Long->Long [^Long x ^Long y]
  (.equals x y))

(t/def str->Long (String -> Int))
(defn str->Long [s] (Long. s))
