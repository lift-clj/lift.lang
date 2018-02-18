(ns lift.lang.prim
  (:require [lift.lang.type :as t])
  (:import
   [clojure.lang Keyword Ratio Symbol]))

(t/def =Character (Character -> Character -> Boolean))
(defn =Character [^Character x ^Character y] (.equals x y))

(t/def =String (String -> String -> Boolean))
(defn =String [^String x ^String y] (.equals x y))

(t/def =Integer (Integer -> Integer -> Boolean))
(defn =Integer [^Integer x ^Integer y] (.equals x y))

(t/def =Long (Long -> Long -> Boolean))
(defn =Long [^Long x ^Long y] (.equals x y))

(t/def readCharacter (String -> Character))
(defn readCharacter [^String s]
  (or (->> s (re-matches #"^\\(.)$") second first)
      (throw (Exception. (str "Not a Character: " s)))))

(t/def readLong (String -> Long))
(defn readLong [^String s] (Long. s))

(t/def eq (a -> a -> Boolean))
(def eq =)

(t/def t/unmatched-case-error (a -> b))

(t/def nameKeyword (Keyword -> String))
(def nameKeyword name)

(t/def nameSymbol (Symbol -> String))
(def nameSymbol name)

(t/def +Long     (Long -> Long -> Long))
(defn  +Long     [x y] (+ x y))
(t/def *Long     (Long -> Long -> Long))
(defn  *Long     [x y] (* x y))
(t/def -Long     (Long -> Long -> Long))
(defn  -Long     [x y] (- x y))
(t/def divLong   (Long -> Long -> Double))
(defn  divLong   [x y] (double (/ x y)))
(t/def +Double   (Double -> Double -> Double))
(defn  +Double   [x y] (+ x y))
(t/def *Double   (Double -> Double -> Double))
(defn  *Double   [x y] (* x y))
(t/def -Double   (Double -> Double -> Double))
(defn  -Double   [x y] (- x y))
(t/def divDouble (Double -> Double -> Double))
(defn  divDouble [x y] (/ x y))
