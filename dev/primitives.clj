(ns primitives
  (:require [lift.lang.type :as t]))

(t/def =Character (Character -> Character -> Boolean))
(defn =Character [^Character x ^Character y] (.equals x y))

(t/def =Integer (Integer -> Integer -> Boolean))
(defn =Integer [^Integer x ^Integer y] (.equals x y))

(t/def =Long (Long -> Long -> Boolean))
(defn =Long [^Long x ^Long y] (.equals x y))

(t/def readCharacter (String -> Character))
(defn readCharacter [s]
  (or (->> s (re-matches #"^\\(.)$") second first)
      (throw (Exception. (str "Not a Character: " s)))))

(t/def readLong (String -> Long))
(defn readLong [s] (Long. s))
