(ns lift.lang.defn
  (:require
   [lift.lang.type.data :refer [data]]
   [lift.lang.case :as case]))

(data Tuple1 a = Tuple1 a)
(data Tuple2 a b = Tuple2 a b)
(data Tuple3 a b c = Tuple3 a b c)
(data Tuple4 a b c d = Tuple4 a b c d)
(data Tuple5 a b c d e = Tuple5 a b c d e)
(data Tuple6 a b c d e f = Tuple6 a b c d e f)
(data Tuple7 a b c d e f g = Tuple7 a b c d e f g)
(data Tuple8 a b c d e f g h = Tuple8 a b c d e f g h)
(data Tuple9 a b c d e f g h i = Tuple9 a b c d e f g h i)

(def tpl '[Tuple1 Tuple2 Tuple3 Tuple4 Tuple5 Tuple6 Tuple7 Tuple8 Tuple9])

(defn tuple [n vars]
  `(~(nth tpl (dec n)) ~@vars))

(defn vars [n]
  (take n (map (comp symbol str char) (range 97 123))))

(defn defn* [name exprs]
  (let [n  (count (ffirst exprs))
        vs (vars n)]
    `(defn ~name [~@vs]
       ~(case/case*
         (tuple n vs)
         (mapcat (fn [[match expr]] `[~(vec (tuple n match)) ~expr])
                 exprs)))))

(defn* '=
  '(([[Just a] [Just b]] (Tuple2 a b))
    ([[Just a] Nothing ] (Tuple2 a 7))
    ([Nothing  [Just b]] (Tuple2 7 b))
    ([Nothing  Nothing ] (Tuple2 7 7))))
