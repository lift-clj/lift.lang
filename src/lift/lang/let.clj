(ns lift.lang.let
  (:require
   [lift.lang.case :as case]))

(defn destructure-bindings [prjs bindings expr]
  (let [gs (gensym)]
    `[~gs ~expr ~@(mapcat (fn [b p] `[~b (~p ~gs)]) bindings prjs)]))

(defn tupl-binding [bindings expr]
  (destructure-bindings (case/tuple-prjs (count bindings)) bindings expr))

(defn dtor-binding [[dtor & bindings] expr]
  (destructure-bindings (-> dtor resolve meta :prj :fs) bindings expr))

(defn destructuring-let [bindings expr]
  (let [bindings (->> (partition 2 bindings)
                      (mapcat (fn [[b e]]
                                (cond
                                  (case/tupl? b)
                                  (tupl-binding b e)
                                  (case/dtor? b)
                                  (dtor-binding b e)
                                  :else
                                  [b e]))))]
    `(let [~@bindings] ~expr)))
