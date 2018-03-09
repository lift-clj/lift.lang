(ns lift.lang.monad
  (:refer-clojure :exclude [do]))

(defmacro do
  {:style/indent :defn}
  [bindings expr]
  (let [mt     (second bindings)
        steps  (reverse (partition 2 bindings))
        result (reduce (fn [expr [sym mv]]
                         `(lift.lang/>>= ~mv (fn [~sym] ~expr)))
                       expr
                       steps)]
    result))
