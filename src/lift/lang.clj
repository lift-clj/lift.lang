(ns lift.lang)

(defn require [& stuff])

(defn lang [x]
  (if (= x 'lift)
    (do (doseq [[sym v] (ns-map *ns*)]
          (ns-unmap *ns* sym))
        (intern *ns* 'require #'require)
        :done)
    :oops
    ))
