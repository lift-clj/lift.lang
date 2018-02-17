(ns demo
  {:lang :lift/clojure}
  (:refer-clojure :exclude [case defn read = not=])
  (:require [lift.lang :refer :all]))

;; (not= 1 (coerce "1"))

;; (case [Nothing 2]
;;   [(Just a) b] a
;;   [Nothing  _] 0)

(fn [a b] a)
