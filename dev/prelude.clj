(ns prelude
  (:require [lift.lang.type :as t]))

(t/def not (Boolean -> Boolean))
