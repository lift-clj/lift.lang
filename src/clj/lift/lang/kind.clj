(ns lift.lang.kind)

(def env (atom {}))

;; There are 2 kinds ✳ and ✳ -> ✳
;; Maybe a    : *
;; Maybe      : * -> *
;; Either a   : * -> *
;; Either a b : *
;; Functor    : * -> *
;; Functor f  : * -> *

;; (defn Maybe [a] {:type 'Maybe :params [a]})
