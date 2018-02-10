(ns user
  (:refer-clojure :exclude [= not= read])
  (:require
   [clojure.core :as c]
   [clojure.tools.namespace.repl :refer [refresh]]
   [lift.lang.analyze :as ana]
   [lift.lang.inference :refer [check -infer infer]]
   [lift.lang.interface :refer [interface impl]]
   [prelude :refer :all]
   [primitives :as prim]
   [lift.lang.rewrite :refer [-emit emit -rewrite rewrite]]
   [lift.lang.signatures :as sig]
   [lift.lang.type :as t :refer [id import-syntax-types import-type-types]]
   [lift.lang.type.impl :as impl]
   ))


(import-syntax-types)
(import-type-types)

(interface (Eq a)
  (=    (a -> a -> Boolean))
  (not= (a -> a -> Boolean))
  (default
   (=    [x y] (c/= x y))
   (not= [x y] (c/not= x y))))

(impl (Eq Int)
  (=    [x y] (prim/=-Long->Long x y))
  (not= [x y] (not (= x y))))

(interface (Read a)
  (read (String -> a)))

(impl (Read Int)
  (read [s] (prim/str->Long s)))


;; ;; ;; ;; c/eval
;; ;; ;; eval
;; ;; eval
;; ;; emit
eval
(emit
 (impl/cata
  (partial -rewrite @t/type-env)
  (check '(= 1 (read "1")))))

(eval
 (emit
  (impl/cata
   (partial -rewrite @t/type-env)
   (check '((fn [x] (= x (read "2"))) 2)))))
