(ns user
  (:refer-clojure :exclude [= not= read])
  (:require
   [clojure.core :as c]
   [clojure.tools.namespace.repl :refer [refresh]]
   [lift.lang.analyze :as ana]
   [lift.lang.inference :refer [infer]]
   [lift.lang.interface :refer [interface impl]]
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
  (=    [x y] (c/= x y))
  (not= [x y] (c/not= x y)))

(interface (Read a)
  (read (String -> a)))

(impl (Read Int)
  (read [s] (Integer. s)))

;; Eq

;; (->>
;;  (Lambda. (Symbol. 'a)
;;           (Lambda. (Symbol. 'b)
;;                    (Lambda. (Symbol. 'c)
;;                             (-> (Symbol. 'eq)
;;                                 (Apply. (-> (Symbol. '+)
;;                                             (Apply. (Symbol. 'b))
;;                                             (Apply. (Symbol. 'c))))
;;                                 (Apply. (Symbol. 'a))))))
;;  (infer _Gamma)
;;  (second)
;;  (:t)
;;  )
;; (lift.lang/require [lib.a.thing :as a])
;; @lift.lang.type/type-env
(defn check [expr]
  (let [[s ast] (->> expr
                     (impl/ana ana/parse)
                     (infer @t/type-env))]
    (t/substitute ast s)))

(check '(= 1 (read "1")))

;; (t/substitute (SyntaxNode. (Var. 'a) (Var. 'a))
;;               (t/sub {'a (Const. 'Int)}))

;; (->> '(fn [a b] (= a b))
;;      (impl/ana ana/parse)
;;      (infer @lift.lang.type/type-env)
;;      (second)
;;      (:t)
;;      )
