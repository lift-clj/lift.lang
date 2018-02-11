(ns user
  (:refer-clojure :exclude [= not= read])
  (:require
   [clojure.core :as c]
   [clojure.pprint :refer [pprint]]
   [clojure.tools.namespace.repl :refer [refresh]]
   [lift.lang :refer :all]
   [lift.lang.analyze :as ana]
   [lift.lang.inference :refer [check -infer infer]]
   [lift.lang.rewrite :refer [-emit emit -rewrite rewrite]]
   [lift.lang.signatures :as sig]
   [lift.lang.type :as t :refer [id]]
   [lift.lang.type.base :as base]
   [lift.lang.type.impl :as impl]
   [riddley.walk :as walk]))


;; (defn reset-type-env []
;;   (reset! t/type-env {}))

;; (defn print-type-env []
;;   (pprint @t/type-env))

(base/import-syntax-types)
(base/import-type-types)



(eval
 (emit
  (impl/cata
   (partial -rewrite @t/type-env)
   (check (walk/macroexpand-all '(case Nothing [Just x] x Nothing 0))))))

;; (-> #'Just meta :prj)
;; (t/new PhoneNumber = PhoneNumber String)

;; (eval
;;  (emit
;;   (impl/cata
;;    (partial -rewrite @t/type-env)
;;    (check '(= 2 (coerce "2"))))))

;; ;; ;; ;; ;; c/eval
;; ;; ;; ;; eval
;; ;; ;; eval
;; ;; ;; emit
;; eval
;; (emit
;;  (impl/cata
;;   (partial -rewrite @t/type-env)
;;   (check '(= 1 (read "1")))))

;; ;; eval
;; ;; emit
;; eval
;; emit
;; impl/cata
;; (partial -rewrite @t/type-env)
;; (check '(not= \2 (read "\\2")))
