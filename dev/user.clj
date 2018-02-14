(ns user
  (:refer-clojure :exclude [case = not= read])
  (:require
   ;; [clojure.core :as c]
   ;; [clojure.pprint :refer [pprint]]
   ;; [clojure.tools.namespace.repl :refer [refresh]]
   [lift.lang :refer :all]
   [lift.lang.analyze :as ana]
   [lift.lang.inference :refer [check -infer infer]]
   [lift.lang.rewrite :refer [-emit emit -rewrite rewrite]]
   [lift.lang.signatures :as sig]
   [lift.lang.type :as t :refer [id]]
   [lift.lang.type.base :as base]
   [lift.lang.type.impl :as impl]
   [riddley.walk :as walk]
   [lift.lang.type :as type]
   [lift.f.functor :as f]))

;; (base/import-syntax-types)
;; (base/import-type-types)

;; (check '(fn [x y] x))

;; (impl/ana ana/parse '[])

;; (data P a b = P a b)

;; (case (P 1 (P (Just 1) (Just 2)))
;;   [P a [P [Just x] [Just b]]] [a b x]
;;   [P a b] [a b]
;;   [S a] a
;;   Nothing 0)

;; (defn tree-merge [a b]
;;   ;; (prn 'a a)
;;   ;; (prn 'b b)
;;   (cond (and (c/= :bind (:type a)) (c/= :bind (:type b)))
;;         {:type :bind
;;          :ings (mapv #(merge-with tree-merge % %2)
;;                      (:ings a)
;;                      (:ings b))}
;;         (and (map? a) (map? b))
;;         (merge-with tree-merge a b)
;;         :else
;;         a))

;; (let [matches '[[[P [Just a] [P [Just x] [Just b]]] [a b x]]
;;                 [[P [Just a] [P [Just x] Nothing]] [a x]]
;;                 [[P [Just a] [P x Nothing]] [a x]]
;;                 [[P [Just a] b] [a b]]
;;                 [[Just f] f]
;;                 [Nothing 0]
;;                 [_ 1]]]
;;   (letfn [(f [[pattern expr]]
;;             (if (vector? pattern)
;;               {:dest {(first pattern)
;;                       {:type :bind
;;                        :ings (mapv (fn [p] (f [p expr])) (rest pattern))}}}
;;               {:bind {pattern expr}}))]
;;     (->> matches
;;          (map f)
;;          (reduce #(merge-with tree-merge % %2)))))

;; (defn push-default-binding [default {:keys [type] :as tree}]
;;   (c/case type
;;     :bind (update :ings (partial mapv (partial push-default-binding default)))
;;     :ing  (if (ing-bound? tree))
;;     here
;;     )

;;   )

;; (defn var? [x]
;;   (and (simple-symbol? x) (re-matches #"^[a-z].*$" (name x))))

;; (defn fully-bound? [{:keys [type] :as x}]
;;   (c/case type
;;     :bind (every? fully-bound? (:ings x))
;;     :ing  (and (or (some var? (keys (:bind x)))
;;                    (:dflt x))
;;                (every? (comp fully-bound? val) (:dest x)))))

;; (fully-bound?
;;  '{:type :ing
;;    :dest
;;    {P
;;     {:type :bind,
;;      :ings
;;      [{:type :ing
;;        :dest {Just {:type :bind, :ings [{:type :ing :bind {a [a b x]}}]}}
;;        :dflt {_ 1}}
;;       {:type :ing
;;        :dest
;;        {P
;;         {:type :bind,
;;          :ings
;;          [{:type :ing
;;            :dest {Just {:type :bind, :ings [{:type :ing
;;                                              :bind {x [a b x]}}]}},
;;            :bind {x [a x]}}
;;           {:type :ing
;;            :dest {Just {:type :bind, :ings [{:type :ing
;;                                              :bind {b [a b x]}}]}},
;;            :bind {Nothing [a x]}

;;            }]}},
;;        :bind {b [a b]}}]},
;;     Just {:type :bind, :ings [{:type :ing :bind {f f}}]}},
;;    :bind {Nothing 0}
;;    :dflt {_ 1}})




(comment
  (c/= 1
       (eval
        (emit
         (impl/cata
          (partial -rewrite @t/type-env)
          (check (walk/macroexpand-all '(case (Just 1) [Just x] x Nothing 0)))))))

  )

(comment

  (c/= "a"
      (eval
       (emit
        (impl/cata
         (partial -rewrite @t/type-env)
         (check (walk/macroexpand-all '(case (Right "a")
                                         [Left a] a
                                         [Right b] b)))))))

  )

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
