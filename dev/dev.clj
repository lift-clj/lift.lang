(ns dev
  (:refer-clojure :exclude [case])
  (:require
   [clojure.core :as c]
   [clojure.pprint :refer [pprint]]
   [lift.lang :refer [case data Just Nothing Pair]]
   [lift.f.functor :as f]
   [lift.lang.util :as u]
   [lift.lang.type :as t]
   [lift.lang.prim :as prim]))

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

(defmacro pattern-1 [x]
  `(case ~x
     [Pair [Pair [Just ~'a] [Just ~'b]] ~'c] [~'a ~'b ~'c]
     [Pair [Pair       ~'d  [Just ~'e]] ~'f] [~'d ~'e ~'f]
     [Pair [Pair [Just ~'g]       ~'h ] ~'i] [~'g ~'h ~'i]
     [Pair             ~'j              ~'k] [~'j ~'k    ]
     Nothing                                 [0          ]
     ~'_                                     [           ]))

(= (pattern-1 (Pair (Pair (Just 'a) (Just 'b)) 'c)) '[a b c])
(= (pattern-1 (Pair (Pair       'd  (Just 'e)) 'f)) '[d e f])
(= (pattern-1 (Pair (Pair (Just 'g)       'h)  'i)) '[g h i])
(= (pattern-1 (Pair             'j             'k)) '[j k  ])
(= (pattern-1 Nothing                             ) '[0    ])
(= (pattern-1 '_                                  ) '[     ])

(case (Pair (Pair (Just 1) 2) 3)
  [Pair [Pair [Just a] [Just b]] c] [a b c]
  [Pair [Pair       d  [Just e]] f] [d e f]
  [Pair [Pair [Just g]       h ] i] [g h i]
  [Pair             j            k] [j k  ]
  Nothing                           [0    ]
  _                                 [     ])

;; (def case-tree-1
;;   (let [x 'Nothing]
;;     (-> nil
;;         (case-tree x '[Pair [Pair [Just a] [Just b]] c] '[a b c] nil)
;;         (case-tree x '[Pair [Pair       d  [Just e]] f] '[d e f] nil)
;;         (case-tree x '[Pair [Pair [Just g]       h ] i] '[g h i] nil)
;;         (case-tree x '[Pair             j            k] '[j k  ] nil)
;;         (case-tree x 'Nothing                           '[     ] nil)
;;         (case-tree x '_                                 '[     ] nil)
;;         (default-case nil))))

;; (def case-source (emit-case case-tree-1))

;; (if (clojure.core/instance? lift.lang.__private.Pair Nothing)
;;   (let* [_29397 (Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a)))
;;          _29398 (Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b)))]
;;     (if (clojure.core/instance? lift.lang.__private.Pair _29397)
;;       (let* [_29399 (Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a)))
;;              _29400 (Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b)))]
;;         (if (clojure.core/instance? lift.lang.__private.Just _29399)
;;           (let* [_29401 (Prim (clojure.core/fn -prj-Just [x] (clojure.core/nth x 0)) ((Maybe (Var a)) -> (Var a)))]
;;             (let* [a _29401 g _29401]
;;               (if (clojure.core/instance? lift.lang.__private.Just _29400)
;;                 (let* [_29402 (Prim (clojure.core/fn -prj-Just [x] (clojure.core/nth x 0)) ((Maybe (Var a)) -> (Var a)))]
;;                   (let* [b _29402]
;;                     (let* [c _29398] [a b c])))
;;                 (let* [h _29400]
;;                   (let* [i _29398] [g h i])))))
;;           (let* [d _29399]
;;             (if (clojure.core/instance? lift.lang.__private.Just _29400)
;;               (let* [_29403 (Prim (clojure.core/fn -prj-Just [x] (clojure.core/nth x 0)) ((Maybe (Var a)) -> (Var a)))]
;;                 (let* [e _29403]
;;                   (let* [f _29398] [d e f])))
;;               (let* [j _29397]
;;                 (let* [k _29398] [j k]))))))
;;       (let* [j _29397]
;;         (let* [k _29398] [j k]))))
;;   (if (lift.lang.prim/eq Nothing Nothing) [] (let* [_ Nothing] [])))

;; {:type :dest,
;;  :test Pair,
;;  :thex Nothing,
;;  :prjs [_29397 _29398],
;;  :then {:type :dest,
;;         :test Pair,
;;         :thex _29397,
;;         :prjs [_29399 _29400],
;;         :then {:type :dest,
;;                :test Just,
;;                :thex _29399,
;;                :prjs [_29401],
;;                :then {:type :bind,
;;                       :bind [a g],
;;                       :thex _29401,
;;                       :then {:type :dest,
;;                              :test Just,
;;                              :thex _29400,
;;                              :prjs [_29402],
;;                              :then {:type :bind,
;;                                     :bind [b],
;;                                     :thex _29402,
;;                                     :then {:type :bind,
;;                                            :bind [c],
;;                                            :thex _29398,
;;                                            :expr [a b c],
;;                                            :else nil}},
;;                              :else {:type :bind,
;;                                     :bind [h],
;;                                     :thex _29400,
;;                                     :then {:type :bind
;;                                            :bind [i]
;;                                            :thex _29398
;;                                            :expr [g h i]}}}}
;;                :else {:type :bind,
;;                       :bind [d],
;;                       :thex _29399,
;;                       :then {:type :dest,
;;                              :test Just,
;;                              :thex _29400,
;;                              :prjs [_29403],
;;                              :then {:type :bind,
;;                                     :bind [e],
;;                                     :thex _29403,
;;                                     :then {:type :bind,
;;                                            :bind [f],
;;                                            :thex _29398,
;;                                            :expr [d e f],
;;                                            :else nil}},
;;                              :else {:type :bind,
;;                                     :bind [j],
;;                                     :thex _29397,
;;                                     :then {:type :bind
;;                                            :bind [k]
;;                                            :thex _29398
;;                                            :expr [j k]}}}}}
;;         :else {:type :bind,
;;                :bind [j],
;;                :thex _29397,
;;                :then {:type :bind, :bind [k], :thex _29398, :expr [j k]}}},
;;  :else {:type :ltrl,
;;         :ltrl Nothing,
;;         :thex Nothing,
;;         :expr [],
;;         :else {:type :bind, :bind [_], :thex Nothing, :expr []}}}
