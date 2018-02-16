(ns dev
  (:refer-clojure :exclude [case])
  (:require
   [clojure.core :as c]
   [clojure.pprint :refer [pprint]]
   [lift.lang :refer [case data Just Nothing Pair]]
   [lift.f.functor :as f]
   [lift.lang.rewrite :refer [emit rewrite]]
   [lift.lang.util :as u]
   [lift.lang.type :as t]
   [lift.lang.prim :as prim]
   [lift.lang.inference :as infer]
   [riddley.walk :as walk]))

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

(defmacro check [expr]
  `(infer/check (walk/macroexpand-all '~expr)))

(defmacro lift [expr]
  `(->> '~expr
        (walk/macroexpand-all)
        (infer/check)
        (rewrite @t/type-env)
        (emit)
        (eval)))

(lift
 (case (Pair Nothing (Pair (Just 1) 2))
   [Pair Nothing [Pair [Just a] b]] (Pair (Just a) 3)
   [Pair Nothing  x               ] x
   ))
