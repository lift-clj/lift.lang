(ns lift.lang.case-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [lift.lang :refer [data Just Nothing True False]]
   [lift.lang.case :as case]
   [clojure.walk :as walk]
   [lift.lang.util :as u])
  (:import
   [clojure.lang ExceptionInfo]))

(def x (reify Object))

(defmacro case*
  {:style/indent :defn}
  [x & pattern-exprs]
  (case/case* x pattern-exprs))

(defn gensym-seed [seed]
  (let [x (atom seed)]
    (fn [& _] (symbol (str "a" (swap! x inc))))))

(defn gensym-fixture [f]
  (with-redefs [gensym (gensym-seed 0)]
    (f)))

(defn ensure-*ns*-fixture [f]
  (let [ns *ns*]
    (in-ns 'lift.lang.case-test)
    (f)
    (in-ns (.getName ns))))

(use-fixtures :each ensure-*ns*-fixture gensym-fixture)

(defn quo [expr]
  (walk/postwalk (fn [x]
                   (cond (= `unquote x) x
                         (and (seq? x) (= `unquote (first x))) (eval (second x))
                         (symbol? x) (list 'quote x)
                         (seq? x) (cons 'list x)
                         :else x))
                 expr))

(defmacro wrap-let [expr]
  ``(let [~'~'a1 ~x] ~~(quo expr)))

(defn prj [t i]
  (-> t u/resolve-sym (lift.lang.Instance/getProjection i)))

(deftest identity-test
  (is (= (wrap-let (let* [a a1] a))
         (case/case* x '(a a)))))

(deftest unresolved-type-destructure-test
  (let [err? (try
               (case/case* x '((RandomThing a) a))
               (catch ExceptionInfo e e))]
    (is (-> err? ex-data :type (= ::case/unresolved-type-destructure)))))

(deftest sum-type-destructure-test
  (is (= (wrap-let
          (if (lift.lang.case/tagged? a1 '~`Just)
            (let* [a2 (~(prj 'Just 0) a1)]
              (let* [a a2] a))
            (lift.lang.type/unmatched-case-error a1)))
         (case/case* x '((Just a) a))))
  (is (= 13
         (eval (case/case* (Just 13) '((Just a) a))))))

(data Pair a b = Pair a b)

(deftest product-type-destructure-test
  (is (= (wrap-let
          (if (lift.lang.case/tagged? a1 '~`Pair)
            (let* [a2 (~(prj 'Pair 0) a1)
                   a3 (~(prj 'Pair 1) a1)]
              (let* [a a2]
                (let* [b a3]
                  a)))
            (lift.lang.type/unmatched-case-error a1)))
         (case/case* x '((Pair a b) a))))
  (is (= [14 15]
         (eval (case/case* (Pair 14 15) '((Pair a b) [a b]))))))

(deftest tuple-destructure-test
  (is (= (wrap-let
          (if (lift.lang.case/tagged? a1 '~`case/Tuple2)
            (let* [a2 (~(prj `case/Tuple2 0) a1)
                   a3 (~(prj `case/Tuple2 1) a1)]
              (let* [_ a2]
                (if (lift.lang.case/tagged? a3 '~`Pair)
                  (let* [a4 (~(prj 'Pair 0) a3)
                         a5 (~(prj 'Pair 1) a3)]
                    (let* [a a4]
                      (let* [b a5]
                        a)))
                  (lift.lang.type/unmatched-case-error a1))))
            (lift.lang.type/unmatched-case-error a1)))
         (case/case* x '([_ (Pair a b)] a)))))

(deftest Maybe-equals-test
  (is (= (eval
          (quo
           '(clojure.core/let [a1 (lift.lang.case/Tuple2 (Just 1) (Just 2))]
              (if (lift.lang.case/tagged? a1 'lift.lang.case/Tuple2)
                (let* [a2 (~(prj `case/Tuple2 0) a1)
                       a3 (~(prj `case/Tuple2 1) a1)]
                  (if (lift.lang.case/tagged? a2 'lift.lang/Just)
                    (let* [a4 (~(prj `Just 0) a2)]
                      (let* [x a4]
                        (if (lift.lang.case/tagged? a3 'lift.lang/Just)
                          (let* [a5 (~(prj `Just 0) a3)]
                            (let* [y a5] (= x y)))
                          (if (lift.lang.prim/eq Nothing a2)
                            (if (lift.lang.prim/eq Nothing a3)
                              True
                              (lift.lang.type/unmatched-case-error a3))
                            (let* [_ a2] (let* [_ a3] False))))))
                    (if (lift.lang.prim/eq Nothing a2)
                      (if (lift.lang.prim/eq Nothing a3)
                        True
                        (lift.lang.type/unmatched-case-error a3))
                      (let* [_ a2] (let* [_ a3] False)))))
                (lift.lang.type/unmatched-case-error a1)))))
         (case/case*
          '[(Just 1) (Just 2)]
          '([(Just x) (Just y)] (= x y)
            [Nothing   Nothing] True
            [_         _      ] False))))
  (is (= false
         (eval (case/case*
                '[(Just 1) (Just 2)]
                '([(Just x) (Just y)] (= x y)
                  [Nothing   Nothing] True
                  [_         _      ] False)))))
  (is (= true
         (eval (case/case*
                '[Nothing Nothing]
                '([(Just x) (Just y)] (= x y)
                  [Nothing   Nothing] true
                  [_         _      ] false)))))
  (is (= true
         (case/pcase [(Just 1) (Just 1)]
           [(Just x) (Just y)] (= x y)
           [Nothing   Nothing] True
           [_         _      ] False)))

  (is (false? (case/pcase [(Just 1) Nothing]
                [(Just x) (Just y)] (= x y)
                [Nothing   Nothing] true
                [_         _      ] false)))

  (is (false? (case/pcase [Nothing (Just 1)]
                [(Just x) (Just y)] (= x y)
                [Nothing   Nothing] true
                [_         _      ] false)))

  (is (true? (case/pcase [Nothing Nothing]
               [(Just x) (Just y)] (= x y)
               [Nothing   Nothing] true
               [_         _      ] false)))
  )
