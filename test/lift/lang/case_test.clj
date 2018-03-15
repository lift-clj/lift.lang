(ns lift.lang.case-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [lift.lang :refer [data Just Nothing Left Right True False]]
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

  (is (false?
       (case/pcase [(Just 1) (Just 2)]
         [(Just x) (Just y)] (= x y)
         [Nothing   Nothing] true
         [_         _      ] false)))

  (is (true?
       (case/pcase [Nothing Nothing]
         [(Just x) (Just y)] (= x y)
         [Nothing   Nothing] true
         [_         _      ] false)))

  (is (true?
       (case/pcase [(Just 1) (Just 1)]
         [(Just x) (Just y)] (= x y)
         [Nothing   Nothing] true
         [_         _      ] false)))

  (is (false?
       (case/pcase [(Just 1) Nothing]
         [(Just x) (Just y)] (= x y)
         [Nothing   Nothing] true
         [_         _      ] false)))

  (is (false?
       (case/pcase [Nothing (Just 1)]
         [(Just x) (Just y)] (= x y)
         [Nothing   Nothing] true
         [_         _      ] false))))

(deftest Either-equals-test

  (is (true?
       (case/pcase [(Left 1) (Left 1)]
         [(Left  x) (Left  y)] (= x y)
         [(Right x) (Right y)] (= x y)
         [_         _      ] false)))

  (is (false?
       (case/pcase [(Left 1) (Left 2)]
         [(Left  x) (Left  y)] (= x y)
         [(Right x) (Right y)] (= x y)
         [_         _      ] false)))

  (is (true?
       (case/pcase [(Right 1) (Right 1)]
         [(Left  x) (Left  y)] (= x y)
         [(Right x) (Right y)] (= x y)
         [_         _      ] false)))

  (is (false?
       (case/pcase [(Right 1) (Right 2)]
         [(Left  x) (Left  y)] (= x y)
         [(Right x) (Right y)] (= x y)
         [_         _      ] false)))

  (is (false?
       (case/pcase [(Left 1) (Right 1)]
         [(Left  x) (Left  y)] (= x y)
         [(Right x) (Right y)] (= x y)
         [_         _      ] false)))

  (is (false?
       (case/pcase [(Right 1) (Left 1)]
         [(Left  x) (Left  y)] (= x y)
         [(Right x) (Right y)] (= x y)
         [_         _      ] false))))
