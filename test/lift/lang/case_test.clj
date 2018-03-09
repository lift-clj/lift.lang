(ns lift.lang.case-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [lift.lang :refer [data Just]]
   [lift.lang.case :as case]
   [clojure.walk :as walk])
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

(use-fixtures :each gensym-fixture)

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
  (-> t resolve meta :prj :fs (get i)))

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
          (if (clojure.core/instance? ~lift.lang.__private.Just a1)
            (let* [a2 (~(prj 'Just 0) a1)]
              (let* [a a2] a))
            (lift.lang.type/unmatched-case-error a1)))
         (case/case* x '((Just a) a)))))

(data Pair a b = Pair a b)

(deftest product-type-destructure-test
  (is (= (wrap-let
          (if (clojure.core/instance? ~lift.lang.case_test.__private.Pair a1)
            (let* [a2 (~(prj 'Pair 0) a1)
                   a3 (~(prj 'Pair 1) a1)]
              (let* [a a2]
                (let* [b a3]
                  a)))
            (lift.lang.type/unmatched-case-error a1)))
         (case/case* x '((Pair a b) a)))))

(deftest tuple-destructure-test
  (is (= (case/case* x '([_ (Pair a b)] a))
         (wrap-let
          (if (clojure.core/instance? ~lift.lang.case.__private.Tuple2 a1)
            (let* [a2 (~(prj `case/Tuple2 0) a1)
                   a3 (~(prj `case/Tuple2 1) a1)]
              (let* [_ a2]
                (if (clojure.core/instance? ~lift.lang.case_test.__private.Pair a3)
                  (let* [a4 (~(prj 'Pair 0) a3)
                         a5 (~(prj 'Pair 1) a3)]
                    (let* [a a4]
                      (let* [b a5]
                        a)))
                  (lift.lang.type/unmatched-case-error a1))))
            (lift.lang.type/unmatched-case-error a1))))))
