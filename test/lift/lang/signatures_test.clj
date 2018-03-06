(ns lift.lang.signatures-test
  (:require
   [lift.lang.signatures :as sig]
   [clojure.test :refer [deftest is]]
   [lift.lang.type.base :as base]))

(base/import-type-types)
(base/import-container-types)

(def ts sig/type-signature)

(defn ctr [tag & args]
  (Container. tag (vec args)))

(defn a-> [& args]
  (reduce #(Arrow. %2 %) (reverse args)))

(defn prcd1 [pred type]
  (Predicated. [pred] type))

(defn pred [tag & args]
  (Predicate. tag (vec args)))

(deftest simple-test
  (is (= (ts '(a)) (Var. 'a)))
  (is (= (ts '(& a)) (Vargs. 'a)))
  (is (= (ts '(Boolean)) (Const. 'Boolean))))

(deftest arrow-test
  (is (= (ts '((a -> b)))
         (a-> (Var. 'a) (Var. 'b))))
  (is (= (ts '((a -> b -> c)))
         (a-> (Var. 'a) (Var. 'b) (Var. 'c))))
  (is (= (ts '(((a -> b) -> c)))
         (a-> (a-> (Var. 'a) (Var. 'b)) (Var. 'c))))
  (is (= (ts '(((a -> b) -> c -> d)))
         (a-> (a-> (Var. 'a) (Var. 'b)) (Var. 'c) (Var. 'd))))
  (is (= (ts '(((a -> b) -> (c -> d) -> e)))
         (a-> (a-> (Var. 'a) (Var. 'b)) (a-> (Var. 'c) (Var. 'd)) (Var. 'e)))))

(deftest container-test
  (is (= (ts '(Maybe a)) (ctr (Const. 'Maybe) (Var. 'a))))
  (is (= (ts '(Either a b)) (ctr (Const. 'Either) (Var. 'a) (Var. 'b))))
  (is (= (ts '(m a)) (ctr (Var. 'm) (Var. 'a))))
  (is (= (ts '(m a b)) (ctr (Var. 'm) (Var. 'a) (Var. 'b)))))

(deftest predicated-test
  (is (= (ts '(Monad m => m a))
         (prcd1 (pred 'Monad (Var. 'm)) (ctr (Var. 'm) (Var. 'a)))))
  (is (= (ts '(Monad m n => m a))
         (prcd1 (pred 'Monad (Var. 'm) (Var. 'n)) (ctr (Var. 'm) (Var. 'a)))))
  (is (= (ts '(Corecursive f t => (a -> f a) -> a -> t))
         (prcd1 (pred 'Corecursive (Var. 'f) (Var. 't))
                (a-> (a-> (Var. 'a) (ctr (Var. 'f) (Var. 'a)))
                     (Var. 'a)
                     (Var. 't))))))

(deftest tuple-test
  (is (= (ts '([a b])) (Tuple. [(Var. 'a) (Var. 'b)]))))

(deftest set-test
  (is (= (ts '(#{a})) (ctr (Const. 'Set) (Var. 'a)))))
