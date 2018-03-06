(ns lift.lang.type.def-test
  (:require
   [lift.lang.type.def :as def]
   [clojure.test :refer [deftest is]]
   [lift.lang.type.base :as base]))

(base/import-type-types)
(base/import-container-types)

(def ts def/type-signature)

(defn ctr [tag & args]
  (Container. tag (vec args)))

(defn a-> [& args]
  (reduce #(Arrow. %2 %) (reverse args)))

(defn prcd1 [pred type]
  (Predicated. [pred] type))

(defn pred [tag & args]
  (Predicate. tag (vec args)))

(deftest simple-test
  (is (= (ts '()) (Unit.)))
  (is (= (ts 'a) (Var. 'a)))
  (is (= (ts '(a)) (Var. 'a)))
  (is (= (ts 'Boolean) (Const. 'Boolean)))
  (is (= (ts '(Boolean)) (Const. 'Boolean))))

(deftest arrow-test
  (is (= (ts '(a -> b))
         (a-> (Var. 'a) (Var. 'b))))
  (is (= (ts '(a -> b -> c))
         (a-> (Var. 'a) (Var. 'b) (Var. 'c))))
  (is (= (ts '((a -> b) -> c))
         (a-> (a-> (Var. 'a) (Var. 'b)) (Var. 'c))))
  (is (= (ts '((a -> b) -> c -> d))
         (a-> (a-> (Var. 'a) (Var. 'b)) (Var. 'c) (Var. 'd))))
  (is (= (ts '((a -> b) -> (c -> d) -> e))
         (a-> (a-> (Var. 'a) (Var. 'b)) (a-> (Var. 'c) (Var. 'd)) (Var. 'e)))))

(deftest container-test
  (is (= (ts '(Maybe a)) (ctr 'lift.lang/Maybe (Var. 'a))))
  (is (= (ts '(Either a b)) (ctr 'lift.lang/Either (Var. 'a) (Var. 'b)))))

(deftest set-test
  (is (= (ts '#{a}) (Set. #{(Var. 'a)}))))

(deftest record-test
  (is (= (ts '{}) (Record. (RowEmpty.))))
  (is (= (ts '{| r}) (Record. (Var. 'r))))
  (is (= (ts '{l a | r}) (Record. (Row. (Var. 'l) (Var. 'a) (Var. 'r)))))
  (is (= (ts '{:k a | r}) (Record. (Row. (Const. :k) (Var. 'a) (Var. 'r))))))
