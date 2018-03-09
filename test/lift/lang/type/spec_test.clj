(ns lift.lang.type.spec-test
  (:require
   [clojure.test :refer [deftest is]]
   [lift.lang.type.base :as base]
   [lift.lang.type.spec :as spec]))

(base/import-type-types)
(base/import-container-types)

(def ts spec/parse)

(defn ctr [tag & args]
  (Container. tag (vec args)))

(defn a-> [& args]
  (reduce #(Arrow. %2 %) (reverse args)))

(defn prcd1 [pred type]
  (Predicated. [pred] type))

(defn prcd2 [pred1 pred2 type]
  (Predicated. [pred1 pred2] type))

(defn pred [tag & args]
  (Predicate. tag (vec args)))

(deftest simple-test
  (is (= (ts '()) (Unit.)))
  (is (= (ts 'a) (Var. 'a)))
  (is (= (ts 'Boolean) (Const. 'Boolean))))

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
                     (Var. 't)))))
  (is (= (ts '([(Corecursive f t) (Base a f)] => (a -> f a) -> a -> t))
         (prcd2 (pred 'Corecursive (Var. 'f) (Var. 't))
                (pred 'Base (Var. 'a) (Var. 'f))
                (a-> (a-> (Var. 'a) (ctr (Var. 'f) (Var. 'a)))
                     (Var. 'a)
                     (Var. 't))))))

(deftest list-test
  (is (= (ts '(Boolean)) (ctr (Const. 'List) (Const. 'Boolean))))
  (is (= (ts '(a -> b)) (a-> (Var. 'a) (Var. 'b))))
  (is (= (ts '((a -> b))) (ctr (Const. 'List) (a-> (Var. 'a) (Var. 'b)))))
  (is (= (ts '((a -> b) -> c)) (a-> (a-> (Var. 'a) (Var. 'b)) (Var. 'c))))
  (is (= (ts '(a -> (b -> c))) (a-> (Var. 'a) (Var. 'b) (Var. 'c))))
  (is (= (ts '(a -> ((b -> c))))
         (a-> (Var. 'a) (ctr (Const. 'List) (a-> (Var. 'b) (Var. 'c))))))
  (is (= (ts '(((a -> b) -> c)))
         (ctr (Const. 'List) (a-> (a-> (Var. 'a) (Var. 'b)) (Var. 'c)))))
  (is (= (ts '(a)) (ctr (Const. 'List) (Var. 'a)))))

(deftest vector-test
  (is (= (ts '[Boolean]) (ctr (Const. 'Vector) (Const. 'Boolean))))
  (is (= (ts '[a]) (ctr (Const. 'Vector) (Var. 'a)))))

(deftest tuple-test
  (is (= (ts '[Long String])
         (Container. (Const. 'Tuple) [(Const. 'Long) (Const. 'String)])))
  (is (= (ts '[Long b])
         (Container. (Const. 'Tuple) [(Const. 'Long) (Var. 'b)])))
  (is (= (ts '[a b])
         (Container. (Const. 'Tuple) [(Var. 'a) (Var. 'b)]))))

(deftest set-test
  (is (= (ts '#{Long}) (ctr (Const. 'Set) (Const. 'Long))))
  (is (= (ts '#{a}) (ctr (Const. 'Set) (Var. 'a)))))

(deftest record-test
  (is (= (ts '{}) (Record. (RowEmpty.))))
  (is (= (ts '{| r}) (Record. (Var. 'r))))
  (is (= (ts '{l a | r}) (Record. (Row. (Var. 'l) (Var. 'a) (Var. 'r)))))
  (is (= (ts '{:k a | r}) (Record. (Row. (Const. :k) (Var. 'a) (Var. 'r))))))
