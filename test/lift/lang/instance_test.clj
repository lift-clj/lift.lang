(ns lift.lang.instance-test
  (:require
   [clojure.test :refer [deftest is]]))

(deftest hash-lookup-test
  (is (= 5 (get {(lift.lang.Instance.
                  (lift.lang.type.base.Container.
                   (lift.lang.type.base.Const. 'Maybe)
                   [(lift.lang.type.base.Var. 'a)])
                  'Just [1 2]) 5}
                (lift.lang.Instance.
                 (lift.lang.type.base.Container.
                  (lift.lang.type.base.Const. 'Maybe)
                  [(lift.lang.type.base.Var. 'a)])
                 'Just [1 2])))))

(deftest equality-test
  (is (= (lift.lang.Instance.
          (lift.lang.type.base.Container.
           (lift.lang.type.base.Const. 'Maybe)
           [(lift.lang.type.base.Var. 'a)])
          'Just [1 2])
         (lift.lang.Instance.
          (lift.lang.type.base.Container.
           (lift.lang.type.base.Const. 'Maybe)
           [(lift.lang.type.base.Var. 'a)])
          'Just [1 2]))))
