(ns dev
  (:refer-clojure :exclude [var?])
  (:require
   [clojure.core :as c]
   [clojure.pprint :refer [pprint]]
   [lift.lang :refer [data Just Pair]]
   [lift.f.functor :as f]
   [lift.lang.util :as u]
   [lift.lang.type :as t]))

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

(defn type? [x]
  (and (simple-symbol? x) (re-matches #"^[A-Z].*$" (name x))))

(defn var? [x]
  (and (simple-symbol? x) (re-matches #"^[^A-Z].*$" (name x))))

(let [n (atom 0)]
  (defn gsym [& _]
    (symbol (str "_" (swap! n inc)))))

(declare case-tree)

(defn bind [n x m e l]
  (if n
    (if l
      (-> (update n :bind conj m) (update :then l))
      (throw (Exception. "Trying to make final binding twice")))
    (if l
      {:type :bind :bind [m] :thex x :then (l nil)}
      {:type :bind :bind [m] :thex x :expr e})))

(defn dest [n x m e l]
  (let [[p & ms] m
        prjs (map (juxt gsym identity) ms)
        sjrp (reverse prjs)
        l-fn (fn [l [gs m]]
               (fn [n]
                 (case-tree n gs m e l)))
        l'   (reduce l-fn (l-fn l (first sjrp)) (rest sjrp))]
    (if n
      (if (= p (:test n))
        (update n :then l')
        (update n :else l' ))
      {:type :dest
       :test p
       :thex x
       :prjs (mapv first prjs)
       :then (l' nil)})))

(defn lit-eq [n x m e l]
  (if n
    (if (= m (:ltrl n))
      (update n :then l)
      (update n :else l))
    (if l
      {:type :ltrl :ltrl m :thex x :then (l nil)}
      {:type :ltrl :ltrl m :thex x :expr e})))

(defn case-tree [n x m e l]
  (let [t (:type n)]
    (cond (and (vector? m) (or (nil? n) (= t :dest)))
          (dest n x m e l)
          (vector? m)
          (throw (Exception. "Trying to match pattern after irrefutable bind"))
          (and (var? m) (or (nil? n) (= t :bind)))
          (bind n x m e l)
          (var? m)
          (update n :else case-tree x m e l)
          (or (nil? n) (= t :ltrl))
          (lit-eq n x m e l)
          :else
          (update n :else case-tree x m e l))))

(defn default-case [{:keys [type then else] :as n} default]
  (if (contains? #{:dest :ltrl} type)
    (if then
      (-> (update n :then default-case (or else default))
          (cond-> (and (nil? else) default)
            (assoc :else default)))
      (if default
        (update n :else default-case default)
        n))
    n))

(def case-tree-1
  (let [x 'Nothing]
    (-> nil
        (case-tree x '[Pair [Pair [Just a] [Just b]] c] '[a b c] nil)
        (case-tree x '[Pair [Pair       a  [Just b]] c] '[a b c] nil)
        (case-tree x '[Pair [Pair       a        b ] c] '[a b c] nil)
        (case-tree x '[Pair             a        b    ] '[a b  ] nil)
        (case-tree x 'Nothing                           '[     ] nil)
        (case-tree x '_                                 '[     ] nil)
        (default-case nil))))

(case x
  [Pair a b] [a b]
  )

{:type :dest,
 :test Pair,
 :thex Nothing,
 :prjs [_263 _264],
 :then {:type :dest,
        :test Pair,
        :thex _263,
        :prjs [_265 _266],
        :then {:type :dest,
               :test Just,
               :thex _265,
               :prjs [_267],
               :then {:type :bind,
                      :bind [a],
                      :thex _267,
                      :then {:type :dest,
                             :test Just,
                             :thex _266,
                             :prjs [_268],
                             :then {:type :bind,
                                    :bind [b],
                                    :thex _268,
                                    :then {:type :bind, :bind [c], :thex _264, :expr [a b c]}}}},
               :else {:type :bind,
                      :bind [a a],
                      :thex _271,
                      :then {:type :dest,
                             :test Just,
                             :thex _272,
                             :prjs [_273],
                             :then {:type :bind,
                                    :bind [b],
                                    :thex _273,
                                    :then {:type :bind, :bind [c], :thex _270, :expr [a b c]}},
                             :else {:type :bind,
                                    :bind [b],
                                    :thex _277,
                                    :then {:type :bind, :bind [c], :thex _275, :expr [a b c]}}}}},
        :else {:type :ltrl,
               :ltrl Nothing,
               :thex Nothing,
               :expr [],
               :else {:type :bind, :bind [_], :thex Nothing, :expr []}}},
 :else {:type :ltrl,
        :ltrl Nothing,
        :thex Nothing,
        :expr [],
        :else {:type :bind, :bind [_], :thex Nothing, :expr []}}}
