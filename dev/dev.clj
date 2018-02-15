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

(defn ing-bound? [tree]
  (or (some var? (keys (:bind tree))) (:dflt tree)))

(defn ing-binding [{:keys [bind dflt]}]
  (or dflt (->> bind (filter (comp var? key)) (into {}))))

(defn fully-bound? [{:keys [type] :as x}]
  (c/case type
    :bind (every? fully-bound? (:ings x))
    :ing  (and (ing-bound? x)
               (every? (comp fully-bound? val) (:dest x)))))

(let [n (atom 0)]
  (defn gsym [& _]
    (symbol (str "_" (swap! n inc)))))

(declare case-tree)

(defn bind [n m e l]
  (if n
    (if l
      (-> (update n :bind conj m) (update :then l))
      (throw (Exception. "Trying to make final binding twice")))
    (if l
      {:type :bind :bind [m] :then (l nil)}
      {:type :bind :bind [m] :expr e})))

(defn dest [n m e l]
  (let [[p & ms] m
        prjs (map (juxt gsym identity) ms)
        sjrp (reverse prjs)
        l-fn (fn [l [gs m]]
               (fn [n]
                 (case-tree n m e l)))
        l'   (reduce l-fn (l-fn l (first sjrp)) (rest sjrp))]
    (if n
      (if (= p (:test n))
        (update n :then l')
        (update n :else l' ))
      {:type :dest
       :test p
       :prjs (mapv first prjs)
       :then (l' nil)})))

(defn lit-eq [n m e l]
  (if n
    (if (= m (:ltrl n))
      (update n :then l)
      (update n :else l))
    (if l
      {:type :ltrl :ltrl m :then (l nil)}
      {:type :ltrl :ltrl m :expr e})))

(defn case-tree [n m e l]
  (let [t (:type n)]
    (cond (and (vector? m) (or (nil? n) (= t :dest)))
          (dest n m e l)
          (vector? m)
          (throw (Exception. "Trying to match pattern after irrefutable bind"))
          (and (var? m) (or (nil? n) (= t :bind)))
          (bind n m e l)
          (var? m)
          (update n :else case-tree m e l)
          (or (nil? n) (= t :ltrl))
          (lit-eq n m e l)
          :else
          (update n :else case-tree m e l))))

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

(-> nil
    (case-tree '[Pair [Pair [Just a] [Just b]] c] '[a b c] nil)
    (case-tree '[Pair [Pair       a  [Just b]] c] '[a b c] nil)
    (case-tree '[Pair [Pair       a        b ] c] '[a b c] nil)
    ;; (case-tree '[Pair             a        b    ] '[a b  ] nil)
    (case-tree 'Nothing                           '[     ] nil)
    (case-tree '_                                 '[     ] nil)
    (default-case nil))

{:type :dest,
 :test Pair,
 :prjs [_218 _219],
 :then {:type :dest,
        :test Pair,
        :prjs [_220 _221],
        :then {:type :dest,
               :test Just,
               :prjs [_222],
               :then {:type :bind,
                      :bind [a],
                      :then {:type :dest,
                             :test Just,
                             :prjs [_223],
                             :then {:type :bind,
                                    :bind [b],
                                    :then {:type :bind, :bind [c], :expr [a b c]}}}},
               :else {:type :bind,
                      :bind [a a],
                      :then {:type :dest,
                             :test Just,
                             :prjs [_228],
                             :then {:type :bind,
                                    :bind [b],
                                    :then {:type :bind, :bind [c], :expr [a b c]}},
                             :else {:type :bind,
                                    :bind [b],
                                    :then {:type :bind, :bind [c], :expr [a b c]}}}}},
        :else {:type :ltrl,
               :ltrl Nothing,
               :expr [],
               :else {:type :bind, :bind [_], :expr []}}},
 :else {:type :ltrl,
        :ltrl Nothing,
        :expr [],
        :else {:type :bind, :bind [_], :expr []}}}
