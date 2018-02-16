(ns lift.lang.case
  (:refer-clojure :exclude [var?])
  (:require [lift.lang.type :as t]))

(defn var? [x]
  (and (simple-symbol? x) (re-matches #"^[^A-Z].*$" (name x))))

(defn gsym [& _]
  (gensym '_))

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
        l-fn (fn [l [gs m]]
               (fn [n]
                 (case-tree n gs m e l)))]
    (if n
      (if (= p (:test n))
        (let [prjs (map vector (:prjs n) ms)
              sjrp (reverse prjs)
              l'   (reduce l-fn (l-fn l (first sjrp)) (rest sjrp))]
          (update n :then l'))
        (update n :else dest x m e l))
      (let [prjs (map (juxt gsym identity) ms)
            sjrp (reverse prjs)
            l'   (reduce l-fn (l-fn l (first sjrp)) (rest sjrp))]
        {:type :dest
         :test p
         :thex x
         :prjs (mapv first prjs)
         :then (l' nil)}))))

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

(defn irrefutable? [{:keys [type then else expr]}]
  (cond (= type :bind)
        (if expr true (recur then))
        (= type :ltrl)
        (if expr true (recur else))))

(defn default-case [{:keys [type then else] :as n} default]
  (when n
    (if (irrefutable? else)
      (update n :then default-case else)
      (if then
        (let [n' (update n :then default-case default)]
          (if else
            (update n' :else default-case default)
            (if (and (not= :bind type) default)
              (assoc n' :else default)
              n')))
        (if  default
          (update n :else default-case default)
          n)))))

(def emit-case nil)
;; (defmulti emit-case (fn [x] (prn x) (:type x)))
(defmulti emit-case :type)

(defmethod emit-case :dest [{:keys [test thex prjs then else]}]
  (let [{:keys [isa? fs]} (-> test resolve meta :prj)]
    `(if (~@isa? ~thex)
       (let* [~@(mapcat (fn [gs p] [gs `(~p ~thex)]) prjs fs)]
         ~(emit-case then))
       ~(emit-case else))))

(defmethod emit-case :bind [{:keys [bind thex then expr]}]
  `(let* [~@(mapcat (juxt identity (constantly thex)) bind)]
     ~(if expr expr (emit-case then))))

(defmethod emit-case :ltrl [{:keys [ltrl thex then expr else]}]
  `(if (lift.lang.prim/eq ~ltrl ~thex)
     ~(if then (emit-case then) expr)
     ~(if else
        (emit-case else)
        `(t/unmatched-case-error ~thex))))

(defmethod emit-case :uerr [{:keys [expr]}]
  expr)

(defn case* [x pattern-exprs]
  (let [gs (gensym)]
    `(let [~gs ~x]
       ~(-> (->> (partition 2 pattern-exprs)
                 (reduce (fn [tree [pattern expr]]
                           (case-tree tree gs pattern expr nil))
                         nil))
            (default-case {:type :uerr :expr `(t/unmatched-case-error ~gs)})
            (emit-case)))))

;; TODO: need vector destructuring to n-tuple
;; (defn defn-match [name & param-exprs]
;;   `(defn ~name
;;      ~(case* )
;;      )
;;   )
