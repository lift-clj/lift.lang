(ns lift.lang.case
  (:refer-clojure :exclude [var?])
  (:require
   [lift.lang.type.data :refer [data]]
   [lift.lang.type :as t]
   [clojure.spec.alpha :as s]))

(data Tuple1 a = Tuple1 a)
(data Tuple2 a b = Tuple2 a b)
(data Tuple3 a b c = Tuple3 a b c)
(data Tuple4 a b c d = Tuple4 a b c d)
(data Tuple5 a b c d e = Tuple5 a b c d e)
(data Tuple6 a b c d e f = Tuple6 a b c d e f)
(data Tuple7 a b c d e f g = Tuple7 a b c d e f g)
(data Tuple8 a b c d e f g h = Tuple8 a b c d e f g h)
(data Tuple9 a b c d e f g h i = Tuple9 a b c d e f g h i)

(def tpl `[Tuple1 Tuple2 Tuple3 Tuple4 Tuple5 Tuple6 Tuple7 Tuple8 Tuple9])

(defn tuple
  ([vars] (tuple (count vars) vars))
  ([n vars] `(~(nth tpl (dec n)) ~@vars)))

(s/def ::ctor (s/and symbol? #(re-matches #"^[A-Z].*$" (name %))))
(s/def ::var  (s/and simple-symbol? #(re-matches #"^[^A-Z].*$" (name %))))
(s/def ::tupl (s/coll-of any? :kind vector?))
(s/def ::dtor (s/and seq? (s/cat :ctor ::ctor :args (s/+ any?))))

(defn var?  [x] (s/valid? ::var x))
(defn tupl? [x] (s/valid? ::tupl x))
(defn dtor? [x] (s/valid? ::dtor x))

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
  (prn m)
  (let [t (:type n)]
    (cond (tupl? m)
          (case-tree n x (tuple m) e l)
          (and (dtor? m) (or (nil? n) (= t :dest)))
          (dest n x m e l)
          (dtor? m)
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
  (let [gs (gensym)
        x' (if (tupl? x) (tuple x) x)]
    `(let [~gs ~x']
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
