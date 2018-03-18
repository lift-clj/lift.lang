(ns lift.lang.case
  (:refer-clojure :exclude [var?])
  (:require
   [clojure.spec.alpha :as s]
   [lift.lang.type.data :refer [data get-prj tagged?]]
   [lift.lang.util :as u]))

(def tpl
  '[lift.lang.type/Tuple1
    lift.lang.type/Tuple2
    lift.lang.type/Tuple3
    lift.lang.type/Tuple4
    lift.lang.type/Tuple5
    lift.lang.type/Tuple6
    lift.lang.type/Tuple7
    lift.lang.type/Tuple8
    lift.lang.type/Tuple9])

(defn tuple
  ([vars] (tuple (count vars) vars))
  ([n vars] `(~(nth tpl (dec n)) ~@vars)))

(defn tuple-prjs [n]
  (-> tpl (nth (dec n)) resolve meta :prjs))

(s/def ::ctor (s/and symbol? #(re-matches #"^[A-Z].*$" (name %))))
(s/def ::var  (s/and simple-symbol? #(re-matches #"^[^A-Z].*$" (name %))))
(s/def ::tupl (s/coll-of any? :kind vector?))
(s/def ::dtor (s/and seq? (s/cat :ctor ::ctor :args (s/+ any?))))

(defn var?  [x] (s/valid? ::var x))
(defn tupl? [x] (s/valid? ::tupl x))
(defn dtor? [x] (s/valid? ::dtor x))

(defn gsym [& _]
  (gensym '_))

(defn unmatched-case-error [x]
  (throw (ex-info "Unmatched Case" {:type :unmatched-case-error :x x})))

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
  (or (when (= type :bind)
        (if (some? expr)
          true
          (irrefutable? then)))
      (if else
        (recur else))))

(defn default-case [{:keys [type then else] :as n} default]
  (if n
    (if (irrefutable? else)
      (if then
        (let [else (default-case else default)]
          (-> n
              (update :then default-case else)
              (assoc :else else)))
        n)
      (if then
        (let [n' (update n :then default-case default)]
          (if else
            (update n' :else default-case default)
            (if (and (not= :bind type) default)
              (assoc n' :else default)
              n')))
        (if default
          (update n :else default-case default)
          n)))
    default))

(defmulti emit-case :type)

(defmethod emit-case :dest [{:keys [test thex prjs then else]}]
  (if-let [tag (some-> test resolve u/->sym)]
    `(if (tagged? ~thex '~tag)
       (let* [~@(->> prjs
                     (map-indexed (fn [i gs]
                                    (let [p (get-prj tag i)] [gs `(~p ~thex)])))
                     (apply concat))]
         ~(emit-case then))
       ~(emit-case else)))
  (throw (ex-info "Could not resolve constructor for destructure"
                  {:type ::unresolved-type-destructure
                   :ctor test})))

(defmethod emit-case :bind [{:keys [bind thex then expr]}]
  `(let* [~@(mapcat (juxt identity (constantly thex)) bind)]
     ~(if (some? expr) expr (emit-case then))))

(defmethod emit-case :ltrl [{:keys [ltrl thex then expr else]}]
  `(if (lift.lang.prim/eq ~ltrl ~thex)
     ~(if then (emit-case then) expr)
     ~(if else
        (emit-case else)
        `(unmatched-case-error ~thex))))

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
            (default-case {:type :uerr :expr `(unmatched-case-error ~gs)})
            (emit-case)))))

(defmacro pcase [x & pattern-exprs]
 (lift.lang.case/case* x pattern-exprs))
