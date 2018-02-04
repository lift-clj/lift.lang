(ns lift.lang.util
  (:require [clojure.spec.alpha :as s]))

(defn ns-qualify
  "Qualify symbol s by resolving it or using the current *ns*."
  [s]
  (if-let [ns-sym (some-> s namespace symbol)]
    (or (some-> (get (ns-aliases *ns*) ns-sym) str (symbol (name s))) s)
    (symbol (str (.name *ns*)) (str s))))

(defn ->sym [v]
  (if (class? v)
    (throw (Exception. (format "Cannot turn class %s into symbol" v)))
    (symbol (name (.getName (:ns (meta v))))
            (name (:name (meta v))))))

(defn resolve-sym
  ([ns s]
   (or (some-> s resolve ->sym)
       (ns-qualify s)))
  ([s]
   (resolve-sym *ns* s)))

(defn unification-failure [a b]
  (throw
   (ex-info (format "Cannot unify %s and %s" (pr-str a) (pr-str b))
            {:type :unification-failure :a a :b b})))

(defn unbound-variable-error [x]
  (throw
   (ex-info (format "UnboundVariable %s" (pr-str x))
            {:type :unbound-variable :value x})))

(defn arity-error [a b]
  (throw
   (ex-info (format "Arities do not match %s %s" (pr-str a) (pr-str b))
            {:type :arity :a a :b b})))

(defn map-keys [f x]
  (into x (for [[k v] x] [(f k) v])))

(defn map-vals [f x]
  (into x (for [[k v] x] [k (f v)])))

(defn curry [op [a b & tail]]
  (if (seq tail)
    (op a (curry op (cons b tail)))
    (op a b)))

(defn assert-conform [spec x]
  (let [c (s/conform spec x)]
    (if (s/invalid? c)
      (throw
       (Exception. (format "Value %s does not conform to spec %s\n%s"
                           x spec (with-out-str (s/explain spec x)))))
      c)))
