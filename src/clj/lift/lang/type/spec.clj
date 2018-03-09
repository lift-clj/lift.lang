(ns lift.lang.type.spec
  (:require
   [clojure.spec.alpha :as s]
   [lift.lang.type.base :as base]
   [lift.lang.util :as u]))

(base/import-syntax-types)
(base/import-type-types)
(base/import-container-types)

(s/def ::unit #{()})

;; a, b, c, etc.
(s/def ::var
  (s/and simple-symbol? #(re-matches #"^[a-z][a-z0-9]*$" (name %))))

;; String, Functor, etc.
(s/def ::name
  (s/and simple-symbol? #(re-matches #"[A-Z][a-zA-Z0-9]*" (name %))))

;; Functor f, (Functor f), Either (Maybe a) b, m a b
(s/def ::app
  (s/cat :op (s/or :var ::var :name ::name) :args (s/+ ::type)))

;; a -> (a -> b) -> c, f -> f a
(s/def ::arrow
  (s/cat :a ::type :-> #{'->} :b (s/alt :p ::type :n ::type-re)))

(s/def ::list
  (s/and seq? (s/cat :a ::type)))

(s/def ::vector
  (s/and vector? (s/cat :a ::type)))

(s/def ::tuple
  (s/and vector? (s/+ ::type)))

(s/def ::set
  (s/and set? (s/cat :a ::type)))

(s/def ::reckey (s/or :var ::var :| #{'|} :name keyword?))

(s/def ::record (s/map-of ::reckey ::type))

(s/def ::pred
  (s/cat :pred (s/alt :a ::app :t (s/coll-of (s/and seq? ::app) :kind vector?))
         :=>   #{'=>}
         :expr ::type-re))

(s/def ::type-re
  (s/alt :unit  ::unit
         :var   ::var
         :name  ::name
         :list  ::list
         :vect  ::vector
         :tuple ::tuple
         :app   ::app
         :arrow ::arrow
         :set   ::set
         :rec   ::record
         :pred  ::pred))

(s/def ::type
  (s/or :unit  ::unit
        :var   ::var
        :name  ::name
        :list  ::list
        :vect  ::vector
        :tuple ::tuple
        :app   ::app
        :arrow (s/and seq? ::arrow)
        :set   ::set
        :rec   ::record
        :pred  ::pred))

(defmulti parse* (fn [[tag]] tag))

(defmethod parse* :unit [[tag expr]] (Unit.))

(defmethod parse* :var [[tag expr]] (Var. expr))

(defmethod parse* :name [[tag expr]] (Const. expr))

(defmethod parse* :app [[tag {:keys [op args]}]]
  (Container. (parse* op) (mapv parse* args)))

(defmethod parse* :arrow [[tag {:keys [a b]}]]
  (Arrow. (parse* a) (parse* (second b))))

(defmethod parse* :tuple [[tag expr]]
  (Container. (Const. 'Tuple) (mapv parse* expr)))

(defmethod parse* :list [[tag {:keys [a]}]]
  (Container. (Const. 'List) [(parse* a)]))

(defmethod parse* :vect [[tag {:keys [a]}]]
  (Container. (Const. 'Vector) [(parse* a)]))

(defmethod parse* :set [[tag {:keys [a]}]]
  (Container. (Const. 'Set) [(parse* a)]))

(defmethod parse* :rec [[tag expr]]
  (let [r (some-> expr (get '|) second (Var.))]
    (->> (dissoc expr '|)
         (map (fn [[k v]] [(parse* (s/conform ::reckey k)) (parse* v)]))
         (reduce (fn [row [k v]] (Row. k v row)) (or r (RowEmpty.)))
         (Record.))))

(defmethod parse* :pred [[tag {:keys [expr pred]}]]
  (let [preds (case (first pred) :a [(second pred)] :t (second pred))]
    (Predicated.
     (mapv #(Predicate. (second (:op %)) (mapv parse* (:args %))) preds)
     (parse* expr))))

(defn parse [expr]
  (parse* (u/assert-conform ::type expr)))
