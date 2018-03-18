(ns lift.lang.type.spec
  (:require
   [clojure.spec.alpha :as s]
   [lift.lang.util :as u])
  (:import
   [lift.lang Instance]))


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

;; (def Type (Instance. nil 'Type []))

;; (defn Unit  []        (Instance. Type 'Unit []))
;; (defn Var   [a]       (Instance. Type 'Var [a]))
;; (defn Const [a]       (Instance. Type 'Const [a]))
;; (defn App   [op args] (Instance. Type op args))
;; (defn Arr   [a b]     (Instance. Type '-> [a b]))

;; (defn Row
;;   ([]
;;    (Instance. Type 'RowEmpty []))
;;   ([k v row]
;;    (Instance. Type 'Row [k v row])))

;; (defn Record [r]
;;   (Instance. Type 'Record [r]))

;; (defn Predicated [p e]
;;   (Instance. Type 'Predicated [p e]))

;; (defn Pred [op args]
;;   (Instance. Type 'Pred [p e]))

(defmulti parse* (fn [[tag]] tag))

(defmethod parse* :unit [[tag expr]] (Unit))

(defmethod parse* :var [[tag expr]] (Var expr))

(defmethod parse* :name [[tag expr]] (Const expr))

(defmethod parse* :app [[tag {:keys [op args]}]]
  (App op (mapv parse* args)))

(defmethod parse* :arrow [[tag {:keys [a b]}]]
  (Arr (parse* a) (parse* (second b))))

(defmethod parse* :tuple [[tag expr]]
  (Instance. Type 'Tuple (mapv parse* expr)))

(defmethod parse* :list [[tag {:keys [a]}]]
  (Instance. Type 'List [(parse* a)]))

(defmethod parse* :vect [[tag {:keys [a]}]]
  (Instance. Type 'Vector [(parse* a)]))

(defmethod parse* :set [[tag {:keys [a]}]]
  (Instance. Type 'Set [(parse* a)]))

(defmethod parse* :rec [[tag expr]]
  (let [r (some-> expr (get '|) second (Var))]
    (->> (dissoc expr '|)
         (map (fn [[k v]] [(parse* (s/conform ::reckey k)) (parse* v)]))
         (reduce (fn [row [k v]] (Row k v row)) (or r (Row)))
         (Record))))

(defmethod parse* :pred [[tag {:keys [expr pred]}]]
  (let [preds (case (first pred) :a [(second pred)] :t (second pred))]
    (Predicated.
     (mapv #(Predicate. (second (:op %)) (mapv parse* (:args %))) preds)
     (parse* expr))))

(defn parse [expr]
  (parse* (u/assert-conform ::type expr)))
