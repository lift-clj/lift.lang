(ns lift.lang.type.def
  (:refer-clojure :exclude [ns-name])
  (:require
   [clojure.spec.alpha :as s]
   [lift.lang.type.base :as base]
   [lift.lang.util :as u]))

(base/import-container-types)
(base/import-type-types)

(def type-env (atom {}))

(defn find-type [_Gamma tag]
  (some (fn [[k v]] (and (instance? Container k) (= tag (:tag k)) v)) _Gamma))

(defn get-type [_Gamma tag]
  (get _Gamma tag))

(defn ns-name [ns]
  (cond (symbol? ns) (name ns)
        (instance? clojure.lang.Namespace ns) (.name ns)
        :else (throw (Exception. (str "Cannot get name of non-ns " (pr-str ns))))))

(defn try-ns-resolve [ns s]
  (try (ns-resolve ns s) (catch Throwable _)))

(defn resolve-sym
  ([ns s]
   (if (namespace s)
     (some->> s (try-ns-resolve ns) u/->sym)
     (or (some->> s (try-ns-resolve ns) u/->sym)
         (get-type @type-env (symbol (name (ns-name ns)) (name s)))
         (find-type @type-env (symbol (name (ns-name ns)) (name s)))
         (some->> s (try-ns-resolve 'lift.lang) u/->sym)
         (let [s' (symbol "lift.lang" (name s))]
           (or (when (get-type @type-env s') s')
               (when (find-type @type-env s') s')))
         (u/ns-qualify s))))
  ([s]
   (resolve-sym *ns* s)))

(s/def ::unit #{()})

(s/def ::type-name
  (s/and symbol? #(re-matches #"^[A-Z][A-z0-9]*$" (name %))))

(s/def ::type-const qualified-keyword?)

(s/def ::type-var
  (s/and simple-symbol? #(re-matches #"^[a-z\-]+$" (name %))))

(s/def ::arrow-regex
  (s/cat :type ::retype :more (s/+ (s/cat :-> #{'->} :type ::retype))))

(s/def ::arrow-parens
  (s/and seq? ::arrow-regex))

(s/def ::arrow
  (s/alt :noparens ::arrow-regex
         :parens   ::arrow-parens))

(s/def ::parameterized-regex
  (s/cat :type-name ::type-name :args (s/+ ::type)))

(s/def ::parameterized
  (s/alt :parens   (s/and seq? ::parameterized-regex)
         :noparens ::parameterized-regex))

(s/def ::rec-cons
  (s/alt :noparens (s/cat :type-name ::type-name
                          :recmap (s/map-of keyword? ::type))
         :parens (s/and seq? (s/cat :type-name ::type-name
                                    :recmap (s/map-of keyword? ::type)))))

(s/def ::row
  (s/map-of (s/or :type-var ::type-var :| #{'|}) ::type))

(s/def ::type
  (s/or :unit          ::unit
        :type-name     ::type-name
        :type-const    ::type-const
        :type-var      ::type-var
        :arrow         ::arrow-parens
        :parameterized ::parameterized
        :rec-cons      ::rec-cons
        :row           ::row))

(s/def ::retype
  (s/alt :unit          ::unit
         :type-name     ::type-name
         :type-const    ::type-const
         :type-var      ::type-var
         :arrow         ::arrow-parens
         :parameterized ::parameterized
         :rec-cons      ::rec-cons
         :row           ::row))

(s/def ::product
  (s/cat :type-cons ::type-cons := #{'=} :value-cons ::parameterized))

(s/def ::type-cons
  (s/alt :simple-type ::type-name
         :parameterized ::parameterized))

(s/def ::sum-cons
  (s/cat :type ::retype
         :more (s/+ (s/cat :| #{'|} :type ::retype))))

(s/def ::sum
  (s/cat :type-cons ::type-cons := #{'=} :sum-cons ::sum-cons))

(s/def ::record
  (s/cat :type-cons ::type-cons := #{'=} :rec-cons ::rec-cons))

(s/def ::data
  (s/or :product ::product :sum ::sum :record ::record))


(def construct nil)
(defmulti construct first)

(defmethod construct :unit  [[_ _]] (Unit.))

(defmethod construct :type-name [[_ ast]]
  (let [sym (resolve-sym ast)]
    (Const. sym)))

(defmethod construct :type-const [[_ ast]] (Const. ast))

(defmethod construct :type-var [[_ ast]] (Var. ast))

(defmethod construct :arrow [[_ ast]]
  (->> (map :type (:more ast))
       (cons (:type ast))
       (map construct)
       (u/curry #(Arrow. % %2))))

(defmethod construct :product [[_ ast]]
  (let [{:keys [value-cons]} ast]
    (-> ast
        (update :type-cons construct)
        (assoc :value-cons (construct [:parameterized value-cons]))
        (dissoc :=))))

(defmethod construct :sum [[_ ast]]
  (let [{:keys [sum-cons]} ast]
    (-> ast
        (update :type-cons construct)
        (assoc :sum-cons (map construct (cons (:type sum-cons)
                                              (map :type (:more sum-cons)))))
        (dissoc :=))))

;; (defmethod construct :rec-cons [[_ [_ {:keys [type-name recmap]}]]]
;;   (Container. (resolve-sym type-name) [(f/map construct (VarSet. recmap))]))

(defmethod construct :record [[_ ast]]
  (-> ast
      (update :type-cons construct)
      (assoc :rec-cons (construct [:rec-cons (:rec-cons ast)]))))

(defmethod construct :simple-type [[_ type-name]]
  (Container. (resolve-sym type-name) nil))

(defmethod construct :parameterized [[_ [_ x]]]
  (let [tag (resolve-sym (:type-name x))]
    (let [args (mapv construct (:args x))]
      (Container. tag args))))

(defmethod construct :non-constrained [[_ ast]]
  (construct ast))

(defmethod construct :sub-arrow [[_ ast]]
  (construct ast))

(defmethod construct :noparens [[_ ast]]
  (construct [:arrow ast]))

(defmethod construct :row [[_ ast]]
  (let [r (some-> ast (get '|) second (Var.))]
    (->> (dissoc ast '|)
         (map (fn [[k v]] [(construct [:type-var k]) (construct v)]))
         (reduce (fn [row [k v]] (Row. k v row)) (or r (RowEmpty.)))
         (Record.))))

(defn parse-data-decl [decl]
  (construct (u/assert-conform ::data decl)))

(defn type-signature [sig]
  (let [spec (if (and (seq? sig) (= (count sig) 1)) ::retype ::type)]
    (construct (u/assert-conform spec sig))))

(defmacro prim [t]
  `(let [t# (Const. '~t)]
     (swap! type-env assoc '~t t#)
     t#))

(defn intern-type-only [type]
  (swap! type-env assoc type (Forall. (base/ftv type) type))
  type)

(defn intern-type-sig [type sig]
  (swap! type-env assoc type (Forall. (base/ftv sig) sig))
  type)

(defmacro intern-signature
  ([type]
   `(intern-type-only (type-signature '~type)))
  ([type sig]
   `(intern-type-sig '~(resolve-sym type) (type-signature '~sig))))

;; (map (comp prn key) @type-env)

;; (@type-env 'Pair)
