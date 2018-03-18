(ns lift.lang.type.data
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.lang.env :as env]
   [lift.lang.type.impl :as impl]
   [lift.lang.type.spec :as spec]
   [lift.lang.util :as u])
  (:import
   [lift.lang Instance Tagged]
   [lift.lang.type.impl Show]))

(def projections (ref {}))

(defprotocol ADT
  (tag [_])
  (dbg [_]))

(defn adt-impl [tag arglist]
  (let [hq (volatile! nil)]
    (with-meta
      (reify
        ADT
        (tag [_] tag)
        (dbg [_]
          (if (seq arglist)
            (format (str "(" (name tag) " %s)")
                    (string/join " " (map pr-str arglist)))
            (name tag)))
        clojure.lang.Indexed
        (nth [_ i _] (nth arglist i nil))
        clojure.lang.IHashEq
        (hasheq [_]
          (or @hq (vreset! hq (bit-xor (.hasheq tag) (.hasheq arglist)))))
        Object
        (hashCode [this] (.hasheq this))
        (equals [_ other]
          (and (satisfies? ADT other)
               (= tag (tag other))
               (->> (map-indexed #(= (nth other % nil) %2) arglist)
                    (every? true?)))))
      {:type ::ADT})))

(defmacro prim [name & args]
  (let [args (vec args)]
    `(defn ~name ~args (adt-impl '~name ~args))))

(s/def ::unit #{()})

(s/def ::var
  (s/and simple-symbol? #(re-matches #"^[a-z][a-z0-9]*$" (name %))))

(s/def ::name
  (s/and simple-symbol? #(re-matches #"[A-Z][a-zA-Z0-9]*" (name %))))

(s/def ::app
  (s/cat :op (s/or :var ::var :name ::name) :args (s/+ ::type)))

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

(s/def ::constructor
  (s/alt :name ::name
         :app  ::app))

(s/def ::product
  (s/cat :type-cons ::constructor := #{'=} :value-cons ::app))

(s/def ::sum
  (s/cat :type-cons ::constructor
         := #{'=}
         :sum-cons (s/cat :type ::type-re
                          :more (s/+ (s/cat :| #{'|} :type ::type-re)))))

(s/def ::data
  (s/or :product ::product :sum ::sum))

(defn prj-name [tag]
  (symbol (str "-prj-" (name tag))))

(defn parse-arg [[tag expr]]
  (case tag
    :name (list 'Const (list 'quote expr))
    :var  (list 'Var   (list 'quote expr))))

(defn parse-type-ctor [[tag expr]]
  (case tag
    :name (list 'Const expr)
    :app  (u/curry (partial list 'Apply)
                   (cons (parse-arg (:op expr))
                         (map parse-arg (:args expr))))))

(defn container-prj-impl [tag sig args]
  (let [tag' (u/resolve-sym tag)]
    `(dosync
      (alter projections assoc '~tag'
             ~(mapv (fn [i t]
                      `(Prim
                        (fn ~(prj-name tag) [x#] (nth x# ~i))
                        (Forall (base/ftv ~(parse-type-ctor sig))
                                (Arrow ~(parse-type-ctor sig) ~(parse-arg t)))))
                    (range)
                    args)))))

(defmethod print-method ::ADT [o, ^java.io.Writer w]
  (.write w (dbg o)))

(defn container-ctor-impl [tag sig arglist args]
  `(~(container-prj-impl tag sig args)
    (defn ~tag ~arglist (adt-impl '~tag ~arglist))))

(defn container-arg [x]
  (if (= :var (first x))
    (second x)
    (gensym)))

(defn container-intern-impl [tag args sig]
  `(env/intern
    '~(u/resolve-sym tag)
    ~(->> (into (map parse-arg args)
                [(parse-type-ctor sig)])
          (u/curry (partial list 'Arrow)))))

(defn container-impl [part type-cons]
  (let [tag     (-> part :op second)
        args    (:args part)
        arglist (mapv container-arg args)]
    `[~(container-intern-impl tag args type-cons)
      ~@(container-ctor-impl tag type-cons arglist args)]))

(defn value-impl [tag sig]
  `((def ~tag (adt-impl '~tag nil))
    (env/intern '~(u/resolve-sym tag) ~sig)))

(defn type-ctor-impl [[tag expr]]
  (case tag
    :name
    `((env/intern '~expr (Const 'Type))
      (def ~expr (adt-impl '~expr nil)))
    :app
    (let [ctor (-> expr :op second)
          argl (mapv container-arg (:args expr))]
      `((env/intern '~ctor
                    ~(u/curry (partial list 'Arrow)
                              (map (fn [_] (list 'Const 'Type)) (cons 'Type argl))))
        (defn ~ctor ~argl (adt-impl '~ctor ~argl))))))

(defn data* [decl]
  (let [[_ {:keys [type-cons] :as parsed}] (u/assert-conform ::data decl)
        type-ctor (parse-type-ctor type-cons)
        sum-ctors (when-let [ctors (:sum-cons parsed)]
                    (cons (:type ctors) (map :type(:more ctors))))]
    `(do
       ~@(type-ctor-impl type-cons)
       ~@(mapcat
          (fn [[tag part]]
            (if (= :app tag)
              (container-impl part type-cons)
              (value-impl part type-ctor)))
          (or sum-ctors [(:value-cons parsed)]))
       ~type-ctor)))

;; (defn private-ctor-impl [tag dtor-sig arglist args ctor-fn]
;;   `(defn ~(with-meta tag (container-prj-impl tag dtor-sig arglist args))
;;      ~arglist
;;      (letfn [(~tag [& xs#] (Instance. dtor-sig ~tag xs#))]
;;        (~ctor-fn ~@arglist))))

;; (defn private-container-impl [part sig ctor-sig ctor-fn]
;;   (let [tag (-> part :tag :x name symbol)
;;         args (:args part)
;;         arglist (mapv container-arg args)]
;;     [`(env/intern '~(u/resolve-sym tag) ~ctor-sig)
;;      (private-ctor-impl tag sig arglist args ctor-fn)]))

;; (defn private-data* [ctor-sig ctor-fn decl]
;;   (let [{:keys [type-cons value-cons] :as parsed} (parse decl)
;;         args (mapv :a (:args type-cons))
;;         tag (-> type-cons :tag :x name symbol)
;;         type-expr (apply list tag args)
;;         sig (parse-type type-expr)
;;         ctor-sig (parse-type ctor-sig)]
;;     ;; TODO: *must* check only one type param, and not a value/sumtype
;;     `(do
;;        (env/intern ~sig ~sig)
;;        ~@(private-container-impl value-cons sig ctor-sig ctor-fn)
;;        ~sig)))

(defmacro data
  {:style/indent :defn}
  [& decl]
  (data* decl))

;; (defmacro with-ctor
;;   {:style/indent :defn}
;;   [data-decl type-sig ctor-fn]
;;   (private-data* type-sig ctor-fn (rest data-decl)))

;; (data Unit = Unit)

(prim Var a)
(prim Const a)
(prim Apply e1 e2)
(prim Arrow a b)

(data Expr
  = Var Symbol
  | Const Symbol
  | Apply Expr Expr
  | Arrow Expr Expr)

(data Maybe a = Just a | Nothing)
