(ns lift.lang.type
  (:refer-clojure :exclude [case def])
  (:require
   [clojure.set :refer [difference union]]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.f.functor :as f :refer [Functor]]
   [lift.lang.type.impl :as impl :refer [Show]]
   [lift.lang.util :refer :all]
   [clojure.walk :as walk]
   [clojure.java.io :as io]
   [clojure.core :as c]
   [clojure.string :as str])
  (:import
   [clojure.lang IHashEq Indexed ISeq IPersistentVector]))

(alias 'c 'clojure.core)

(defn cata [f x]
  (f (f/-map x #(cata f %))))

(defprotocol Ftv (-ftv [x]))
(defprotocol Substitutable (-sub [x s]))

(def ftv (partial cata -ftv))
(def show (partial cata impl/-show))
(def substitute (partial cata -sub))

(extend-protocol Substitutable
  Object (-sub [x _] x))

(impl/deftype (Unit)
  Ftv           (-ftv [_] #{})
  Substitutable (-sub [x _] x)
  Show          (-show [_] "()"))

(impl/deftype (Const x)
  Ftv           (-ftv [_] #{})
  Substitutable (-sub [x _] x)
  Show          (-show [_] (name x)))

(impl/deftype (Var a)
  Ftv           (-ftv [_] #{a})
  Substitutable (-sub [_ s] (get s a x))
  Show          (-show [_] (name a)))

(impl/deftype (Vargs a)
  Ftv           (-ftv [_] #{a})
  Show          (-show [_] (str "& " a))
  Substitutable (-sub [_ s] (Vargs. (get s a a))))

(impl/deftype (Arrow a b)
  Ftv           (-ftv [_] (union a b))
  Substitutable (-sub [_ s] (Arrow. a b))
  Functor       (-map [_ f] (Arrow. (f a) (f b)))
  Show          (-show [_] (format "(%s -> %s)" a b)))

(impl/deftype (Forall as t)
  Ftv           (-ftv [x] (difference t as))
  Substitutable (-sub [_ s] (Forall. (apply dissoc s as) t))
  Show          (-show [_] (str (string/join " " (map #(str "∀" %) as)) \. t)))

(impl/deftype (Predicate tag a)
  Ftv           (-ftv [_] a)
  Substitutable (-sub [_ s] (Predicate. tag a))
  Show          (-show [_] (format "%s %s" (name tag) a)))

(impl/deftype (Predicated pred t)
  Ftv           (-ftv [_] (difference t pred))
  Substitutable (-sub [_ s] (Predicated. pred t))
  Show          (-show [_] (str (string/join ", " pred) " => " t)))

(impl/deftype (Literal a)
  Show (-show [_] (name a)))

(impl/deftype (Symbol a)
  Show (-show [_] (name a)))

(impl/deftype (Lambda x e)
  Show (-show [_] (format "(fn [%s] %s)" x e)))

(impl/deftype (Apply e1 e2)
  Show    (-show [_] (format "(%s %s)" e1 e2))
  Functor (-map [_ f] (Apply. (f e1) (f e2))))

(impl/deftype (Let x e1 e2)
  Show    (-show [_] (format "(let [%s %s] %s)" x e1 e2))
  Functor (-map [_ f] (Let. x (f e1) (f e2))))

(impl/deftype (If cond then else)
  Show    (-show [_] (format "(if %s %s %s)" cond then else))
  Functor (-map [_ f] (If. (f cond) (f then) (f else))))

(impl/deftype (Container tag args)
  Ftv           (-ftv [_] (set (apply concat args)))
  Show          (-show [_]
                  (if args
                    (format "(%s %s)" (name tag) (string/join ", " args))
                    (name tag)))
  Substitutable (-sub [_ s] (Container. tag args)))

(impl/deftype (RowEmpty)
  Ftv           (-ftv [_] #{})
  Show          (-show [_] "{}")
  Substitutable (-sub [x _] x))

(impl/deftype (Row k v tail)
  Ftv           (-ftv [_] (union v tail))
  Show          (-show [_] (format "%s : %s, %s" k v tail))
  Substitutable (-sub [_ _] (Row. k v tail)))

(impl/deftype (Record row)
  Ftv           (-ftv [_] (ftv row))
  Show          (-show [x] (format "{%s}" row))
  Substitutable (-sub [_ s] (Record. row)))

(impl/deftype (Variadic vfns)
  Functor (-map [_ f] (Variadic. (f/-map vfns f)))
  Show    (-show [_]
            (->> vfns
                 (map (fn [[xs e]] (format "(%s %s)" (pr-str xs) (pr-str e))))
                 (string/join "\n ")
                 (format "(fn\n %s)"))))

(impl/deftype (Env t)
  Ftv           (-ftv [_] (->> t vals (apply concat) set))
  Functor       (-map [_ f] (f/-map t f))
  Show          (-show [_] (str "Γ " x))
  Substitutable (-sub [x s] (Env. t)))

(impl/deftype (Vector xs)
  Ftv           (-ftv [_] (set xs))
  Functor       (-map [_ f] (Vector. (f/-map xs f)))
  Show          (-show [_] (format "[%s]" (string/join " " xs)))
  Substitutable (-sub [_ s] (Vector. xs)))

(impl/deftype (Map r)
  Functor (-map [_ f] (Map. (f/-map r f)))
  Show    (-show [_] (->> (map (fn [[k v]] (str k " " v)) r)
                          (string/join ", ")
                          (format "{%s}"))))

(impl/deftype (Tuple xs)
  Ftv           (-ftv [_] (set (apply concat xs)))
  Functor       (-map [_ f] (Tuple. (f/-map xs f)))
  Show          (-show [_] (format "[%s]" (string/join " " xs)))
  Substitutable (-sub [_ s] (Tuple. xs)))

(impl/deftype (Substitution s)
  Functor (-map [_ f] (f/-map s f))
  Show    (-show [_] (->> (map (fn [[k v]] (str v \/ k)) s)
                          (string/join ", ")
                          (format "S[%s]"))))

(defn arrow [a b]
  (Arrow. a b))

(defn sub [m]
  (merge (Substitution.) m))

(defn env [m]
  (merge (Env.) m))

(def type-env (atom {}))

(defn ex-unknown-type [t]
  (throw (ex-info (format "Unknown Type %s" (pr-str t))
                  {:type ::unknown-type :t t})))

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
  (let [env @type-env]
    (or (get env ast)
        (let [sym (resolve-sym ast)
              t (Const. sym)]
          #_(or (when (contains? env sym) t)
                (when (contains? env t) t))
          t)
        ;(ex-unknown-type ast)
        )))

(defmethod construct :type-const [[_ ast]] (Const. ast))

(defmethod construct :type-var [[_ ast]] (Var. ast))

(defmethod construct :arrow [[_ ast]]
  (->> (map :type (:more ast))
       (cons (:type ast))
       (map construct)
       (curry arrow)))

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

(defmethod construct :rec-cons [[_ [_ {:keys [type-name recmap]}]]]
  (Container. (resolve-sym type-name) [(f/map construct (VarSet. recmap))]))

(defmethod construct :record [[_ ast]]
  (-> ast
      (update :type-cons construct)
      (assoc :rec-cons (construct [:rec-cons (:rec-cons ast)]))))

(defmethod construct :simple-type [[_ type-name]]
  (Container. (resolve-sym type-name) nil))

(defmethod construct :parameterized [[_ [_ x]]]
  (let [tag (resolve-sym (:type-name x))]
    (let [args (map construct (:args x))]
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

(defn type-signature [sig]
  (let [spec (if (and (seq? sig) (= (count sig) 1)) ::retype ::type)
        conformed (s/conform spec sig)]
    (if (s/invalid? conformed)
      (do
        ;; (s/explain spec sig)
        (throw (Exception. (format "Could not parse type signature %s" sig))))
      (construct (s/conform spec sig)))))

(defmacro prim [t]
  `(let [t# (Const. '~t)]
     (swap! type-env assoc '~t t#)
     t#))

(defn record-keyword [r k]
  (let [ns (or (namespace k) (name (.getName *ns*)))
        nm (name k)]
    (keyword ns (if (.startsWith nm (str r \.)) nm (str r \. nm)))))

(defmacro def
  ([type]
   `(let [type# (type-signature '~type)]
      (swap! type-env assoc type# (Scheme. type# (ftv type#)))
      type#))
  ([type sig]
   (let [m (meta sig)]
     (cond
       (symbol? type)
       `(let [type# '~(resolve-sym type)
              ts# (type-signature '~sig)]
          (swap! type-env assoc type# (with-meta (Scheme. ts# (ftv ts#)) ~m))
          type#)
       (keyword? type)
       `(let [type# ~type
              ts# (type-signature '~sig)]
          (swap! type-env assoc type# (with-meta (Scheme. ts# (ftv ts#)) ~m))
          type#)))))

(defmacro def-tuples []
  (let [fst (int \a)]
    (cons 'do
          (apply concat
                 (for [n (range 1 16)]
                   (let [ns    (range 0 n)
                         vars  (mapv (comp symbol str char (partial + fst)) ns)
                         tuple (apply list (symbol (str "Tuple" n)) vars)]
                     `((lift.lang.type/def ~tuple)
                       ~@(apply concat
                                (for [[n2 a] (map vector ns vars)]
                                  (let [proj (symbol (str "proj-" n \- n2))]
                                    `((lift.lang.type/def ~proj ~(list tuple '-> a))
                                      (defn ~proj [~'x]
                                        (nth ~'x ~n2)))))))))))))
;; (def-tuples)

(defn unmatched-case-error []
  (throw (ex-info "Unmatched Case" {:type :unmatched-case-error})))

(defn type-rkeyword [rkw]
  (get @type-env rkw))

(defn record-type [tagstr]
  `(deftype ~(symbol (str "__" tagstr)) [x#]
     clojure.lang.ILookup
     (valAt [_# key#]
       (get x# (record-keyword ~tagstr key#)))
     clojure.lang.Associative
     (assoc [_# key# val#]
       ;; TODO: some rule amout same ns keywords?
       (__Record. (assoc x# (record-keyword ~tagstr key#) val#)))
     Show
     (show [_#]
       (format "(%s %s)" ~tagstr (pr-str x#)))))

(defn record-defn [tagstr margs]
  (let [arglist (vec (repeatedly (count margs) gensym))]
    `(defn ~(symbol tagstr) {:style/indent :defn} ~arglist
       (new ~(symbol (str "__" tagstr))
            ~(into {} (map vector (keys margs) arglist))))))

;; (defn record-constuct [tag rec-cons]
;;   (let [tagstr (name (.-tag rec-cons))
;;         tag' (symbol tagstr)
;;         args' (:vs (first (.-args rec-cons)))
;;         val-types (vec (mapcat -destructure (vals args')))
;;         margs (zipmap (map (partial record-keyword tagstr) (keys args'))
;;                       val-types)
;;         type-expr (list tag (into {} (map (juxt key (comp :val val)) args')))]
;;     `((t/def ~type-expr)
;;       (t/def ~tag ~(interpose '-> (conj val-types type-expr)))
;;       ~(record-type tagstr)
;;       ~(record-defn tagstr margs)
;;       ~@(map (fn [[k v]] `(t/def ~k ~(concat type-expr ['-> v]))) margs)
;;       (list '~(resolve-sym (first type-expr))
;;        '~@(rest type-expr)))))

;; (defmacro data
;;   {:style/indent :defn}
;;   [& decl]
;;   (let [{:keys [type-cons] :as parsed} (construct (s/conform ::data decl))
;;         args (mapv :val (.-args type-cons))
;;         tag (symbol (name (.-tag type-cons)))
;;         type-expr (apply list tag args)]
;;     `(do
;;        ~@(if-let [rec-cons (:rec-cons parsed)]
;;            (record-constuct tag rec-cons)
;;            (apply list
;;                   `(t/def ~type-expr)
;;                   (concat
;;                    (mapcat
;;                     (fn [part]
;;                       (let [container? (instance? Container part)
;;                             tag (if container? (symbol (name (.-tag part))) (:x part))
;;                             args (if container? (mapv :val (.-args part)) [])
;;                             size (count args)
;;                             tupl (symbol "type" (str "Tuple" size))
;;                             __name (str "__" (name tag))
;;                             __type-name (symbol __name)
;;                             destructor (symbol (str __name "-destructure"))
;;                             projs-syms (mapv #(symbol "type" (str "proj-" size \- %))
;;                                              (range 0 size))
;;                             projs (symbol (str __name "-projs"))]
;;                         `((t/def ~tag ~(if container?
;;                                          (interpose '-> (conj args type-expr))
;;                                          type-expr))
;;                           ~@(when container?
;;                               `((deftype ~__type-name ~args
;;                                   Show
;;                                   (show [_#]
;;                                     (format "(%s %s)" ~(str tag)
;;                                             (string/join " " (map pr-str ~args)))))))
;;                           ~@(if container?
;;                               `((defn ~(with-meta tag {:ctor __type-name})
;;                                   ~args (new ~__type-name ~@args)))
;;                               `((def ~tag (reify Show (show [_#] ~(name tag))))))
;;                           ~@(when container?
;;                               `((def ~projs '~projs-syms)
;;                                 (t/def ~destructor
;;                                   ~(interpose '-> (into [type-expr] [(cons tupl args)])))
;;                                 (defn ~destructor [~'x]
;;                                   ~(mapv (fn [a] `(~(symbol (str ".-" a)) ~'x)) args)))))))
;;                     (or (:sum-cons parsed)
;;                         [(:value-cons parsed)]))
;;                    [type-cons]))))))

(defmethod print-method Object [x w]
  (if (satisfies? Show x)
    (.write w (show x))
    (#'clojure.core/print-object x w)))

(defn case-form [x & [pattern expr & more]]
  (if (vector? pattern)
    (let [[c & as] pattern
          __name (str "__" c)
          destructor (symbol (str __name "-destructure"))
          projs @(resolve (symbol (str __name "-projs")))
          dsym (gensym)]
      `(if (instance? ~(symbol __name) ~x)
         (let* [~@(mapcat  (fn [a p] [a `(~p (~destructor ~x))]) as projs)]
           ~expr)
         ~(if (seq more)
            (apply case-form x more)
            `(t/unmatched-case-error))))
    `(if (= ~x ~pattern)
       ~expr
       ~(if (seq more)
          (apply case-form x more)
          `(t/unmatched-case-error)))))

(defn case [x decl]
  (let [xsym (gensym)]
    `(let [~xsym ~x]
       ~(apply case-form xsym decl))))

(defn lang [x]
  (prn "#clojure/lang" x)
  (if (= 'lift x)
    (do
      (prn "<3")
      )))

(defn require-typed [& [decl & more]]
  (loop [[ns & kwargs] decl]
    (let [path (-> (name ns)
                   (string/split #"\.")
                   (->> (string/join "/"))
                   (str ".cljt"))]
      (prn (io/resource path)))
    (when (seq more) (recur more))))

(prim Symbol)
(lift.lang.type/def t/unmatched-case-error a)
(lift.lang.type/def def (Symbol -> a -> ()))
(lift.lang.type/def restrict (l -> {l a | r} ->  {| r}))
