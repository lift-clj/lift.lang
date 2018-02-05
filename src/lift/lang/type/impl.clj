(ns lift.lang.type.impl
  (:refer-clojure :exclude [deftype])
  (:require
   [clojure.string :as string]
   [lift.f.functor :as f :refer [Functor]]
   [clojure.set :as set])
  (:import
   [clojure.lang ISeq IPersistentMap]))

(defn cata [f x]
  (f (f/map #(cata f %) x)))

(defn hylo [f g x]
  (f (f/-map (g x) #(hylo f g %))))

(defprotocol Type)

(extend-protocol Functor

  ;; Type (-map [x _] x)
  ;; Object (-map [x _] x)
  ;; nil    (-map [_ f] nil)
  )

;; (alter-var-root #'Functor (fn [x] (update x :impls dissoc java.lang.Object)))

(defprotocol Show (-show [_]))

(defn show [x] (cata -show x))
;; (alter-var-root #'Show (fn [x] (assoc x :impls {})))

(extend-protocol Show
  Object
  (-show [x] x)
  IPersistentMap
  (-show [x] (->> (map (fn [[k v]] (str (show k) " " v)) x) (string/join ", "))))


(defn ->sym [x]
  (cond
    (symbol? x) x
    (var? x)    (let [{:keys [name ns]} (meta x)] (symbol (str ns) (str name)))
    (class? x)  (symbol (.getName x))))

(defn resolve-sym [sym]
  (let [res (resolve sym)]
    (->sym res)))

(defn qualify-sym [sym]
  (symbol (name (.getName *ns*)) (name sym)))

(defn resolve-class [sym]
  (let [res (resolve sym)]
    (cond
      (var? res) (let [{:keys [name ns]} (meta res)] (symbol (str ns \. name)))
      (class? res) (symbol (.getName res)))))

(defn hyphenate [s]
  (->> (string/split s #"(?=[A-Z]+[^A-Z]?)")
       (string/join "-")))

(defn kebab [s]
  (string/lower-case (hyphenate s)))

(defn kebab-case [x]
  (kebab (name x)))

(defn base-classname [tag]
  (symbol (str (namespace-munge *ns*) "." tag)))

(defn type-name [tag]
  (symbol (name (ns-name *ns*)) (name tag)))

(defn ctor-name [tag]
  (symbol (kebab-case tag)))

(defn seq-impl [args]
  `(list ~@args))

(defn equiv-impl [f args]
  `(~f ~'[this other]
    (boolean
     (or (identical? ~'this ~'other)
         (when (identical? (class ~'this) (class ~'other))
           (every? true? (map = ~args (seq ~'other))))))))

(defn hasheq-impl [tag args]
  `(~'hasheq [~'_]
    (int (bit-xor ~(hash tag)
                  (clojure.lang.APersistentMap/mapHasheq
                   ~(zipmap (map #(list 'quote %) args) args))))))

(defn hashcode-impl [args]
  `(~'hashCode [~'_]
    (clojure.lang.APersistentMap/mapHasheq
     ~(zipmap (map #(list 'quote %) args) args))))

(defn seq-impl [args]
  `(~'seq ~'[_] (list ~@args)))

(defn assoc-impl [ctor args]
  `(assoc ~'[_ k v]
    (case ~'k
      ~@(mapcat (fn [a] [(keyword a) (list* 'new ctor (replace {a 'v} args))])
                args)
      (throw (Exception. (format "Key %s not part of type" ~'k))))))

(defn get-impl [args]
  `[(~'valAt ~'[_ k]
     (case ~'k
       ~@(mapcat (fn [a] [(keyword a) a]) args)
       (throw (Exception. (format "Key %s not part of type" ~'k)))))
    (~'valAt ~'[_ k not-found]
     (case ~'k
       ~@(mapcat (fn [a] [(keyword a) a]) args)
       ~'not-found))])

(defn show-impl [tag args]
  `(-show ~'[_]
     (format "%s %s" ~(pr-str tag) (string/join " " (map pr-str ~args)))))

(defn prn-impl [classname]
  `(defmethod print-method ~classname [~'x ~'w] (.write ~'w (show ~'x))))

(defn functor-impl [tag args]
  `(f/-map ~'[_ f] (new ~tag ~@(butlast args)
                        ~@(if-let [l (last args)] [`(~'f ~l)] []))))

(def default-ifaces
  '#{clojure.lang.IHashEq
     clojure.lang.ILookup
     clojure.lang.Indexed
     clojure.lang.ISeq
     clojure.lang.Associative
     lift.f.functor.Functor
     lift.lang.type.impl.Show
     lift.lang.type.impl.Type})

(defn default-impls [tag args impls]
  (->> [[(equiv-impl 'equals args)]
        [(equiv-impl 'equiv args)]
        [(hasheq-impl tag args)]
        [(hashcode-impl args)]
        [(list 'nth '[_ index] (list 'nth args 'index))]
        [(list 'nth '[_ index not-found] (list 'nth args 'index 'not-found))]
        [(seq-impl args)]
        [(assoc-impl tag args)]
        (when-not (contains? impls 'clojure.lang.ILookup)
          (get-impl args))
        (when-not (contains? impls 'lift.lang.type.impl.Show)
          [(show-impl tag args)])
        (when-not (contains? impls 'lift.f.functor.Functor)
          [(functor-impl tag args)])]
       (apply concat)))

(defn deftype-expr [tag args impls]
  (let [ifaces (set (map resolve-class (take-nth 2 impls)))
        impls  (take-nth 2 (rest impls))]
    `(deftype*
       ~(type-name tag)
       ~(base-classname tag)
       ~(vec args)
       :implements
       ~(vec (sort (set/union default-ifaces ifaces)))
       ~@(default-impls tag args ifaces)
       ~@impls)))

(defmacro deftype
  {:style/indent [:defn :defn]}
  [[tag & args] & impls]
  (let [cls (base-classname tag)]
    (list 'do
          (deftype-expr tag (vec args) impls)
          (prn-impl cls)
          (list 'import cls)
          (list 'quote (qualify-sym tag)))))
