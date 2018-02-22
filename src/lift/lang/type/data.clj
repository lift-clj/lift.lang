(ns lift.lang.type.data
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.lang.type :as t]
   [lift.lang.type.base :as base]
   [lift.lang.type.def :as def]
   [lift.lang.type.impl :as impl]
   [lift.lang.pattern :as p]
   [lift.lang.util :as u])
  (:import
   [lift.lang.type.impl Show]))

(base/import-types)

(defn container-intern-impl [tag args sig]
  `(def/intern-type-sig '~(u/resolve-sym tag)
     ~(u/curry #(Arrow. % %2) (into args [sig]))))

(defn private-classname [tag]
  (symbol (str (namespace-munge *ns*) ".__private." tag)))

(defn container-deftype-impl [tag classname args]
  `(deftype*
     ~(impl/type-name tag)
     ~classname
     ~args
     :implements
     [clojure.lang.Indexed
      lift.lang.type.impl.Show]
     (nth [_# i#] (nth ~args i#))
     (-show [_#]
       (format "(%s %s)" ~(str tag) (string/join " " (map pr-str ~args))))))

(defn prj-name [tag]
  (symbol (str "-prj-" (name tag))))

(defn container-prj-impl [tag classname sig arglist args]
  `{:prj {:isa? (list `instance? ~classname)
          :fs (mapv (fn [i# t#]
                      (Prim.
                       (list 'fn '~(prj-name tag) ['~'x] (list 'nth '~'x i#))
                       (Forall. (base/ftv ~sig) (Arrow. ~sig t#))))
                    (range)
                    ~args)}})

(defn container-ctor-impl [tag classname sig arglist args]
  `(defn ~(with-meta tag (container-prj-impl tag classname sig arglist args))
     ~arglist
     (new ~classname ~@arglist)))

(p/defn container-arg
  ([[Var a]] a)
  ([_] (gensym)))

(defn container-impl [part sig]
  (let [tag (symbol (name (.-tag part)))
        args (.-args part)
        arglist (mapv container-arg args)
        classname (private-classname tag)]
    [(container-intern-impl tag args sig)
     (container-deftype-impl tag classname arglist)
     (impl/prn-impl classname)
     (container-ctor-impl tag classname sig arglist args)]))

(defn value-impl [tag sig]
  (let [obj (reify Show (-show [_] (name tag)))]
    `[(def ~tag ~obj)
      ~(impl/prn-impl (class obj))
      (def/intern-type-sig '~(u/resolve-sym tag) ~sig)]))

(defn data* [decl]
  (let [{:keys [type-cons] :as parsed} (def/parse-data-decl decl)
        args (mapv :a (.-args type-cons))
        tag (symbol (name (.-tag type-cons)))
        type-expr (apply list tag args)
        sig (def/type-signature type-expr)]
    `(do
       ;; (swap! t/type-env assoc '~tag ~sig)
       (def/intern-type-only ~sig)
       ~@(mapcat
          (fn [part]
            (if (instance? Container part)
              (container-impl part sig)
              (value-impl (:x part) sig)))
          (or (:sum-cons parsed) [(:value-cons parsed)]))
       ~sig)))

(defn private-ctor-impl [tag classname dtor-sig arglist args ctor-fn]
  `(defn ~(with-meta tag (container-prj-impl tag classname dtor-sig arglist args))
     ~arglist
     (letfn [(~tag [x#] (new ~classname x#))]
       (~ctor-fn ~@arglist))))

(defn private-container-impl [part sig ctor-sig ctor-fn]
  (let [tag (symbol (name (.-tag part)))
        args (.-args part)
        arglist (mapv container-arg args)
        classname (private-classname tag)]
    [`(def/intern-type-sig '~(u/resolve-sym tag) ~ctor-sig)
     (container-deftype-impl tag classname arglist)
     (impl/prn-impl classname)
     (private-ctor-impl tag classname sig arglist args ctor-fn)]))

(defn private-data* [ctor-sig ctor-fn decl]
  (let [{:keys [type-cons value-cons] :as parsed} (def/parse-data-decl decl)
        args (mapv :a (.-args type-cons))
        tag (symbol (name (.-tag type-cons)))
        type-expr (apply list tag args)
        sig (def/type-signature type-expr)
        ctor-sig (def/type-signature ctor-sig)]
    ;; TODO: *must* check only one type param, and not a value/sumtype
    `(do
       (def/intern-type-only ~sig)
       ~@(private-container-impl value-cons sig ctor-sig ctor-fn)
       ~sig)))

(defmacro data
  {:style/indent :defn}
  [& decl]
  (data* decl))

(defmacro with-ctor
  {:style/indent :defn}
  [data-decl type-sig ctor-fn]
  (private-data* type-sig ctor-fn (rest data-decl)))
