(ns lift.lang.type.data
  (:refer-clojure :exclude [type])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.lang.type.base :as base]
   [lift.lang.type.def :as def]
   [lift.lang.type.impl :as impl]
   [lift.lang.pattern :as p]
   [lift.lang.util :as u]
   [clojure.java.io :as io]
   [clojure.core :as c])
  (:import
   [lift.lang Instance Tagged]
   [lift.lang.type.impl Show]))

(defmethod print-method Instance [x w]
  (->> (map pr-str (.-elems x))
       (string/join ", ")
       (format "(%s %s)" (.-tag x))
       (.write w)))

(defmethod print-dup Instance [x w]
  (->> (map pr-str (.-elems x))
       (string/join ", ")
       (format "(%s %s)" (.-tag x))
       (.write w)))

(base/import-types)

(defn type [x]
  (if (instance? Tagged x)
    (.getType x)
    (-> x c/type .getSimpleName symbol Const.)))

(defn container-intern-impl [tag args sig]
  `(def/intern-type-sig '~(u/resolve-sym tag)
     ~(u/curry #(Arrow. % %2) (into args [sig]))))

(defn prj-name [tag]
  (symbol (str "-prj-" (name tag))))

(defn container-prj-impl [tag sig arglist args]
  (let [tag' (u/resolve-sym tag)]
    `((lift.lang.Instance/resetProjections '~tag')
      ~@(mapv (fn [i t]
                `(lift.lang.Instance/addProjection
                  '~tag'
                  (Prim.
                   (fn ~(prj-name tag) [x#] (.nth x# ~i))
                   (Forall. (base/ftv ~sig) (Arrow. ~sig ~t)))))
              (range)
              args))))

(defn container-ctor-impl [tag sig arglist args]
  `(~@(container-prj-impl tag sig arglist args)
    (defn ~tag
      ~arglist
      (Instance. ~sig '~(u/resolve-sym tag) ~arglist))))

(p/defn container-arg
  ([[Var a]] a)
  ([_] (gensym)))

(defn container-impl [part sig]
  (let [tag (symbol (name (.-tag part)))
        args (.-args part)
        arglist (mapv container-arg args)]
    `[~(container-intern-impl tag args sig)
      ~@(container-ctor-impl tag sig arglist args)]))

(defn value-impl [tag sig]
  (let [obj (reify
              Show (-show [_] (name tag))
              Tagged (getType [_] sig))]
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
       (def/intern-type-only ~sig)
       ~@(mapcat
          (fn [part]
            (if (instance? Container part)
              (container-impl part sig)
              (value-impl (:x part) sig)))
          (or (:sum-cons parsed) [(:value-cons parsed)]))
       ~sig)))

(defn private-ctor-impl [tag dtor-sig arglist args ctor-fn]
  `(defn ~(with-meta tag (container-prj-impl tag dtor-sig arglist args))
     ~arglist
     (letfn [(~tag [& xs#] (Instance. dtor-sig ~tag xs#))]
       (~ctor-fn ~@arglist))))

(defn private-container-impl [part sig ctor-sig ctor-fn]
  (let [tag (symbol (name (.-tag part)))
        args (.-args part)
        arglist (mapv container-arg args)]
    [`(def/intern-type-sig '~(u/resolve-sym tag) ~ctor-sig)
     (private-ctor-impl tag sig arglist args ctor-fn)]))

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
