(ns lift.lang.type.data
  (:refer-clojure :exclude [type])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.lang.type.base :as base]
   [lift.lang.type.spec :as spec]
   [lift.lang.env :as env]
   [lift.lang.type.impl :as impl]
   [lift.lang.util :as u]
   [clojure.core :as c])
  (:import
   [lift.lang Instance Tagged]
   [lift.lang.type.impl Show]))

(s/def ::constructor
  (s/alt :name ::spec/name
         :app  ::spec/app))

(s/def ::product
  (s/cat :type-cons ::constructor := #{'=} :value-cons ::spec/app))

(s/def ::sum
  (s/cat :type-cons ::constructor
         := #{'=}
         :sum-cons (s/cat :type ::spec/type-re
                          :more (s/+ (s/cat :| #{'|} :type ::spec/type-re)))))

(s/def ::data
  (s/or :product ::product :sum ::sum))

(defn parse [[t ast]]
  (case t
    :product
    (let [{:keys [value-cons]} ast]
      (-> ast
          (update :type-cons spec/parse*)
          (assoc :value-cons (spec/parse* [:app value-cons]))
          (dissoc :=)))
    :sum
    (let [{:keys [sum-cons]} ast]
      (-> ast
          (update :type-cons spec/parse*)
          (assoc :sum-cons (map spec/parse* (cons (:type sum-cons)
                                                  (map :type (:more sum-cons)))))
          (dissoc :=)))))

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
  `(env/intern '~(u/resolve-sym tag) ~(u/curry #(Arrow. % %2) (into args [sig]))))

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

(defn container-arg [x]
  (if (instance? Var x)
    (:a x)
    (gensym)))

(defn container-impl [part sig]
  (let [tag (-> part :tag :x name symbol)
        args (:args part)
        arglist (mapv container-arg args)]
    `[~(container-intern-impl tag args sig)
      ~@(container-ctor-impl tag sig arglist args)]))

(defn value-impl [tag sig]
  (let [obj (reify
              Show (-show [_] (name tag))
              Tagged (getType [_] sig))]
    `[(def ~tag ~obj)
      ~(impl/prn-impl (class obj))
      (env/intern '~(u/resolve-sym tag) ~sig)]))

(defn Maybe [a]
  (Instance. (Const. 'Type) 'Maybe [(Var. a)]))

(defn type-ctor-impl [])

(defn data* [decl]
  (let [{:keys [type-cons] :as parsed} (parse (u/assert-conform ::data decl))
        args (mapv :a (:args type-cons))
        tag (-> type-cons :tag :x name symbol)
        type-expr (apply list tag args)
        sig (spec/parse type-expr)]
    `(do
       (env/intern ~sig ~sig)
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
  (let [tag (-> part :tag :x name symbol)
        args (:args part)
        arglist (mapv container-arg args)]
    [`(env/intern '~(u/resolve-sym tag) ~ctor-sig)
     (private-ctor-impl tag sig arglist args ctor-fn)]))

(defn private-data* [ctor-sig ctor-fn decl]
  (let [{:keys [type-cons value-cons] :as parsed} (parse decl)
        args (mapv :a (:args type-cons))
        tag (-> type-cons :tag :x name symbol)
        type-expr (apply list tag args)
        sig (spec/parse type-expr)
        ctor-sig (spec/parse ctor-sig)]
    ;; TODO: *must* check only one type param, and not a value/sumtype
    `(do
       (env/intern ~sig ~sig)
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
