(ns lift.lang.type
  (:refer-clojure :exclude [case def])
  (:require
   [clojure.core :as c]
   [clojure.core.protocols :refer [IKVReduce]]
   [clojure.java.io :as io]
   [clojure.set :refer [difference union]]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.f.functor :as f :refer [Functor]]
   [lift.lang.util :as u]
   [lift.lang.type.base :as base]
   [lift.lang.type.def :as def]))

(base/import-container-types)
(base/import-infer-types)
(base/import-type-types)

(defn sub [s]
  (Substitution. s))

(def id (sub {}))

(defn env [e]
  (Env. e))

(def type-env def/type-env)
(def ftv base/ftv)
(def substitute base/substitute)

(defn ex-unknown-type [t]
  (throw (ex-info (format "Unknown Type %s" (pr-str t))
                  {:type ::unknown-type :t t})))

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

(defn unmatched-case-error [x]
  (throw (ex-info "Unmatched Case" {:type :unmatched-case-error :x x})))

(defn type-rkeyword [rkw]
  (get @type-env rkw))

;; (defmethod print-method Object [x w]
;;   (if (satisfies? Show x)
;;     (.write w (show x))
;;     (#'clojure.core/print-object x w)))

(defn case-form [x & [pattern expr & more]]
  (if (vector? pattern)
    (let [[c & as] pattern
          {{:keys [isa? fs]} :prj} (-> c resolve meta)
          gsyms (map (fn [_] (gensym)) as)]
      `(if (~@isa? ~x)
         (let* [~@(mapcat (fn [a g p] [(if (symbol? a) a g) `(~p ~x)])
                          as gsyms fs)]
           ~(reduce (fn [expr [s a]]
                      (if (vector? a) (case-form s a expr) expr))
                    expr
                    (reverse (map vector gsyms as))))
         ~(if (seq more)
            (apply case-form x more)
            `(t/unmatched-case-error ~x))))
    `(if (lift.lang.prim/eq ~x ~(u/resolve-sym pattern))
       ~expr
       ~(if (seq more)
          (apply case-form x more)
          `(t/unmatched-case-error ~x)))))

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

;; (prim Symbol)
;; (lift.lang.type/def t/unmatched-case-error a)
;; (lift.lang.type/def def (Symbol -> a -> ()))
;; (lift.lang.type/def restrict (l -> {l a | r} ->  {| r}))

(defmacro def [name sig]
  `(def/intern-signature ~name ~sig))
