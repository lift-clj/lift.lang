(ns lift.tools.loader
  (:refer-clojure :exclude [eval load])
  (:require [clojure.core :as c]
            [clojure.core.specs.alpha :as cs]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.tools.reader :as r]
            [lift.lang]
            [lift.lang.inference :as infer]
            [lift.lang.type.base :as base]
            [lift.lang.rewrite :as rewrite]
            [lift.lang.util :as u]
            [lift.lang.type.def :as def]
            [lift.lang.namespace :as ns]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def *loaded-libs* (ref #{}))

(defn root-resource [lib]
  (str \/
       (.. (name lib)
           (replace \- \_)
           (replace \. \/))))

(defn root-directory [lib]
  (let [d (root-resource lib)]
    (subs d 0 (.lastIndexOf d "/"))))

(defn extension [filename]
  (let [i (.lastIndexOf filename ".")]
    (when (pos? i) (subs filename (inc i)))))

(defn lift-macros []
  (->> 'lift.lang ns-publics vals (filter #(-> % meta :macro true?)) set))

(def ignore
  (set/union #{'try #'defmacro} (lift-macros)))

(defn ignore? [expr]
  (if-let [[op] (when (seq? expr) expr)]
    (and (symbol? op)
         (or (contains? ignore op)
             (contains? ignore (resolve op))))))

(defn type-check-error
  [[msg {:keys [file line column expr] :as x} :as infer-err] top-meta]
  (throw
   (clojure.lang.Compiler$CompilerException.
    (or file (:file top-meta) (pr-str expr))
    (or line (:line top-meta) 0)
    (or column (:column top-meta) 0)
    (Exception. msg))))

(defn check [expr]
  (let [[s1 [_ _ err :as e1]] (infer/checks expr)]
    (if (seq err)
      (throw (type-check-error (first err) (meta expr)))
      (base/substitute e1 s1))))

(defn rewrite [expr]
  (->> expr
       (rewrite/rewrite @infer/env base/id)
       (rewrite/emit)))

(defn eval [expr]
  (prn 'hi expr)
  (if (ignore? expr)
    (c/eval expr)
    (if (seq? expr)
      (let [[op & args] expr]
        (cond (= 'def op)
              (c/eval (def/def* args))
              (= 'in-ns op)
              (c/eval (ns/in-ns* (first args)))
              (= 'ns op)
              (c/eval (ns/ns* args))
              :else
              (-> expr u/macroexpand-all check rewrite c/eval)))
      (c/eval expr))))

(defn load* [reader path]
  (with-open [r (rt/indexing-push-back-reader reader 1 path)]
    (loop []
      (when-let [expr (r/read r false nil)]
        (eval expr)
        (recur)))))

(defn load [path]
  (let [path (if (extension path) path (str path ".cljd"))
        resource (io/resource path)]
    (some-> resource io/reader (load* path))))

(defn load-file-code [code filename]
  (letfn [(pr-str [x]
            (binding [*print-length* nil *print-level* nil] (c/pr-str x)))]
    (apply format
           "(lift.tools.loader/load* %s %s)"
           (map pr-str [code filename]))))

(s/def ::require-forms
  (s/+ ::cs/libspec))

(defn load-lib [lib]
  (when-not (contains? @*loaded-libs* lib)
    (some-> lib root-resource (subs 1) load)
    (dosync (commute *loaded-libs* conj lib))))

(defn require1 [[t e]]
  (prn t e)
  (case t
    :lib (load-lib e)
    :lib+opts (load-lib (:lib e))))

(defn require* [& forms]
  (run! require1 (u/assert-conform ::require-forms forms)))

(require* 'lift.lang.type.stlc)
