(ns lift.middleware
  (:refer-clojure :exclude [read read-string])
  (:require
   [clojure.tools.reader :as r]
   [clojure.tools.reader.reader-types :as rt :refer [read-char unread]]
   [clojure.tools.reader.impl.utils :refer [whitespace?]]
   [clojure.tools.nrepl.middleware :as mw]
   [clojure.tools.nrepl.middleware.load-file :as load-file]
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as cs]
   [clojure.string :as string]
   [clojure.java.io :as io]
   [clojure.walk :as walk]
   [lift.lang :as lift]
   [lift.lang.inference :as infer]
   [lift.tools.loader :as loader]
   [lift.lang.rewrite :as rewrite]
   [lift.lang.type :as type]
   [lift.lang.type.base :as base]
   [lift.lang.util :as u]
   [lift.lang.analyze :as ana]
   [lift.lang.namespace :as ns]
   [lift.lang.type.impl :as impl]
   [lift.lang.signatures :as sig]
   [lift.lang.type.def :as def]
   [lift.tools.reader :as rdr]
   [lift.lang.unification :as unify])
  (:import
   [lift.lang.type.base Forall Lambda Let Literal Mark Prim SyntaxNode Var]
   [lift.lang.type.impl Type]))

(def ^:dynamic *type-check* true)
(def ^:dynamic *prn-type* false)

(declare ignore)

(defn ignore? [[op]]
  (and (symbol? op)
       (or (contains? ignore op)
           (contains? ignore (resolve op)))))

(deftype Ret [x t])

(defmethod print-method Ret [o w]
  (.write w (format "%s = %s" (pr-str (.-x o)) (pr-str (.-t o)))))

(defn toggle-type-checker [& _]
  (Ret. '*type-check* (alter-var-root #'*type-check* not)))

(defn read-colon [reader initch opts pending-forms]
  (let [ch (read-char reader)]
    (if (or (nil? ch)
            (whitespace? ch)
            (#'r/macro-terminating? ch))
      (symbol ":")
      (do
        (unread reader ch)
        (#'r/read-keyword reader initch opts pending-forms)))))

(def macros @#'clojure.tools.reader/macros)

(defn reader-macros [ch]
  (if (= ch \:) read-colon (macros ch)))

(c/defmacro with-patched-reader [& body]
  `(with-redefs [clojure.tools.reader/macros reader-macros]
     ~@body))

(defn read
  ([]
   (read *in*))
  ([reader]
   (read reader true nil))
  ([reader eof-error? eof-value]
   (with-patched-reader (r/read reader (boolean eof-error?) eof-value)))
  ([opts reader]
   (with-patched-reader (r/read opts reader))))

(defn read-string [s]
  (with-patched-reader (r/read-string s)))

(defn sub-pretty-vars
  ([[f & ftvs] [v & vars]]
   (if f (assoc (sub-pretty-vars ftvs vars) f v) {}))
  ([ftvs]
   (-> (sort ftvs)
       (sub-pretty-vars (map (comp #(Var. %) symbol str char) (range 97 123)))
       (type/sub))))

(defn check
  ([expr] (check @infer/env expr))
  ([env expr]
   (let [[s ast] ((impl/hylo infer/infer1 ana/parse expr) env)]
     [s (type/substitute ast s)])))

(defn def? [expr]
  (and (seq? expr) (= 'def (first expr))))

(defn type-check-error
  [[msg {:keys [file line column expr] :as x} :as infer-err] top-meta]
  (throw
   (clojure.lang.Compiler$CompilerException.
    (or file (:file top-meta) (pr-str expr))
    (or line (:line top-meta) 0)
    (or column (:column top-meta) 0)
    (Exception. msg))))

(defn type-of-symbol [ns expr]
  (when-let [t (and (symbol? expr)
                    (get @infer/env (u/resolve-sym ns expr)))]
    (base/$ (ns-resolve ns expr) t)))

(defn type-of-type [expr]
  (try
    (when-let [t (get @infer/env (def/type-signature expr))]
      (base/$ expr t))
    (catch Throwable _)))

(defn find-mark [x]
  (let [p (fn [x] (and (instance? SyntaxNode x) (-> x :m :mark true?)))
        a (atom nil)
        f (fn [x] (reset! a (base/$ (pr-str (:expr (:m x))) (:t x))))]
    (if (p x)
      (f x)
      (do
        (impl/cata (fn [x]
                     (cond (p x)
                           (f x)
                           (and (or (instance? Lambda x) (instance? Let x))
                                (instance? SyntaxNode (:x x))
                                (-> x :x :m :mark true?))
                           (f (-> x :x))
                           :else
                           x)) x)
        @a))))

(defn control? [x]
  (and (map? x) (contains? x ::op)))

(defn unmark-symbols [x]
  (walk/postwalk (fn [x]
                   (if (and (instance? Mark x) (-> x :a symbol?))
                     (vary-meta (:a x) assoc :mark true)
                     x))
                 x))

(defn type-of-expr-at-point [{:keys [file ns]
                              {[_ _ t :as tp] :pos top  :code} :top
                              {[_ _ e :as ep] :pos expr :code} :expr}]
  (try
    (or (and (not (symbol? expr))
             (ana/literal? expr)
             (base/$ (Literal. expr) (ana/type (Literal. expr))))

        (type-of-symbol ns expr)

        (type-of-type expr)

        (let [expr (unmark-symbols (rdr/top-level-sexp top (- e t)))
              code (u/macroexpand-all expr)
              [s [n t err :as expr]] (check code)]
          (let [ftvs (base/ftv t)
                sub  (sub-pretty-vars ftvs)
                t'   (type/substitute t sub)
                sigma (Forall. (base/ftv t') t')]
            (-> (find-mark expr)
                (type/substitute s)
                (type/substitute sub)))))
    (catch Throwable t
      (println t)
      t)))

(def impl-namespaces
  (->> '[lift.lang.case
         lift.lang.defn
         lift.lang.type
         lift.lang.type.base
         lift.lang.type.impl]
       (map name)
       (set)))

(defn impl? [sym]
  (contains? impl-namespaces (namespace sym)))

(defn type-search-expr-at-point [msg]
  (let [t? (type-of-expr-at-point msg)]
    (if (instance? Throwable t?)
      t?
      (let [t' (infer/instantiate (Forall. (base/ftv (:t t?)) (:t t?)))]
        (->> @infer/env
             (keep (fn [[k v]]
                     (try
                       (when (not (impl? k))
                         (let [vt (infer/instantiate v)]
                           (when (unify/unify t' vt)
                             k)))
                       (catch Throwable _)))))))))

(defn type-replace-expr-at-point [msg]
  (let [t? (type-search-expr-at-point msg)]
    (if (instance? Throwable t?)
      (throw t?)
      (first t?))))

(def ignore
  #{#'ns 'ns 'try #'type/def #'lift/interface #'lift/impl #'lift/data
    #'lift/with-ctor #'defmacro #'lifted-load})

(defn run-op [msg]
  (try
    ((ns-resolve 'lift.middleware (::op msg)) msg)
    (catch Throwable t
      (println "Run op encountered an Exception")
      (println t))))

(defn read-code [{:keys [op ns file-name expr-pos code] :as msg}]
  (when (seq code)
    (let [[line] expr-pos
          rdr   (some-> code
                        (cond->> line
                          (str (apply str (repeat (dec line) \newline))))
                        (java.io.StringReader.)
                        (rt/indexing-push-back-reader 1 file-name))]
      (if (= op "eval") (r/read rdr) code))))

(defn extension [filename]
  (let [i (.lastIndexOf filename ".")]
    (when (pos? i) (subs filename (inc i)))))

(defn dialect? [filename]
  (some->> filename extension (= "cljd")))

(defn decorate-dialect [{:keys [file-name ns] :as msg}]
  (try
    (cond-> msg
      (dialect? file-name)
      (assoc :dialect (or (ns/dialect ns)
                          (-> file-name rdr/read-namespace ns/analyze-dialect))))
    (catch Throwable _ msg)))

(def dialect-handler nil)
(defmulti dialect-handler
  (fn [handler {:keys [op dialect]}] [(keyword op) dialect]))

(defmethod dialect-handler :default [handler msg] (handler msg))

(defmethod dialect-handler [:eval :lift/clojure] [handler msg]
  (handler (assoc msg :eval `loader/eval)))

(defmethod dialect-handler [:load-file :lift/clojure] [handler msg]
  (binding [load-file/load-file-code loader/load-file-code]
    (handler (assoc msg :eval `c/eval))))

(defn lift-repl [handler]
  (fn [msg]
    (->> msg decorate-dialect (dialect-handler handler))))

(defn handles []
  (->> dialect-handler methods keys (filter vector?) (map (comp name first))))

(mw/set-descriptor!
 #'lift-repl
 {:requires #{"clone"} :expects (set (handles)) :handles {}})
