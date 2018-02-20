(ns lift.middleware
  (:refer-clojure :exclude [read read-string])
  (:require
   [clojure.tools.reader :as r]
   [clojure.tools.reader.reader-types :as rt :refer [read-char unread]]
   [clojure.tools.reader.impl.utils :refer [whitespace?]]
   [clojure.tools.nrepl.middleware :as mw]
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as cs]
   [clojure.string :as string]
   [clojure.java.io :as io]
   [lift.lang :as lift]
   [lift.lang.inference :as infer]
   [lift.lang.rewrite :as rewrite]
   [lift.lang.type :as type]
   [lift.lang.type.base :as base]
   [lift.lang.util :as u]
   [lift.lang.analyze :as ana]
   [lift.lang.type.impl :as impl]
   [lift.lang.signatures :as sig]
   [lift.lang.type.def :as def]
   [lift.tools.reader :as rdr]
   [lift.lang.unification :as unify])
  (:import
   [lift.lang.type.base Forall Literal Mark SyntaxNode Var]
   [lift.lang.type.impl Type]))

(def ^:dynamic *type-check* false)
(def ^:dynamic *prn-type* false)

(def ignore #{#'ns 'ns 'try #'type/def #'lift/data #'defmacro})

(defn ignore? [[op]]
  (and (symbol? op)
       (or (contains? ignore op)
           (contains? ignore (resolve op)))))

(deftype Ret [x t])

(defmethod print-method Ret [o w]
  (.write w (format "%s = %s" (pr-str (.-x o)) (pr-str (.-t o)))))

(defn toggle []
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

(defn check [expr]
  (infer/check (u/macroexpand-all expr)))

(defn sub-pretty-vars
  ([[f & ftvs] [v & vars]]
   (if f (assoc (sub-pretty-vars ftvs vars) f v) {}))
  ([ftvs]
   (-> (sort ftvs)
       (sub-pretty-vars (map (comp #(Var. %) symbol str char) (range 97 123)))
       (type/sub))))

(defn check
  ([expr] (check @type/type-env expr))
  ([env expr]
   (let [[s ast] ((impl/hylo infer/-infer-ann-err ana/parse expr) env)]
     [s (type/substitute ast s)])))

(defn def? [expr]
  (and (seq? expr) (= 'def (first expr))))

(defn type-check-error [[msg {:keys [file line column expr] :as x} :as infer-err]]
  (prn 'infer-err infer-err )
  (throw
   (clojure.lang.Compiler$CompilerException.
    (or file (pr-str expr))
    (or line 0)
    (or column 0)
    (Exception. msg))))

(defn lift [expr]
  (prn 'lift expr)
  (try
    (if (and (seq? expr) (ignore? expr))
      (c/eval expr)
      (let [code (u/macroexpand-all expr)
            [s [_ t err :as expr]] (check code)]
        (prn 'err err code expr)
        (if (seq err)
          (throw (type-check-error (first err)))
          (let [ftvs (base/ftv t)
                sub  (sub-pretty-vars ftvs)
                ret  (->> expr
                          (rewrite/rewrite @type/type-env s)
                          (rewrite/emit))
                t'   (type/substitute t sub)]
            (if (def? code)
              (let [name (second code)]
                (c/eval (list 'def name ret))
                (let [v (u/resolve-sym name)
                      sigma (Forall. (base/ftv t') t')]
                  (swap! type/type-env assoc v sigma)
                  (base/$ (resolve v) t')))
              (base/$ (c/eval ret) t'))))))
    (catch Throwable t
      (prn t)
      (throw t))))

(defn type-of-symbol [expr]
  (when-let [t (and (symbol? expr)
                    (get @type/type-env (u/resolve-sym expr)))]
    (base/$ (resolve expr) t)))

(defn type-of-type [expr]
  (try
    (when-let [t (get @type/type-env (def/type-signature expr))]
      (base/$ expr t))
    (catch Throwable _)))

(defn find-mark [x]
  (let [p (fn [x] (and (instance? SyntaxNode x) (-> x :m :mark true?)))
        a (atom nil)
        f (fn [x] (reset! a (base/$ (pr-str (:expr (:m x))) (:t x))))]
    (if (p x)
      (f x)
      (do
        (impl/cata (fn [x] (if (p x) (f x) x)) x)
        @a))))

(defn control? [x]
  (and (map? x) (contains? x ::op)))

(defn type-of-expr-at-point [{:keys [file]
                              {[_ _ t :as tp] :pos top  :code} :top
                              {[_ _ e :as ep] :pos expr :code} :expr}]
  (try
    (or (and (not (symbol? expr))
             (ana/literal? expr)
             (base/$ (Literal. expr) (ana/type (Literal. expr))))

        (type-of-symbol expr)

        (type-of-type expr)

        (let [expr (rdr/top-level-sexp top (- e t))
              code (u/macroexpand-all expr)
              [s [n t err :as expr]] (check code)]
          (let [ftvs (base/ftv t)
                sub  (sub-pretty-vars ftvs)
                t'   (type/substitute t sub)
                sigma (Forall. (base/ftv t') t')]
            (find-mark expr))))
    (catch Throwable t
      (prn t)
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
      (->> @type/type-env
           (keep (fn [[k v]]
                   (try
                     (when (and (not (impl? k)) (unify/unify (:t t?) (:t v)))
                       k)
                     (catch Throwable _))))))))

(defn type-replace-expr-at-point [msg]
  (let [t? (type-search-expr-at-point msg)]
    (prn (first t?))
    (if (instance? Throwable t?)
      (throw t?)
      (first t?))))

(defn run-op [msg]
  (prn ::op (::op msg))
  (try
    ((ns-resolve 'lift.middleware (::op msg)) msg)
    (catch Throwable t
      (println "Run op encountered an Exception")
      (println t))))

(defn read-code [{:keys [op ns file-name expr-pos code] :as msg}]
  (let [[line] expr-pos
        rdr   (some-> code
                      (cond->> line
                        (str (apply str (repeat (dec line) \newline))))
                      (java.io.StringReader.)
                      (rt/indexing-push-back-reader 1 file-name))]
    (if (= op "eval") [(r/read rdr)] code)))

(defn eval-handler [handler {:keys [op ns file-name expr-pos code] :as msg}]
  (let [lift? (some-> ns symbol find-ns meta :lang (= :lift/clojure))
        [line col] expr-pos
        code (read-code msg)]
    (if (control? (first code))
      (handler (assoc msg :eval `identity :code [(run-op (first code))]))
      (let [eval (case op "eval" `lift "load-file" `lift)]
        (if lift?
          (-> (assoc msg :eval eval :code code)
              (cond-> (= op "load-file") (dissoc :ns))
              (handler))
          (handler msg))))))

(defn repl-fn [handler {:keys [op code ns file column line] :as msg}]
  (cond
    (= "load-file" op) (#'eval-handler handler msg)
    (= "eval" op)      (#'eval-handler handler msg)
    (= "type" op)      (#'eval-handler handler msg)
    :else (handler msg)))

(defn repl [handler]
  (fn [msg]
    (#'repl-fn handler msg)))

(mw/set-descriptor! #'repl
  {:requires #{"clone"}
   :expects #{"eval" "load-file" "type"}
   :handles {}})
