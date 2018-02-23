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
   [clojure.walk :as walk]
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
   [lift.lang.type.base Forall Let Literal Mark Prim SyntaxNode Var]
   [lift.lang.type.impl Type]))

(def ^:dynamic *type-check* true)
(def ^:dynamic *prn-type* false)

(def ignore
  #{#'ns 'ns 'try #'type/def #'lift/interface #'lift/impl #'lift/data
    #'lift/with-ctor #'defmacro})

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
  ([expr] (check @type/type-env expr))
  ([env expr]
   (let [[s ast] ((impl/hylo infer/-infer-ann-err ana/parse expr) env)]
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

(defn lift [top-level-expr]
  (try
    (if (or (not *type-check*)
            (and (seq? top-level-expr) (ignore? top-level-expr)))
      (c/eval top-level-expr)
      (let [code (u/macroexpand-all top-level-expr)
            [s [_ t err :as expr]] (check code)]
        (if (seq err)
          (throw (type-check-error (first err) (meta top-level-expr)))
          (let [ftvs (base/ftv t)
                sub  (sub-pretty-vars ftvs)
                ret  (->> expr
                          (rewrite/rewrite @type/type-env s)
                          (rewrite/emit))
                t'   (type/substitute t sub)]
            (if (def? code)
              (let [name (second code)]
                (c/eval top-level-expr)
                (let [v     (u/resolve-sym name)
                      sig   (type/get-type @type/type-env v)
                      _ (prn v (type v) sig)
                      sigma (Forall. (base/ftv t') t')
                      _ (prn v sigma)
                      sig   (if sig
                              (do (unify/unify (infer/instantiate sig)
                                               (infer/instantiate sigma))
                                  sig)
                              sigma)]
                  (prn sig)
                  (swap! type/type-env assoc v sig)
                  (base/$ (resolve v) sig)))
              (let [ret' (pr-str (c/eval ret))]
                (base/$ ret' t')))))))
    (catch Throwable t
      (throw t))))

(defn type-of-symbol [ns expr]
  (when-let [t (and (symbol? expr)
                    (get @type/type-env (u/resolve-sym ns expr)))]
    (base/$ (ns-resolve ns expr) t)))

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
        (impl/cata (fn [x]
                     (cond (p x)
                           (f x)
                           (and (instance? Let x)
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
            (find-mark expr))))
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
      (->> @type/type-env
           (keep (fn [[k v]]
                   (try
                     (when (and (not (impl? k)) (unify/unify (:t t?) (:t v)))
                       k)
                     (catch Throwable _))))))))

(defn type-replace-expr-at-point [msg]
  (let [t? (type-search-expr-at-point msg)]
    (if (instance? Throwable t?)
      (throw t?)
      (first t?))))

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

(defn eval-handler [handler {:keys [op ns file-name expr-pos code] :as msg}]
  (let [lift? (some-> ns symbol find-ns meta :lang (= :lift/clojure))
        [line col] expr-pos
        code' (read-code msg)]
    (if (control? code')
      (handler
       (assoc msg
              :eval (symbol "lift.middleware" (name (::op code')))
              :code [(assoc code' :ns (symbol ns))]))
      (let [eval (case op "eval" `lift "load-file" `lift)]
        (if lift?
          (-> (assoc msg :eval eval :code [code'])
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
