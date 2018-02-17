(ns lift.middleware
  (:refer-clojure
   :exclude
   [compile defmacro load load-file macroexpand macroexpand-1 read read-string])
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
   [riddley.walk :as walk]
   [lift.lang.inference :as infer]
   [lift.lang.rewrite :as rewrite]
   [lift.lang.type :as type]
   [lift.lang.type.base :as base])
  (:import
   [lift.lang.type.base SyntaxNode Var]))

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
  (infer/check (walk/macroexpand-all expr)))

(defn sub-pretty-vars
  ([[f & ftvs] [v & vars]]
   (if f (assoc (sub-pretty-vars ftvs vars) f v) {}))
  ([ftvs]
   (-> (seq ftvs)
       (sub-pretty-vars (map (comp #(Var. %) symbol str char) (range 97 123)))
       (type/sub))))

(defn lift [expr]
  (try
    (let [[_ t :as expr] (-> expr walk/macroexpand-all infer/check)
          ftvs (base/ftv t)
          sub  (sub-pretty-vars ftvs)
          t'   (type/substitute t sub)]
      (->> expr
           (rewrite/rewrite @type/type-env)
           (rewrite/emit)
           (c/eval)
           (#(SyntaxNode. % t'))))
    (catch clojure.lang.ExceptionInfo e
      (prn (ex-data e))
      e)))

(defn parse [expr]
  ;;(analyze/parse expr)
  expr
  ) ;; -> AST

(defn check [ast] ast) ;; -> AST

(defn rewrite [ast]
  ast) ;; -> AST

(defn reform [ast] ast) ;; -> SExpr
;; (def ext "clj")
;; (def sep java.io.File/separator)

(defn resolve-sym [sym]
  (let [sym-ns (some-> sym namespace symbol)
        ns     (if sym-ns
                 (or (some-> *ns* ns-aliases (get sym-ns)) sym-ns)
                 *ns*)]
    (symbol (name (.getName ns)) (name sym))))

(def macro-env (atom {}))

(c/defmacro defmacro [name arglist expr]
  `(swap! macro-env assoc
          '~(resolve-sym name)
          (fn ~name [~'&form ~'&env ~'&type-env ~@arglist] ~expr)))

(declare load-ns)

(defmacro ns [name meta require-expr]
  `(do
     (in-ns ~name)
     ~(when meta `((.resetMeta (clojure.lang.Namespace/find '~name) ~meta)))
     ~@(if (= :require (first require-expr))
         (map load-ns (rest require-expr))
         (throw (Exception. "`require-expr` must be `:require`")))))

(defn macro? [expr]
  (and (symbol? expr) (contains? macro-env expr)))

(defn resolve-macro [sym]
  (get @macro-env (resolve-sym sym)))

(defn macroexpand-1 [expr]
  (or (when (seq? expr)
        (let [[op & args] expr]
          (when-let [macro-fn (resolve-macro op)]
            (apply macro-fn expr nil nil args))))
      expr))

(defn macroexpand [expr]
  (let [expr' (macroexpand-1 expr)]
    (if (= expr expr')
      expr
      (recur expr'))))

(macroexpand-1 '(ns user
                  {:lang :lift/clojure}
                  (:require )
                  ))

(resolve-macro 'ns)

(defn macroexpand-all [x]
  (walk/walk-exprs (constantly false) nil #{'ns} x))

(def EOF (Object.))
(defn- -load [resource]
  (with-open [r (-> resource io/reader rt/indexing-push-back-reader)]
    (loop []
      (let [expr (read r false EOF)]
        (when-not (= expr EOF)
          (prn expr)
          (let [expr'  (macroexpand-all expr)
                _      (prn expr')
                ast    (parse expr')
                _      (prn ast)
                typed  (check ast)
                _      (prn typed)
                ast'   (rewrite typed)
                expr'' (reform ast')
                value  (c/eval expr'')]
            (recur)))))))


(defn ns-file-path
  [ns-sym]
  (-> ns-sym name (.replace \. \/) (.replace \- \_) (str ".cljd")))

(defn load-ns [ns-sym]
  (-load (-> ns-sym ns-file-path io/resource)))

;; (defn find-ns [sym]
;;   (clojure.lang.Namespace/find sym))
;; clojure.tools.nrepl.middleware.interruptible-eval


(defn load-file [path])

(defn load-ns [ns])

(defn compile-expr [expr])

(defn compile-ns [ns])
;; Compile and load both read the file form by form, and apply eval/emit to each.

;; (update :code (comp list read-string))
(defn eval-handler [handler {:keys [ns code] :as msg}]
  (let [lift? (some-> ns symbol find-ns meta :lang (= :lift/clojure))]
    (if lift?
      (handler (assoc msg :eval `lift))
      (handler msg))))

;; (defn load-file-handler [handler msg]
;;   ()
;;   )
;; (defn wrap-load-file
;;   "Middleware that evaluates a file's contents, as per load-file,
;;    but with all data supplied in the sent message (i.e. safe for use
;;    with remote REPL environments).

;;    This middleware depends on the availability of an :op \"eval\"
;;    middleware below it (such as interruptible-eval)."
;;   [h]
;;   (fn [{:keys [op file file-name file-path transport] :as msg}]
;;     (if (not= op "load-file")
;;       (h msg)
;;       (h (assoc (dissoc msg :file :file-name :file-path)
;;            :op "eval"
;;            :code ((if (thread-bound? #'load-file-code)
;;                     load-file-code
;;                     load-large-file-code)
;;                   file file-path file-name)
;;            :transport (reify Transport
;;                         (recv [this] (.recv transport))
;;                         (recv [this timeout] (.recv transport timeout))
;;                         (send [this resp]
;;                           ; *ns* is always 'user' after loading a file, so
;;                           ; *remove it to avoid confusing tools that assume any
;;                           ; *:ns always reports *ns*
;;                           (.send transport (dissoc resp :ns))
;;                           this)))))))

;; clojure.tools.nrepl.middleware.load-file

;; (defn ^{:dynamic true} load-file-code
;;   "Given the contents of a file, its _source-path-relative_ path,
;;    and its filename, returns a string of code containing a single
;;    expression that, when evaluated, will load those contents with
;;    appropriate filename references and line numbers in metadata, etc.

;;    Note that because a single expression is produced, very large
;;    file loads will fail due to the JVM method size limitation.
;;    In such cases, see `load-large-file-code'`."
;;   [file file-path file-name]
;;   (apply format
;;     "(clojure.lang.Compiler/load (java.io.StringReader. %s) %s %s)"
;;     (map (fn [item]
;;            (binding [*print-length* nil
;;                      *print-level* nil]
;;              (pr-str item)))
;;          [file file-path file-name])))

(defn repl-fn [handler {:keys [op code ns file column line] :as msg}]
  (cond
    (= "load-file" op) (eval-handler handler msg)
    (= "eval" op)      (eval-handler handler msg)
    :else (handler msg)))

(defn repl [handler]
  (fn [msg]
    (#'repl-fn handler msg)))

(mw/set-descriptor! #'repl
  {:requires #{"clone"} :expects #{"eval" "load-file"} :handles {}})
