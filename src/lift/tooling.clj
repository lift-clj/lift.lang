(ns lift.tooling
  (:refer-clojure :exclude [compile defmacro load load-file macroexpand
                            macroexpand-1 read read-string])
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
   [lift.lang.analyze :as analyze]
   [riddley.walk :as walk]))

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

(defn parse [expr]
  (analyze/parse expr)) ;; -> AST

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

;; (load-ns 'user)

;; (io/resource "lift/lang.clj")

;; (defn require [& args]
;;   (doseq [[t x] (s/conform (s/+ ::cs/libspec) args)]
;;     (case t
;;       :lib nil
;;       :lib+opts
;;       (let [{:keys [lib options]} x]

;;         )
;;       )
;;     )
;;   ;; load libs, wrap with type/spec contracts
;;   )

;; (s/def ::ns-clauses
;;   (s/* ::cs/ns-require))

;; (s/def ::ns-form
;;   (s/cat :name simple-symbol?
;;          :docstring (s/? string?)
;;          :attr-map (s/? map?)
;;          :clauses ::ns-clauses))

;; (s/fdef lift.lang/ns
;;   :args ::ns-form)

;; (s/conform ::ns-form '(in-lift.user (:require [in-lift.user :as u])))
;; {:name in-lift.user,
;;  :clauses
;;  [{:clause :require,
;;    :body
;;    [[:libspec [:lib+opts {:lib in-lift.user, :options {:as u}}]]]}]}

;; (defmacro lift-ns
;;   {:style/indent :defn
;;    :arglists '([name docstring? attr-map? references*])}
;;   [& args]
;;   (let [{:keys [name docstring attr-map clauses]} (s/conform ::ns-form args)
;;         process-reference (fn [{:keys [clause body]}]
;;                             `(~(symbol "lift.lang" (c/str \- (c/name clause)))
;;                               ~@(map #(list 'quote %) body)))]
;;     `(do
;;        (clojure.core/in-ns '~name)
;;        ~(when-let [m (meta name)]
;;           `(.resetMeta (clojure.lang.Namespace/find '~name) ~m))
;;        ~@(map process-reference clauses)
;;        )
;;     )
;; )

;; (lift-ns in-lift.user
;;   (:require [in-lift2.user :as u]))

(defn lang [x]
  (if (= x 'lift/clojure)
    (do
      ;; TODO: set something here!!
      ;; (doseq [[sym v] (ns-map *ns*)] (ns-unmap *ns* sym))
      ;; (intern *ns* 'require #'require)
      ;; (intern *ns* 'tdef #'tdef)
      ;; (patch-reader-macros!)
        ''lift/clojure)
    (throw (IllegalArgumentException. (format "Unknown #lang %s" x)))))


;; (defn find-ns [sym]
;;   (clojure.lang.Namespace/find sym))
;; clojure.tools.nrepl.middleware.interruptible-eval


(defn load-file [path])

(defn load-ns [ns])

(defn compile-expr [expr])

(defn compile-ns [ns])
;; Compile and load both read the file form by form, and apply eval/emit to each.

(defn eval-handler [handler {:keys [ns code] :as msg}]
  (let [lift? (some-> ns symbol find-ns meta :lang (= :lift/clojure))]
    (when lift?
      (prn (symbol ns))
      (prn (-> msg
               (assoc :eval (comp c/eval reform rewrite check parse))
               (update :code read-string)
               :code))
      ;; (handler msg))
      (handler (-> msg
                   (assoc :eval `prn)
                   (update :code (comp list read-string)))))
    (handler msg)))

(defn repl-fn [handler {:keys [op code ns file column line] :as msg}]
  (cond
    (= "load-file" op) (handler msg)
    (= "eval" op)      (eval-handler handler msg)
    :else (handler msg)))

(defn repl [handler]
  (fn [msg]
    (#'repl-fn handler msg)))

(mw/set-descriptor! #'repl
  {:requires #{"clone"} :expects #{"eval" "load-file"} :handles {}})
