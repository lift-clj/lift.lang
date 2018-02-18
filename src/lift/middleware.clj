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
   [lift.lang.inference :as infer]
   [lift.lang.rewrite :as rewrite]
   [lift.lang.type :as type]
   [lift.lang.type.base :as base]
   [lift.lang.util :as u]
   [lift.lang.analyze :as ana]
   [lift.lang.type.impl :as impl])
  (:import
   [lift.lang.type.base Forall SyntaxNode Var]))

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

(defn lift [expr]
  (try
    (let [code (u/macroexpand-all expr)
          [s [_ t err :as expr]] (check code)]
      (if err
        (throw
         (Exception. (str (pr-str expr) "\n"
                          (string/join "\n" err))))
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
                (SyntaxNode. (resolve v) t' nil)))
            (SyntaxNode. (c/eval ret) t' nil)))))))

(defn eval-handler [handler {:keys [ns code] :as msg}]
  (let [lift? (some-> ns symbol find-ns meta :lang (= :lift/clojure))]
    (if lift?
      (handler (assoc msg :eval `lift))
      (handler msg))))

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
