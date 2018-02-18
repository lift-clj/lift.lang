;; (ns tools
;;   (:require
;;    check parse type
;;    [util :refer [fmap]]
;;    [clojure.tools.reader :as r]
;;    [clojure.tools.reader.reader-types :as rt :refer [to-pbr]]
;;    [clojure.walk :as walk]
;;    [clojure.string :as string]
;;    [check :as check]))

;; (def ^:dynamic *type-check* false)
;; (def ^:dynamic *prn-type* false)

;; (deftype Ret [x t])

;; (defmethod print-method Ret [o w]
;;   (.write w (format "%s = %s" (pr-str (.-x o)) (pr-str (.-t o)))))

;; (defn toggle []
;;   (Ret. '*type-check* (alter-var-root #'*type-check* not)))

;; (defn infer [parsed]
;;   (check/infer @type/type-env (parse/parse (walk/macroexpand-all parsed))))

;; (defn typed-ast [parsed-ast]
;;   (let [[s ast] (infer parsed-ast)]
;;     (type/walk ast (fn [x]
;;                      (cond-> x (:type x) (update :type type/substitute s))))))

;; (defn check* [env expr]
;;   (let [[s ast] (->> expr parse/parse (check/infer env))]
;;     (-> ast
;;         (type/walk (fn [x]
;;                      (cond-> x (:type x) (update :type type/substitute s)))))))

;; (defn check [expr]
;;   (let [env @type/type-env
;;         src (walk/macroexpand-all expr)]
;;     (or (when (seq? src)
;;           (let [[op & [name? expr? :as args]] src]
;;             (when (= 'def op)
;;               (let [n  (util/resolve-sym name?)
;;                     ts (get env n)
;;                     t  (:t ts)
;;                     ast (check* env expr?)
;;                     t'  (:type ast)]
;;                 (if (and t (not (:auto (meta ts))))
;;                   (do (check/unify t t') t)
;;                   (do
;;                     (swap! type/type-env
;;                            assoc
;;                            n
;;                            (with-meta (->> t'(type/ftv) (type.Scheme. t'))
;;                              {:auto true}))
;;                     t'))))))
;;         (:type (check* env src)))))

;; (declare ignore)

;; (defn ignore? [[op]]
;;   (and (symbol? op) (contains? ignore (resolve op))))

;; (defmacro try-check [& forms]
;;   `(try
;;      ~@forms
;;      (catch clojure.lang.ExceptionInfo e#
;;        (let [{:keys [~'type] :as data#} (-> (ex-data e#)
;;                                          (assoc :type-error (.getMessage e#)))]
;;          (case ~'type
;;            :unification-failure
;;            (-> data#
;;                (update :a type/explode)
;;                (update :b type/explode))
;;            :unbound-variable
;;            (-> data#
;;                (update :value type/explode)))))
;;      (catch Throwable e# e#)))

;; (deftype TRet [x t])

;; (defmethod print-method TRet [o w]
;;   (.write w (format "%s : %s" (pr-str (.-x o)) (pr-str (.-t o)))))

;; (defn check-eval [expr]
;;   (if (and (seq? expr) (ignore? expr))
;;     (eval expr)
;;     (try-check
;;      (if (and *type-check* (-> *ns* meta :type-check))
;;        (let [t    (check expr)
;;              tret (TRet. (eval expr) t)]
;;          (when *prn-type*
;;            (prn tret))
;;          tret)
;;        (eval expr)))))

;; (defn typed? [expr]
;;   (cond (seq? expr)
;;         (let [[op] expr]
;;           (cond (= 'fn op)
;;                 (typed? (nth expr 2))
;;                 (= 'fn* op)
;;                 (typed? (second (first expr)))
;;                 (symbol? op)
;;                 (->> op util/resolve-sym (contains? @type/type-env))
;;                 :else (typed? op)))
;;         (symbol? expr)
;;         (->> expr util/resolve-sym (contains? @type/type-env))
;;         (map-entry? expr)
;;         (typed? (val expr))
;;         (coll? expr)
;;         (some? (first (keep typed? expr)))
;;         :else false))

;; ;; (typed? '(fn [x] (something-123 (get {} :x))))

;; ;; (second (first [[1 2] [2]]))

;; ;; (defn calling-in?
;; ;;   "`expr` starts in an untyped world, and calls a function in the typed world,
;; ;;   with untyped arguments."
;; ;;   [expr]
;; ;;   (if (typed? expr)
;; ;;     false

;; ;;     ))

;; ;; (defn calling-out?
;; ;;   "`expr` starts in a typed world, and calls a function in the untyped world."
;; ;;   [expr]
;; ;;   (if (typed? expr)
;; ;;     (walk-any? (complement typed?) expr)
;; ;;     false))

;; (defn t [expr-info]
;;   (let [{:keys [expr]} (read-with-meta (:expr expr-info))]
;;     (or (:t (or (get @type/type-env (when (symbol? expr) (util/resolve-sym expr)))
;;                 (try
;;                   (get @type/type-env (type/type-signature expr))
;;                   (catch Throwable _))))
;;         (let [{:keys [expr top-level]} expr-info
;;               [expr topx] (unify-position expr top-level)]
;;           (if (= expr topx)
;;             (:type (typed-ast (:expr topx)))
;;             (try-check
;;              (let [t-ast (typed-ast (:expr topx))]
;;                (or (-> t-ast
;;                        (type/walk (fn [x]
;;                                     (cond (found? expr x)
;;                                           (reduced (:type x))
;;                                           (and (not (symbol? (:expr x)))
;;                                                (= (:expr expr) (:expr x)))
;;                                           (reduced (:type x))
;;                                           :else
;;                                           (if (map? x)
;;                                             (first (filter reduced? (vals x)))))))
;;                        (unreduced))
;;                    (prn 'default)
;;                    (:type (typed-ast (:expr expr)))))))))))

;; (def ignore #{#'type/def #'t #'type/data #'time #'defmacro})

;; (defn spy [f]
;;   (fn [& args]
;;     (prn args)
;;     (apply f args)))

;; (defn type-search [t]
;;   (->> @type/type-env
;;        (keep (fn [[k v]]
;;                (try
;;                  (when (and (and (not= "type" (namespace k))
;;                                  (not (.startsWith "proj-" (name k))))
;;                             (check/unify t (:t v))) k)
;;                  (catch Throwable _))))
;;        (remove #{'type/unmatched-case-error})))
