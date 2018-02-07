(ns lift.lang.analyze
  (:refer-clojure :exclude [case type])
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [lift.lang.type :refer :all]
   [lift.lang.util :refer [resolve-sym]]
   [lift.f.functor :as f]
   [clojure.set :as set]
   [lift.lang.pattern :as p])
  (:import
   [lift.lang.type Apply Arrow Const Container If Lambda Let
    Literal Map Predicate Predicated Symbol Tuple Var Variadic Vector]))

;; (extend-protocol f/Functor
;;   ;; overriding the incorrect impl by lift.f.functor
;;   )

(defn cata [f x]
  (f (f/map #(cata f %) x)))

(defn ana [f x]
  (f/map #(ana f %) (f x)))

(defn hylo [f g x]
  (f (f/map #(hylo f g %) (g x))))

(defn prim? [x]
  (and (simple-symbol? x) (contains? @type-env x)))

(defn class-name? [x]
  (and (simple-symbol? x) (not (prim? x)) (class? (resolve x))))

(s/def ::literal
  (s/or :Boolean  boolean?
        :Num      (s/or :i integer? :d double? :f float?)
        :String   string?
        :Class    class-name?
        :Symbol   symbol?
        :Keyword  keyword?
        :Char     char?))

(s/def ::def
  (s/and seq? #(= 'def (first %)) (s/coll-of any?)))

(s/def ::var
  (s/and symbol? (complement prim?) (complement class-name?)))

(s/def ::arglist (s/coll-of ::var :kind vector?))

(s/def ::lambda
  (s/and seq? (s/cat :lamb #{'fn}
                     :bind ::arglist
                     :expr any?)))

(s/def ::variadic
  (s/and seq?
         (s/cat :fn #{'fn}
                :vs (s/+ (s/and seq? (s/cat :bind ::arglist :expr any?))))))

(s/def ::record-selection
  (s/and seq? (s/cat :op keyword? :arg ::record-expr)))

(s/def ::record-extension
  (s/and seq? (s/cat :op #{'assoc} :r ::record-expr :l keyword? :a ::record-expr)))

(s/def ::record-restriction
  (s/and seq? (s/cat :op #{'dissoc} :r ::record-expr :l keyword?)))

(s/def ::application
  (s/and seq? (s/cat :op any? :args (s/* any?))))

(s/def ::let
  (s/and seq?
         (s/cat :let  #{'let}
                :bind (s/and vector? (s/+ (s/cat :var ::var :expr any?)))
                :expr any?)))

(s/def ::if
  (s/and seq? (s/cat :if #{'if} :cond any? :then any? :else any?)))

(s/def ::vector vector?)

(s/def ::record
  (s/and (complement record?) (s/map-of keyword? any?)))

(s/def ::record-expr
  (s/or :Var ::var
        :Lit ::literal
        :Let ::let
        :If  ::if
        :Sel ::record-selection
        :Ext ::record-extension
        :Res ::record-restriction
        :App ::application
        :Rec ::record))

(s/def ::expr
  (s/or :Def ::def
        :Var ::var
        :Lit ::literal
        :VFn ::variadic
        :Lam ::lambda
        :Let ::let
        :If  ::if
        :Sel ::record-selection
        :Ext ::record-extension
        :Res ::record-restriction
        :App ::application
        :Vec ::vector
        :Rec ::record))

(defn curry [op args]
  (if (seq args)
    (recur (Apply. op (first args)) (rest args))
    op))

(p/defn type
  ([[Literal a]]
   (let [k (->> a (s/conform ::literal) first name (symbol "lift"))]
     (c/case k
       lift/Num (Predicated. [(Predicate. k (Var. 'a))] (Var. 'a))
       (Const. k))))
  ([x]
   (throw
    (Exception. (str "Cannot parse type of non-Literal: " x)))))

(def -parse nil)
(defmulti -parse (fn [t expr] t))

(defmethod -parse :Lit [_ value]
  (Literal. value))

(defmethod -parse :Def [_ [_ _ expr]]
  expr)

(defmethod -parse :Var [_ expr]
  (Symbol. expr))

(defmethod -parse :VFn [_ [_ & vfns]]
  (Variadic.
   (mapv (fn [[arglist expr]]
           (Lambda. (Tuple. (map #(Symbol. %) arglist)) expr))
         vfns)))

(defmethod -parse :Lam [_ [_ arglist expr]]
  (Lambda. (Tuple. (map #(Symbol. %) arglist)) expr))

(defmethod -parse :App [_ [op & args]]
  (Apply. op (Tuple. (vec args))))

(defmethod -parse :Let [_ [_ bindings expr]]
  (->> bindings
       (partition 2)
       (reverse)
       (reduce (fn [expr [v x]] (Let. (Symbol. v) x expr)) expr)))

(defmethod -parse :If  [_ [_ cond then else]]
  (If. cond then else))

(defmethod -parse :Vec [_ expr] (Vector. expr))

(defmethod -parse :Rec [_ expr] (Map. expr))

;; (defmethod -parse :Sel [_ [op arg]]
;;   (Select. arg (-> (Literal. op) (assoc :type (Const. op)))))

(defmethod -parse :Ext [_ [op r l a]]
  (Apply. op [r l a]))

(defmethod -parse :Res [_ [op r l]]
  (Apply. op [r l]))

(defn parse [expr]
  (let [conformed (s/conform ::expr expr)]
    (if (s/invalid? conformed)
      (if (record? expr)
        expr
        (throw (Exception. (str "Invalid Syntax"
                                (with-out-str (s/explain ::expr expr))))))
      (let [x (-parse (first conformed) expr)]
        (cond-> x (record? x) (assoc :expr expr))))))

;; (->> '(fn
;;         ([a] [a])
;;         ([a b] [a b])
;;         ([a b c] [a b c]))
;;      (ana ; (fn [expr] (fn [env] (check/infer expr env)))
;;            parse)
;;     ;; (#(% check/empty-env))
;;     ;; second
;;     ;; :type
;;  )

;; (->> '[1 2]
;;      (hylo (fn [expr] (fn [env] (check/infer expr env)))
;;            parse)
;;      (#(% (assoc check/empty-env :type @type-env)))
;;      second
;;      :type
;;      )
