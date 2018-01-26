(ns lift.lang.analyze
  (:refer-clojure :exclude [case])
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [lift.lang.check :as check]
   [lift.lang.type :refer :all]
   [lift.lang.util :refer [resolve-sym]]
   [lift.f.functor :as f]
   [clojure.set :as set])
  (:import
   [lift.lang.type Apply Arrow Const Container Extend If Lambda Let Literal
    Quantified Restrict Select Symbol Var]))

(extend-protocol f/Functor
  Object
  (f/-map [x f] x)
  ;; overriding the incorrect impl by lift.f.functor
  clojure.lang.IFn
  (f/-map [x f] x)
  clojure.lang.AFn
  (f/-map [x f] (comp f x))
  clojure.lang.ISeq
  (f/-map [x f] (map f x)))

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

(s/def ::lambda
  (s/and seq? (s/cat :lamb #{'fn}
                     :bind (s/coll-of ::var :kind vector?)
                     :expr any?)))

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

(s/def ::record (s/map-of keyword? any?))

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

(def -parse nil)
(defmulti -parse (fn [t expr] t))

(defmethod -parse :Lit [_ value]
  (let [k (->> value (s/conform ::literal) first name symbol)
        t (c/case k
            Num (Quantified. (Container. k [(Var. 'a)]) (Var. 'a))
            (Const. k))]
    (-> (Literal. value)
        (assoc :type t))))

(defmethod -parse :Def [_ [_ _ expr]]
  expr)

(defmethod -parse :Var [_ expr]
  (Symbol. expr))

(defmethod -parse :Lam [_ [_ arglist expr]]
  (reduce (fn [e x] (Lambda. (Symbol. x) e))
          expr
          (reverse arglist)))

(defmethod -parse :App [_ [op & args]]
  (reduce (fn [e1 e2] (Apply. e1 e2)) op args))

(defmethod -parse :Let [_ [_ bindings expr]]
  (->> bindings
       (partition 2)
       (reverse)
       (reduce (fn [expr [v x]] (Let. (Symbol. v) x expr)) expr)))

(defmethod -parse :If  [_ [_ cond then else]]
  (If. cond then else))

(defmethod -parse :Vec [_ expr] expr)

(defmethod -parse :Rec [_ expr] expr)

(defmethod -parse :Sel [_ [op arg]]
  (Select. arg (-> (Literal. op) (assoc :type (Const. op)))))

(defmethod -parse :Ext [_ [op r l a]]
  (Apply. op [r l a]))

(defmethod -parse :Res [_ [op r l]]
  (Apply. op [r l]))

(defn parse [expr]
  (let [conformed (s/conform ::expr expr)]
    (if (s/invalid? conformed)
      (do
        (s/explain ::expr expr)
        (throw (Exception. "Invalid Syntax")))
      (let [x (-parse (first conformed) expr)]
        (cond-> x (record? x) (assoc :expr expr))))))

(->> '(if true 2 2.9)
     (hylo (fn [expr] (fn [env] (check/infer expr env)))
           parse)
    (#(% check/empty-env))
    second
    :type
 )
