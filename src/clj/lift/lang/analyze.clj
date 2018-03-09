(ns lift.lang.analyze
  (:refer-clojure :exclude [type])
  (:require
   [clojure.core :as c]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [lift.f.functor :as f]
   [lift.lang.pattern :as p]
   [lift.lang.type.base :as base :refer [$]]
   [lift.lang.type.impl :refer [ana]]
   [lift.lang.util :refer [resolve-sym]]
   [lift.lang.util :as u])
  (:import
   [lift.lang.type.impl Type]))

(base/import-container-types)
(base/import-syntax-types)
(base/import-type-types)

(defn class-name? [x]
  (try
    (and (symbol? x) (class? (resolve x)))
    (catch ClassNotFoundException _)))

(defn mark? [x]
  (instance? Mark x))

(s/def ::literal
  (s/or :Boolean  boolean?
        :Int      integer?
        :Double   double?
        :Float    float?
        :String   string?
        :Class    class?
        :Symbol   symbol?
        :Keyword  keyword?
        :Char     char?))

(defn literal? [x]
  (s/valid? ::literal x))

(s/def ::def
  (s/and seq? #(= 'def (first %)) (s/coll-of any?)))

(s/def ::var
  (s/and symbol? (complement class-name?)))

(s/def ::arglist (s/coll-of ::var :kind vector?))

(s/def ::lambda
  (s/and seq? (s/cat :lamb #{'fn}
                     :bind ::arglist
                     :expr any?)))
(s/def ::fn*
  (s/and seq? (s/cat :fn* #{'fn* 'fn}
                     :ar1 (s/and seq? (s/cat :bind ::arglist :expr any?)))))

(s/def ::variadic
  (s/and seq?
         (s/cat :fn #{'fn}
                :vs (s/+ (s/and seq? (s/cat :bind ::arglist :expr any?))))))

(s/def ::record-selection
  (s/and seq? (s/cat :op (s/or :k keyword? :m mark?) :arg ::record-expr)))

(s/def ::record-extension
  (s/and seq? (s/cat :op #{'assoc}
                     :r ::record-expr
                     :kvs (s/+ (s/cat :l keyword? :a ::record-expr)))))

(s/def ::record-restriction
  (s/and seq? (s/cat :op #{'dissoc}
                     :r ::record-expr
                     :ls (s/+ keyword?))))

(s/def ::application
  (s/and seq? (s/cat :op #(not (= 'quote %)) :args (s/* any?))))

(s/def ::let
  (s/and seq?
         (s/cat :let  #{'let 'let*}
                :bind (s/and vector? (s/+ (s/cat :var ::var :expr any?)))
                :expr any?)))

(s/def ::if
  (s/and seq? (s/cat :if #{'if} :cond any? :then any? :else any?)))

(s/def ::list
  (s/or :quoted (s/and seq? #(= 'quote (first %)) (comp seq? second))
        :empty  (s/and seq? empty?)))

(s/def ::quoted
  (s/and seq? #(= 'quote (first %))))

(s/def ::vector vector?)

(s/def ::record
  (s/and (complement record?) (s/map-of keyword? any?)))

(s/def ::set set?)

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
        :Fn* ::fn*
        :Let ::let
        :If  ::if
        :Sel ::record-selection
        :Ext ::record-extension
        :Res ::record-restriction
        :App ::application
        :Lst ::list
        :Vec ::vector
        :Rec ::record
        :Ste ::set
        :Quo ::quoted))

(defn curry [op args]
  (if (seq args)
    (recur (Apply. op (first args)) (rest args))
    op))

(p/defn type
  ([[Literal a]]
   (Const. (-> a c/type .getSimpleName symbol)))
  ([x]
   (throw
    (Exception. (str "Cannot parse type of non-Literal: " (pr-str x))))))

(def -parse nil)
(defmulti -parse (fn [t expr] t))

(defmethod -parse :Lit [_ value]
  (Literal. value))

(defmethod -parse :Def [_ [_ _ expr]]
  expr)

(defmethod -parse :Var [_ expr]
  (Symbol. expr))

;; (defmethod -parse :VFn [_ [_ & vfns]]
;;   (Variadic.
;;    (mapv (fn [[arglist expr]]
;;            (Lambda. (Tuple. (map #(Symbol. %) arglist)) expr))
;;          vfns)))

(defn encode-args [strategy ctor args]
  (case strategy
    :curry (reduce (fn [a b] (ctor a b)) args)
    :tuple (ctor (Tuple. (vec (butlast args))) (last args))))

(defmethod -parse :Lam [_ [_ arglist expr]]
  (encode-args :curry
               #(Lambda. %2 %)
               (cons expr (map #(Symbol. %) (reverse arglist)))))

(defmethod -parse :Fn* [_ [_ [arglist expr]]]
  (encode-args :curry
               #(Lambda. %2 %)
               (cons expr (map #(Symbol. %) (reverse arglist)))))

(defmethod -parse :App [_ [op & args]]
  (encode-args :curry #(Apply. % %2) (cons op args)))

(defmethod -parse :Let [_ [_ bindings expr]]
  (->> bindings
       (partition 2)
       (reverse)
       (reduce (fn [expr [v x]] (Let. (Symbol. v) x expr)) expr)))

(defmethod -parse :If  [_ [_ cond then else]]
  (If. cond then else))

(defmethod -parse :Lst [_ expr]
  (if (= 'quote (first expr))
    (List. (second expr))
    (List. expr)))

(defmethod -parse :Vec [_ expr] (Vector. expr))

(defmethod -parse :Rec [_ expr] (Map. expr))

(defmethod -parse :Set [_ expr] (Set. expr))

(defmethod -parse :Sel [_ [op arg]]
  (Select. op arg))

(defmethod -parse :Ext [_ [op r & kvs]]
  (reduce (fn [r [l a]]
            (-> op
                (Apply. r)
                (Apply. (Key. l))
                (Apply. a)))
          r
          (partition 2 kvs)))

(defmethod -parse :Res [_ [op r & ls]]
  (reduce (fn [r l] (Restrict. l r)) r ls))

(defmethod -parse :Fun [_ f] f)

(defmethod -parse :Quo [_ [_ expr]]
  (Quoted. expr))

(defn parse [expr]
  (let [conformed (s/conform ::expr expr)]
    (cond (s/invalid? conformed)
          (if (instance? lift.lang.type.impl.Type expr)
            expr
            (throw (ex-info (format "Invalid Syntax: %s" (pr-str expr))
                            (s/explain-data ::expr expr))))
          (= :Def (first conformed))
          (ana parse (nth expr 2))
          :default
          ($ (-parse (first conformed) expr)
             nil
             nil
             (-> expr meta (assoc :expr expr))))))
