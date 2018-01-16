(ns lift.lang.analyze
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [lift.lang.type :refer :all]
   [lift.lang.util :refer [resolve-sym]]
   [clojure.set :as set])
  (:import
   [lift.lang.type
    Apply Arrow Extend If Lambda Let Literal Restrict Select Symbol]))

(defn prim? [x]
  (and (simple-symbol? x) (contains? @type-env x)))

(defn class-name? [x]
  (and (simple-symbol? x) (not (prim? x)) (class? (resolve x))))

(s/def ::literal
  (s/or :Boolean  boolean?
        :Int      integer?
        :String   string?
        :Class    class-name?
        :Symbol   symbol?
        :Keyword  keyword?
        :Char     char?))

(s/def ::def
  (s/and seq? #(= 'def (first %)) (s/coll-of ::expr)))

(s/def ::var
  (s/and symbol? (complement prim?) (complement class-name?)))

(s/def ::lambda
  (s/and seq?
         (s/or :fn (s/cat ::lamb #{'fn}
                          ::bind (s/coll-of ::var :kind vector?)
                          ::expr ::expr)
               :fn* (s/cat ::fn* #{'fn*}
                           ::arys (s/+ (s/and seq?
                                              (s/cat ::bind (s/coll-of ::var
                                                                       :kind vector?)
                                                     ::expr ::expr)))))))

(s/def ::record-selection
  (s/and seq? (s/cat :op keyword? :arg ::record-expr)))

(s/def ::record-extension
  (s/and seq? (s/cat :op #{'assoc} :r ::record-expr :l keyword? :a ::record-expr)))

(s/def ::record-restriction
  (s/and seq? (s/cat :op #{'dissoc} :r ::record-expr :l keyword?)))

(s/def ::application
  (s/and seq? (s/cat ::op ::expr ::args (s/* ::expr))))

(s/def ::let
  (s/and seq?
         (s/cat ::let #{'let 'let*}
                ::bind (s/and vector?
                              (s/cat :bindings (s/+ (s/cat :var ::var
                                                           :expr ::expr))))
                ::expr ::expr)))

(s/def ::if
  (s/and seq?
         (s/cat ::if #{'if}
                ::cond ::expr
                ::then ::expr
                ::else ::expr)))

(s/def ::vector
  (s/coll-of ::expr :kind vector?))

(s/def ::record (s/map-of ::expr ::expr))

(s/def ::record-expr
  (s/or ::Var ::var
        ::Lit ::literal
        ::Let ::let
        ::If  ::if
        ::Sel ::record-selection
        ::Ext ::record-extension
        ::Res ::record-restriction
        ::App ::application
        ::Rec ::record))

(s/def ::expr
  (s/or ::Def ::def
        ::Var ::var
        ::Lit ::literal
        ::Lam ::lambda
        ::Let ::let
        ::If  ::if
        ::Sel ::record-selection
        ::Ext ::record-extension
        ::Res ::record-restriction
        ::App ::application
        ::Vec ::vector
        ::Rec ::record))

(defn curry [op args]
  (if (seq args)
    (recur (Apply. op (first args)) (rest args))
    op))

(defmulti parse* (fn [[t _] expr] t))

(defmethod parse* ::Lit [[_ [h & more]] expr]
  (-> (Literal. (-> h name symbol))
      (assoc :expr expr)))

(defmethod parse* ::Def [[_ [_ sym e]] expr]
  (parse* e (nth expr 2)))

(defmethod parse* ::Var [[_ node] expr]
  (-> (Symbol. node)
      (assoc :expr expr)))

(defmethod parse* ::Lam [[_ [t node]] expr]
  (letfn [(parse-x [x] (assoc (Symbol. x) :expr x))
          (curry-fn [bindings pexpr expr]
            (let [[x & rest] (reverse bindings)]
              (reduce (fn [f x]
                        (Lambda. (parse-x x) f))
                      (Lambda. (parse-x x) (parse* pexpr expr))
                      rest)))]
    (c/case t
      :fn  (curry-fn (::bind node) (::expr node) (nth expr 2))
      :fn* (let [[a1] (::arys node)]
             (curry-fn (::bind a1) (::expr a1) (second (second expr)))))))

(defmethod parse* ::App [[_ node] expr]
  (-> (curry (parse* (::op node) (first expr))
             (map parse* (::args node) (rest expr)))
      (assoc :expr expr)))

(defmethod parse* ::Let [[_ node] expr]
  (-> (->> (take-nth 2 (rest (second expr)))
           (interleave (:bindings (::bind node)))
           (partition 2)
           (reverse)
           (reduce (fn [expr' [{:keys [var expr]} bind-val]]
                     (Let. (assoc (Symbol. var) :expr var)
                           (parse* expr bind-val) expr'))
                   (parse* (::expr node) (nth expr 2))))
      (assoc :expr expr)))

(defmethod parse* ::If  [[_ node] expr]
  (-> (If. (parse* (::cond node) (second expr))
           (parse* (::then node) (nth expr 2))
           (parse* (::else node) (nth expr 3)))
      (assoc :expr expr)))

(defmethod parse* ::Vec  [[_ node] expr]
  (reduce (fn [init x]
            (curry (assoc (Symbol. 'conj) :expr 'conj) [init x]))
          (assoc (Symbol. 'vector) :expr [])
          (map parse* node expr)))

(defmethod parse* ::Rec  [[_ node] expr]
  (->> node
       (map (fn [[k v]] [(-> k Literal. (assoc :expr k)) (parse* v (k expr))]))
       (reduce (fn [init [k v]]
                 (curry (assoc (Symbol. 'assoc) :expr 'assoc) [init k v]))
               (assoc (Symbol. 'map.) :expr {}))))

(defmethod parse* ::Sel  [[_ {:keys [op arg]}] expr]
  (assoc (Select. (parse* arg (second expr))
                  (assoc (Literal. op) :expr op))
         :expr expr))

(defmethod parse* ::Ext  [[_ {:keys [op r l a]}] expr]
  (-> (parse* [::Var op] (first expr))
      (Apply. (parse* r (second expr)))
      (Apply. (assoc (Literal. l) :expr l))
      (Apply. (parse* a (nth expr 3)))
      (assoc :expr expr)))

(defmethod parse* ::Res  [[_ {:keys [op r l]}] expr]
  (-> (parse* [::Var 'type/restrict] (first expr))
      (Apply. (assoc (Literal. l) :expr l))
      (Apply. (parse* r (second expr)))
      (assoc :expr expr)))

;;; TODO: How to varargs
(defn parse [expr]
  (let [ast (s/conform ::expr expr)]
    (if (s/invalid? ast)
      (do
        ;; (s/explain ::expr expr)
        (throw (Exception. "Invalid Syntax")))
      (parse* ast expr))))
