(ns lift.lang.type.base
  (:require
   [clojure.core.protocols :refer [IKVReduce]]
   [clojure.set :refer [difference union]]
   [clojure.string :as string]
   [lift.f.functor :as f :refer [Functor]]
   [lift.lang.type.impl :as impl :refer [cata Show]])
  (:import
   [clojure.lang IFn ILookup IPersistentMap]
   [lift.lang.type.impl Type]))

(defprotocol Ftv (-ftv [x]))

(extend-protocol Ftv
  Object (-ftv [x] x))

(def ftv (partial cata -ftv))

(defprotocol Sub (-sub [x s]))

(extend-protocol Sub
  Object (-sub [x s] (f/map #(% s) x))
  nil    (-sub [_ _] nil))

(defn substitute [x s]
  (let [f (cata (fn [x'] (fn [s'] (-sub x' s'))) x)]
    (f s)))

(impl/deftype (Unit)
  Functor (-map  [x _] x)
  Ftv     (-ftv  [_]   #{})
  Show    (-show [_]   "()"))

(impl/deftype (Const x)
  Functor (-map  [x _] x)
  Ftv     (-ftv  [_]   #{})
  Show    (-show [_]   (str x)))

(impl/deftype (Var a)
  Functor (-map  [x _] x)
  Ftv     (-ftv  [_]   #{a})
  Show    (-show [_]   (name a))
  Sub     (-sub  [x s] (get s a x)))

(impl/deftype (Vargs a)
  Ftv  (-ftv [_] #{a})
  Show (-show [_] (format "(Vargs & %s)" a))
  Sub  (-sub [_ s] (Vargs. (get s a a))))

(impl/deftype (Arrow a b)
  Functor (-map  [_ f] (Arrow. (f a) (f b)))
  Ftv     (-ftv  [_]   (union a b))
  Show    (-show [_]   (format "(%s -> %s)" a b)))

(impl/deftype (Forall as t)
  Ftv  (-ftv  [x]   (difference t as))
  Show (-show [_]   (str (string/join " " (map #(str "∀" %) as)) \. t))
  Sub  (-sub  [_ s] (Forall. as (t (apply dissoc s as)))))

(impl/deftype (Predicate tag as)
  Ftv  (-ftv  [_] (apply union as))
  Show (-show [_] (format "%s %s" (name tag) (string/join " " as))))

(impl/deftype (Predicated preds t)
  Functor (-map  [_ f] (Predicated. (map f preds) (f t)))
  Ftv     (-ftv  [_]   (difference t preds))
  Show    (-show [_]   (str (string/join ", " preds) " => " t)))

(impl/deftype (New t))

(impl/deftype (Container tag args)
  Ftv  (-ftv [_] (set (apply concat args)))
  Show (-show [_]
         (if args
           (format "(%s %s)" (name tag) (string/join ", " args))
           (name tag))))

(impl/deftype (RowEmpty)
  Ftv  (-ftv  [_] #{})
  Show (-show [_] "{}"))

(impl/deftype (Row k v tail)
  Functor (-map  [_ f] (Row. (f k) (f v) (f tail)))
  Ftv     (-ftv  [_]   (union v tail))
  Show    (-show [_]   (format "%s : %s, %s" k v tail)))

(impl/deftype (Record row)
  Ftv  (-ftv  [_] row)
  Show (-show [x] (format "{%s}" row)))

(impl/deftype (Select l r)
  Show (-show [_] (format "(%s %s)" l r)))

(impl/deftype (Restrict l r)
  Show (-show [_] (format "(dissoc %s %s)" r l)))

(impl/deftype (List xs)
  Functor (-map  [_ f] (List. (f/-map xs f)))
  Ftv     (-ftv  [_]   (set xs))
  Show    (-show [_]   (format "(%s)" (string/join " " xs))))

(impl/deftype (Vector xs)
  Functor (-map  [_ f] (Vector. (f/-map xs f)))
  Ftv     (-ftv  [_]   (set xs))
  Show    (-show [_]   (format "[%s]" (string/join " " xs))))

(impl/deftype (Map r)
  Functor (-map [_ f] (Map. (f/-map r f)))
  Show    (-show [_]  r))

(impl/deftype (Tuple xs)
  Functor (-map  [_ f] (Tuple. (f/-map xs f)))
  Ftv     (-ftv  [_]   (set (apply concat xs)))
  Show    (-show [_]   (format "[%s]" (string/join " " xs))))

(impl/deftype (Mark a))

(impl/deftype (Literal a)
  Functor (-map [x _] x)
  Show    (-show [_] (pr-str a)))

(impl/deftype (Symbol a)
  Functor (-map [x _] x)
  Show    (-show [_] (name a)))

(impl/deftype (Key k)
  Functor (-map  [x _] x)
  Show    (-show [_]   k))

(impl/deftype (Lambda x e)
  Show (-show [_] (format "(fn [%s] %s)" (impl/-show x) e)))

(impl/deftype (Apply e1 e2)
  Show    (-show [_] (format "(%s %s)" e1 e2))
  Functor (-map [_ f] (Apply. (f e1) (f e2))))

(impl/deftype (Let x e1 e2)
  Show    (-show [_] (format "(let [%s %s] %s)" x e1 e2))
  Functor (-map [_ f] (Let. x (f e1) (f e2))))

(impl/deftype (If cond then else)
  Show    (-show [_] (format "(if %s %s %s)" cond then else))
  Functor (-map [_ f] (If. (f cond) (f then) (f else))))

(impl/deftype (Prim f t)
  Functor (-map   [x _] x)
  IFn     (invoke [_ x] ((eval f) x))
  Show    (-show  [_  ] (name (second f))))

(impl/deftype (SyntaxNode n t e m)
  Functor (-map  [_ f] (SyntaxNode. (f n) t e m))
  Show    (-show [_]
            (let [expr (cond (instance? Type n) n
                             (string? n) n
                             :else (pr-str n))]
              (str n
                   (when t (str (when (> (count expr) 40) "\n  ")
                                " : " (pr-str t))))))
  Sub     (-sub  [_ s] (SyntaxNode. (n s) (substitute t s) e m)))

(impl/deftype (Curry f)
  Show (-show [_] (format "(Curry %s)" f)))

(impl/deftype (Variadic vfns)
  Functor (-map [_ f] (Variadic. (f/-map vfns f)))
  Show    (-show [_]
            (->> vfns
                 (map (fn [[xs e]] (format "(%s %s)" (pr-str xs) (pr-str e))))
                 (string/join "\n ")
                 (format "(fn\n %s)"))))

(impl/deftype (Env t)
  Ftv     (-ftv  [_]   (->> t vals (apply concat) set))
  ;; Functor (-map  [_ f] (Env. (f/-map t f)))
  Show    (-show [x]   (str "Γ {" t "}"))
  ;; Sub     (-sub  [_ s] (Env. (f/-map #(% s) t)))
  )

(impl/deftype (Substitution s)
  IKVReduce
  (kv-reduce [_ f init]
    (let [[s'] init] (Substitution. (reduce-kv f s' s))))
  IPersistentMap
  (without [_ k] (Substitution. (dissoc s k)))
  ILookup
  (valAt [_ k] (get s k))
  ILookup
  (valAt [_ k not-found] (get s k not-found))
  Show
  (-show [_] (format "S[%s]" s)))

(defn $ [expr type & [err meta]]
  (SyntaxNode. expr type err meta))

(def container-types
  `[Container RowEmpty Row Record Restrict Select List Vector Map Tuple])

(defmacro import-container-types []
  `(do (import ~@container-types) nil))

(defmacro import-infer-types []
  `(do (import ~@`[Env Substitution]) nil))

(defmacro import-syntax-types []
  `(do (import ~@`[Literal Symbol Key Lambda Apply Let If Prim SyntaxNode Curry Mark]) nil))

(defmacro import-type-types []
  (let [types `[Unit Const Var Vargs Arrow Forall Predicate Predicated New]]
    `(do (import ~@types) nil)))

(defmacro import-types []
  `(do
     (import-container-types)
     (import-infer-types)
     (import-syntax-types)
     (import-type-types)))
