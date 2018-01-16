(ns lift.type
  (:refer-clojure :exclude [type]))

(alias 'c 'clojure.core)

(deftype* lift.type/Type lift.type.Type [])
(def Type (lift.type.Type.))

(deftype* lift.type/Name lift.type.Name [t])
(defn Name [tag] (lift.type.Name. tag))
(defmethod print-method lift.type.Name [x w]
  (.write w (name (.-t x))))

(deftype* lift.type/Vargs lift.type.Vargs [a])
(defn Vargs [a] (lift.type.Vargs. a))
(defmethod print-method lift.type.Vargs [x w]
  (.write w (str (name (.-a x)) "...")))

(deftype* lift.type/Var lift.type.Var [__meta a]
  :implements [clojure.lang.IObj clojure.lang.IMeta]
  (withMeta [_ x] (new Var x a))
  (meta [_] __meta))
(defn Var [a] (new lift.type.Var {:type lift.type.Type} a))
(defmethod print-method lift.type.Var [x w]
  (.write w (name (.-a x))))

(deftype* lift.type/App lift.type.App [__meta t args]
  :implements [clojure.lang.IObj clojure.lang.IMeta]
  (withMeta [_ x] (new App x t args))
  (meta [_] __meta))
(defn App [t args] (lift.type.App. {:type lift.type.Type} t args))
(defmethod print-method lift.type.App [x w]
  (.write w (format "(%s %s)" (pr-str (.-t x)) (pr-str (.-args x)))))

(deftype* lift.type/Fn lift.type.Fn [__meta a b]
  :implements [clojure.lang.IObj clojure.lang.IMeta]
  (withMeta [_ x] (new Fn x a b))
  (meta [_] __meta))
(defn Fn [a b] (lift.type.Fn. {:type lift.type.Type} a b))
(defmethod print-method lift.type.Fn [x w]
  (.write w (format "(%s -> %s)" (pr-str (.-a x)) (pr-str (.-b x)))))

(defn type [x]
  (get @type-map (c/type x)))
