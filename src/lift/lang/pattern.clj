(ns lift.lang.pattern
  (:refer-clojure :exclude [case defn let])
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha :as s]
   [lift.lang.type :as t]))

(s/def ::type-name
  (s/and simple-symbol? #(re-matches #"^[A-Z][A-Za-z]*$" (name %))))

(s/def ::type-parameter
  (s/and simple-symbol? #(re-matches #"^[a-z]$" (name %))))

(s/def ::type-args
  (s/+ (s/or ::type-name ::type-name ::type-parameter ::type-parameter)))

(s/def ::parameterized-constructor
  (s/and list? (s/cat ::type-name ::type-name ::type-args ::type-args)))

(s/def ::type-constructor
  (s/or ::type-name ::type-name
        ::type-var  ::type-parameter
        ::parameterized-constructor ::parameterized-constructor))

(s/def ::sum-constructor
  (s/and list?
         (s/cat :_ #(= % 'or)
                ::type-constructor (s/+ ::type-constructor))))

(s/def ::data-constructor
  (s/or ::type-constructor ::type-constructor
        ::sum-constructor ::sum-constructor))

(s/def ::data-type
  (s/and list?
         (s/cat :type ::type-constructor
                :data ::data-constructor)))

(s/def ::binding
  (s/or :type    ::type-name
        :literal (s/and #(not (s/valid? ::destructuring %))
                        (complement #{:as})
                        (complement symbol?))
        :binding symbol?))

(s/def ::destructuring
  (s/and vector?
         (s/cat :type (s/? ::type-name)
                :args (s/+ ::pattern)
                :bind (s/? (s/cat :as #{:as} :binding symbol?)))))

(s/def ::pattern
  (s/or :binding ::binding
        :destructuring ::destructuring))

(c/defn arg-bindings [[t v g?]]
  (c/case t
    :binding (c/let [[btype binding] v]
               (c/case btype
                 :type '_
                 :literal '_
                 :binding binding))
    :destructuring g?))

(c/defn literal= [destructured args]
  (->> destructured
       (map (fn [[_ [btype a]] b] (when (= :literal btype) (= a b))) args)
       (remove nil?)
       (every? true?)))

(c/defn ensure-class [class-or-symbol]
  (if (class? class-or-symbol)
    class-or-symbol
    (resolve class-or-symbol)))

(c/defn let-destructure [match mval value guard expr]
  (c/case match
    :destructuring
    (c/let [args (map (fn [[t :as v]]
                        (c/case t
                          :binding v
                          :destructuring (conj v (gensym))))
                      (:args mval))
            type (:type mval)
            bind (:bind mval)
            dest (remove (comp #{:binding} first) args)
            dsym (gensym)]
      `(c/let [type?# (and ~type (instance? (ensure-class ~type) ~value))]
         (when (or type?# (nil? ~type))
           (c/let [value# ~value
                   ~dsym (if type?# (t/-destructure value#) value#)
                   ~@(when bind [(:binding bind) value])
                   [~@(map arg-bindings args)] ~dsym]
             ~(cond (some (comp #{:literal} first second) args)
                    `(when (literal= ~dsym '~args)
                       ~(if (seq dest)
                          (apply let-destructure (conj (first dest) guard expr))
                          expr))
                    (every? (comp #{:binding} first second) args)
                    expr
                    (not (some (comp #{:literal} first second) args))
                    (if (seq dest)
                      (reduce (fn [expr d]
                                (apply let-destructure (conj d guard expr)))
                              (if (some? guard)
                                `(and ~guard ~expr)
                                expr)
                              (reverse dest))
                      expr))))))
    :binding
    (c/let [[match' value'] mval]
      (c/case match'
        :type `(when (instance? ~(ensure-class value') ~value) ~expr)
        :literal `(when (= ~value' ~value) ~expr)
        :binding `(c/let [~value' ~value] ~expr)))))

(defmacro let [bindings expr]
  (c/let [[bind value] (take 2 bindings)]
    (when (and bind value)
      (if (sequential? bind)
        (c/let [[bind _ [guard]] (partition-by '#{|} bind)
                [match mval] (s/conform ::pattern (vec bind))]
          (let-destructure match mval value guard expr))
        (c/let [[match mval] (s/conform ::pattern bind)]
          (let-destructure match mval value nil expr))))))

(defmacro case [e & bindings]
  (cons 'or
        (map (fn [[pattern expr]] `(let [~pattern ~e] ~expr))
             (partition 2 bindings))))

(defmacro defn [name & body]
  `(c/defn ~name ~'[& args]
     (case (vec ~'args)
       ~@(mapcat (fn [[a & body]] [a (cons 'do body)]) body))))
