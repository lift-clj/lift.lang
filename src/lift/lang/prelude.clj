
;; `:type-check` metadata on namespace
;; means namespace will be type-checked when turned on

;; (ns ^:type-check core
;;   (:refer-clojure :exclude [case])
;;   (:require
;;    check tools
;;    [type :as t :refer [data]]
;;    [clojure.string :as string]
;;    [clojure.walk :as walk]
;;    [clojure.spec.alpha :as s]
;;    [clojure.spec.test.alpha :as st]
;;    [check :as check]))


;; ;; Declaring primitive types

;; (t/prim Boolean)
;; (t/prim Int)
;; (t/prim Double)
;; (t/prim Ratio)
;; (t/prim String)
;; (t/prim Symbol)
;; (t/prim Keyword)
;; (t/prim Char)
;; (t/prim Class)

;; ;; Import crude pattern-matching case macro

;; (defmacro case [x & decl] (t/case x decl))


;; ;; Some syntax for declaring Sum types at the type & value level

;; (data Maybe a = Just a | Nothing)
;; (data Either a b = Left a | Right b)


;; ;; Some naive `clojure.core` annotations

;; (t/def (List a))
;; (t/def (Vector a))
;; (t/def instance? (Class -> a -> Boolean))
;; (t/def identity  (a -> a))
;; (t/def partial   ((a -> b -> c) -> a -> (b -> c)))
;; (t/def inc       (Int -> Int))
;; (t/def pos?      (Int -> Boolean))
;; (t/def str       (a -> String))
;; (t/def nth       (Vector a -> Int -> a))
;; (t/def name      (Keyword -> String))
;; (t/def =         (a -> a -> Boolean))
;; (t/def +         (Int -> Int -> Int))
;; (t/def *         (Int -> Int -> Int))
;; (t/def /         (Int -> Int -> Ratio))
;; (t/def double    (Ratio -> Double))
;; (t/def list      (List a))
;; (t/def cons      (a -> (List a) -> (List a)))
;; (t/def first     ((List a) -> a))
;; (t/def vector    (Vector a))
;; (t/def conj      (Vector a -> a -> Vector a))
;; (t/def map.      {})
;; (t/def get       ({l a | r} -> l -> a))
;; (t/def assoc     ({| r} -> l -> a -> {l a | r}))
;; (t/def dissoc    ({l a | r} -> l ->  {| r}))
;; (t/def keyword   (String -> Keyword))
;; (t/def reverse   (List a -> List a))
;; (t/def map       ((a -> b) -> (List a) -> (List b)))


;; ;; Typically, you're writing a function... and running it in the REPL
;; (defn ill-typed-function [x]
;;   (+ x "string"))

;; (ill-typed-function 1)

;; ;; turn type-checking `ON`

;; ;; Don't need to run it

;; (defn ill-typed-function [x]
;;   (+ x "string"))


;; ;; Extensible records, with row types
;; {:a 1 :b :c}

;; ;; `assoc` adds key/type `::y : String`
;; (-> {:a 1 :b :c}
;;     (assoc ::y "your name"))

;; ;; `dissoc` removes key/type `:b : Keyword`
;; (-> {:a 1 :b :c}
;;     (assoc ::y "your name")
;;     (dissoc :b))

;; ;; `lookup` gets the value and type
;; (-> {:a 1 :b :c}
;;     (assoc ::y "your name")
;;     (dissoc :b)
;;     :a)

;; ;; `lookup` fails becaus :b not in the map
;; (-> {:a 1 :b :c}
;;     (assoc ::y "your name")
;;     (dissoc :b)
;;     :b)

;; (def VAT 20)

;; ;; Weird calculation because I've not implemented polymorphic numbers
;; (defn with-vat [x]
;;   (double (/ (* (+ 100 VAT) x) 100)))

;; (defn add-final-total [rental-pricing]
;;   (assoc rental-pricing
;;          :total (with-vat (:ex-vat-total rental-pricing))))

;; ;; What's the type of `add-final-total`?

;; (add-final-total {:something 1})

;; ;; I haven't written any types

;; (let [x {:ex-vat-total 500}]
;;   (add-final-total x))


;; ;; Testing out different syntax elements, inspect types
;; (case (let [a (Left {:something "Hi!"})
;;             b (Right "test")
;;             c false]
;;         ((fn [y] (let [x y] (if c x b))) a))
;;   [Left  a] (:something a)
;;   [Right b] b)


;; (defn deep-map-returning-fn [id]
;;   (-> {}
;;       (assoc :a {:b {:c {:d 10}}})
;;       (assoc :e {:f {:g 30}})
;;       (assoc :h {:i "wut"})))

;; ;; Tells you things about your data (\space t e)

;; (deep-map-returning-fn 1142)

;; (:a (deep-map-returning-fn 1142))


;; (defn some-function [condition either-val]
;;   (if condition
;;     (case either-val
;;       [Left  a] (:something a)
;;       [Right b] b)
;;     500))

;; (defn some-other-function [condition either-val]
;;   (if condition
;;     (case either-val
;;       [Left  a] (Just (:something a))
;;       [Right b] Nothing)
;;     Nothing))


;; ;; Different logical interpreter

;; (def list-of-maybes
;;   (->> (list) (cons (Right 1)) (cons (Left {:something 2}))))

;; ;; Hole DD / type search
;; (map (partial _? true) list-of-maybes)

;; (def list-of-maybes-2
;;   (->> (list)
;;        (cons (Right "Err"))
;;        (cons (Left {:something "coffee"}))
;;        (cons (Right "Err2"))
;;        (cons (Left {:something "this"}))))

;; (map (partial core/some-other-function true) list-of-maybes-2)

;; ;; Seen what Idris can do?

;; (defmacro macro-test [expr]
;;   (let [[s ast] (check/infer @type/type-env (parse/parse expr))]
;;     (cond (check/unifies? ast (List a))
;;           `(map inc ~expr)
;;           (check/unifies? ast Int)
;;           `(+ ~expr 666)
;;           (check/unifies? ast Boolean)
;;           :hey!)))


;; (macro-test (map identity (cons 1 (cons 1 (list)))))

;; (macro-test (+ 3 4))

;; (macro-test (= 10 5))
