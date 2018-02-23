
(ns demo
  {:lang :lift/clojure}
  ;; ^^ `metadata` on namespace
  ;; means namespace will be type-checked when turned on
  (:refer-clojure :exclude [+ * - / = case defn let map name read not=])
  (:require
   [clojure.core :as c]
   [lift.lang :refer :all]
   [lift.lang.type :as t]))

;; A few type annotations for `clojure.core`
(t/def (List a))
(t/def (Vector a))
(t/def instance? (Class -> a -> Boolean))
(t/def identity  (a -> a))
(t/def partial   ((a -> b -> c) -> a -> (b -> c)))
(t/def inc       (Int -> Int))
(t/def pos?      (Int -> Boolean))
(t/def str       (a -> String))
(t/def nth       (Vector a -> Int -> a))
(t/def name      (Keyword -> String))
(t/def =         (a -> a -> Boolean))
(t/def +         (Int -> Int -> Int))
(t/def *         (Int -> Int -> Int))
(t/def /         (Int -> Int -> Ratio))
(t/def double    (Ratio -> Double))
(t/def list      (List a))
(t/def cons      (a -> (List a) -> (List a)))
(t/def first     ((List a) -> a))
(t/def vector    (Vector a))
(t/def conj      (Vector a -> a -> Vector a))
(t/def get       ({l a | r} -> l -> a))
(t/def assoc     ({| r} -> l -> a -> {l a | r}))
(t/def dissoc    ({l a | r} -> l ->  {| r}))
(t/def keyword   (String -> Keyword))
(t/def reverse   (List a -> List a))
(t/def map       ((a -> b) -> (List a) -> (List b)))


(t/def  inc-point (Long -> Long -> Vector Long))
(defn inc-point [x y]
  [(inc x) (inc y)])

;; toggle `*type-check*` `OFF`
;; Typically, you're writing a function... and running it in the REPL
(defn ill-typed-function [x]
  (inc-point x "string"))

(ill-typed-function 1)

;; turn type-checking `ON`

;; Don't need to run it

(defn ill-typed-function [x]
  (inc-point x "string"))

;; Extensible records, with row types
{:a 1 :b :c}

;; `assoc` adds key/type `::y : String`
(-> {:a 1 :b :c}
    (assoc ::y "your name"))

;; `dissoc` removes key/type `:b : Keyword`
(-> {:a 1 :b :c}
    (assoc ::y "your name")
    (dissoc :b))

;; `lookup` gets the value and type
(-> {:a 1 :b :c}
    (assoc ::y "your name")
    (dissoc :b)
    :a)

;; `lookup` fails becaus :b not in the map
(-> {:a 1 :b :c}
    (assoc ::y "your name")
    (dissoc :b)
    :b)

;; Define VAT
(def VAT 0.20)

;; A function using VAT
(defn with-vat [x]
  (* (+ 1.0 VAT) x))

;; Another function using that function
(defn add-final-total [rental-pricing]
  (assoc rental-pricing
         :total (with-vat (:ex-vat-total rental-pricing))))

;; Type checker tells me I can't call this
;; What's the type of `add-final-total`?
(add-final-total {:something 1})


(let [x {:ex-vat-total 500.0}]
  (add-final-total x))
;; I haven't written any types

;; Doesn't make assumptions about what else is in the map
;; It just cares about the types needed to fulfil the function
(let [x {:ex-vat-total 500.0 :something-else "This thing?"}]
  (add-final-total x))

;; But it keeps those keys->types around
;; So we could use it later in the program
(let [x {:ex-vat-total 500.0 :something-else "This thing?"}]
  (:something-else (add-final-total x)))

(defn deep-map-returning-fn [id]
  (-> {}
      (assoc :a {:b {:c {:d id}}})
      (assoc :e {:f {:g 30}})
      (assoc :h {:i "wut"})))

;; Tells you things about your data (\space t e)
(deep-map-returning-fn 1142)

;; You don't need to 'run' it, you can ask what type of thing this expr would
;; return
(:a (deep-map-returning-fn 1142))

;; Testing out different syntax elements, inspect types
(case (let [a (Left {:something "Hi!"})
            b (Right "test")
            c false]
        ((fn [y] (let [x y] (if c x b))) a))
  (Left  a) (:something a)
  (Right b) b)

(defn some-function [either-val]
  (case either-val
    (Left  a) (:something a)
    (Right _) 500))


;; Different logical interpreter
(def list-of-eithers
  (->> () (cons (Right 1)) (cons (Left {:something 2}))))


;; Hole DD / type search
;; what type of thing belongs here
(map _? list-of-eithers)

(defn some-other-function [condition either-val]
  (if condition
    (case either-val
      (Left  a) (Just (:something a))
      (Right b) Nothing)
    Nothing))

(def list-of-eithers-2
  (->> ()
       (cons (Right "Err"))
       (cons (Left {:something "coffee"}))
       (cons (Right "Err2"))
       (cons (Left {:something "this"}))))

;; If the types are refined enough, why not just fill it in
(map (_? true) list-of-eithers-2)


;; Type safe containers, asserting an invariant is held when creating a type
;; that way, you can't construct an invalid state
(with-ctor
  (data Email = Email String)
  (String -> Maybe Email)
  (fn [s]
    (if (.contains s "@") (Just (Email s)) Nothing)))

(Email "not-a-real-email")
(Email "me@andrewmcveigh.com")

(def fake-email? (Email "not-a-real-email"))

;; this gives me a Maybe emali, because the constructor will return Nothing
;; if the email address is invalid
(def my-email? (Email "me@andrewmcveigh.com"))

;; but, it can be a pain to have to unwrap these things all the time
;; so we can define a one-way coercion
;; here we have a interface function coerce that will take any Coercible type a
;; and turn it into a type b
(interface (Coercible a b)
  (coerce (a -> b)))

;; as long as Coercible is defined for type a and type b
(impl (Coercible String Long)
  (coerce [a] (read a)))

;; so here we define coercible from email to string, by unwrapping it
(impl (Coercible Email String)
  (coerce ([(Email s)] s)))

;; Here I'm annotating the type so that I can get the compiler to work out
;; which implementation I want below
;; So it's not passive code to _satisfy_ the compiler, this signature is
;; actively directing the _return type_ polymorphism
(t/def person (String -> String -> {:name String :email String}))
(defn  person [name email]
  {:name name :email email})

;; no demo of a type system for a functional programming language would be
;; complete without an implementation of `Monad`
(interface (Monad m)
  (return (a -> m a))
  (>>=    (m a -> (a -> m b) -> m b)))

;; Here's the almost copy-paste from Haskell Maybe instance
(impl (Monad Maybe)
  (return ([a] (Just a)))
  (>>=
    ([(Just x) f] (f x))
    ([Nothing  _] Nothing)))

;; And do notation in a two line macro. Clojure is still really powerful
(defmacro mdo
  {:style/indent :defn}
  [bindings expr]
  (->> (reverse (partition 2 bindings))
       (reduce (fn [expr [sym mv]] `(demo/>>= ~mv (fn [~sym] ~expr))) expr)))

;; can you see what's going on here?
(mdo [email my-email?]
  (return (person "Andrew" (coerce email))))

;; the monadic do notation is looking under the Maybe of `my-email?`, binding
;; the actual `Email` to `email`.
;; Then the `coerce` instance from `Email` `->` `String` is being picked by the
;; type system, because the 2nd argument of `person` is of type `String`.
;; And then, `return`, also polymorphic in the Monad return type, in this case
;; `Maybe`
