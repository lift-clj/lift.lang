(ns dev
  (:refer-clojure :exclude [var?])
  (:require
   [clojure.core :as c]
   [clojure.pprint :refer [pprint]]
   [lift.lang :refer [data Just Pair]]
   [lift.f.functor :as f]
   [lift.lang.util :as u]
   [lift.lang.type :as t]))

;; (base/import-syntax-types)
;; (base/import-type-types)

;; (check '(fn [x y] x))

;; (impl/ana ana/parse '[])

;; (data P a b = P a b)

;; (case (P 1 (P (Just 1) (Just 2)))
;;   [P a [P [Just x] [Just b]]] [a b x]
;;   [P a b] [a b]
;;   [S a] a
;;   Nothing 0)

;; (defn tree-merge [a b]
;;   ;; (prn 'a a)
;;   ;; (prn 'b b)
;;   (cond (and (c/= :bind (:type a)) (c/= :bind (:type b)))
;;         {:type :bind
;;          :ings (mapv #(merge-with tree-merge % %2)
;;                      (:ings a)
;;                      (:ings b))}
;;         (and (map? a) (map? b))
;;         (merge-with tree-merge a b)
;;         :else
;;         a))


(defn type? [x]
  (and (simple-symbol? x) (re-matches #"^[A-Z].*$" (name x))))

(defn var? [x]
  (and (simple-symbol? x) (re-matches #"^[^A-Z].*$" (name x))))

(defn ing-bound? [tree]
  (or (some var? (keys (:bind tree))) (:dflt tree)))

(defn ing-binding [{:keys [bind dflt]}]
  (or dflt (->> bind (filter (comp var? key)) (into {}))))

(defn fully-bound? [{:keys [type] :as x}]
  (c/case type
    :bind (every? fully-bound? (:ings x))
    :ing  (and (ing-bound? x)
               (every? (comp fully-bound? val) (:dest x)))))

;; (defn push-default-binding [default {:keys [type] :as tree}]
;;   ;; (prn 'default default)
;;   ;; (prn 'bound? (fully-bound? tree) type)
;;   ;; (prn 'ing-bound? (ing-bound? tree))
;;   ;; (prn tree)
;;   (if (fully-bound? tree)
;;     tree
;;     (letfn [(push-f [default]
;;               (partial f/map (partial push-default-binding default)))]
;;       (c/case type
;;         :bind (-> (update tree :ings (push-f default))
;;                   (cond-> (not (:dflt tree))
;;                     (assoc :dflt default)))
;;         :ing (cond-> tree
;;                (:dest tree)
;;                (update :dest
;;                        (push-f (if (ing-bound? tree) (ing-binding tree) default))))))))

;; (defn match-ast [match-list]
;;   (letfn [(f [[pattern expr]]
;;             (if (vector? pattern)
;;               {:type :ing
;;                :dest {(first pattern)
;;                       {:type :bind
;;                        :ings (mapv (fn [p] (f [p expr])) (rest pattern))}}}
;;               {:type :ing :bind {pattern expr}}))]
;;     (->> match-list
;;          (map f)
;;          (reduce #(merge-with tree-merge % %2))
;;          (push-default-binding nil))))

;; (def ast
;;   (match-ast '[
;;                ;; [[P [Just a] [P [Just x] [Just b]]] [a b x]]
;;                ;; [[P [Just a] [P [Just x] Nothing]] [a x]]
;;                [[P [Just a] [P x Nothing]] [a x]]
;;                [[P a b] [a b]]
;;                [_ 1]]))

(defn append
  ([] nil)
  ([xs] xs)
  ([xs y] (concat xs [y])))

;; (defn dest-branch [x [p ast']]
;;   (let [{:keys [isa? fs]} (-> p resolve meta :prj)
;;         ast' (update ast'
;;                      :ings (partial map #(assoc %2 :prj %) fs))]
;;     `(if (~@isa? ~x) ~(case-branch x ast'))))

;; (defn bind-branch [x [p expr]]
;;   (if (var? p)
;;     `(let* [~p ~x] ~expr)
;;     `(if (lift.lang.prim/eq ~x ~(u/resolve-sym p)) ~expr)))

;; (defn prj-branch [gs {:keys [prj] :as ing}]
;;   (case-branch gs ing))

;; (defn case-branch [x ast]
;;   (case (:type ast)
;;     :bind
;;     (let [ings (:ings ast)
;;           gsym (map (fn [_] (gensym)) ings)
;;           gings (reverse (map vector gsym ings))]
;;       `(let* ~(vec (interleave gsym (map (fn [{p :prj}] `(~p ~x)) ings)))
;;          ~(reduce
;;            (fn [expr [gs x]]
;;              (append (butlast (case-branch gs x)) expr))
;;            (apply case-branch (first gings))
;;            (rest gings))))
;;     :ing
;;     (->> (append
;;           (->> (:dest ast) (map (partial dest-branch x)))
;;           (->> (:bind ast) (map (partial bind-branch x)) (reduce append)))
;;          (reverse)
;;          (reduce (fn [a b] (append b a))))))

;; (eval (case-branch '(Just 1) ast))

;; (case-branch '(Just 1) ast)

;; {_1 {Just    {:> {Just    {:> x :- _2}
;;                   Nothing {     :- _2}}}
;;      Nothing {                  :- _2}}
;;  _2 {Just {:> {Just {:> y}}}}}

;; (def match '[Just [Just x]])

;; (defn match-ast [match deps]
;;   (if (vector? match)
;;     (let [[p & [b & bs]] match
;;           [g & gs] (map (fn [b] (gensym)) (cons b bs))]
;;       (if (type? p)
;;         (let [[m d] (match-ast b (concat gs deps))]
;;           [{:type :test
;;             p (cond-> {:type :project
;;                        :prjs (conj (->> bs
;;                                         (map #(first (match-ast % deps)))
;;                                         (map vector gs)
;;                                         (reverse)
;;                                         (apply into []))
;;                                    g m)}
;;                 (and d (not (contains? (set gs) d)))
;;                 (assoc :next d))}])
;;         (throw (Exception. "bad match"))))
;;     (cond-> [{:type :binding :bind match}]
;;       (seq deps)
;;       (into (match-ast (first deps) (rest deps))))))


;; (defmulti compile-ast :type)

;; (defmethod compile-ast :test [tree]
;;   (let [tests (dissoc tree :type)]
;;     (fn [x]
;;       (prn x)
;;       (reduce (fn [expr [p tree]]
;;                 (if p
;;                   (let [{:keys [isa? fs]} (-> p resolve meta :prj)]
;;                    (prn p isa? fs)
;;                    `(if (~@isa? ~x)
;;                       ~((compile-ast (assoc tree :fs fs)) x)
;;                       ~expr))
;;                   (throw (Exception. (str "Type not imported: " p)))))
;;               `(t/unmatched-case-error ~x)
;;               tests))))

;; (defmethod compile-ast :project [{:keys [prjs fs]}]
;;   (fn [x]
;;     (prn x)
;;     (->> (partition 2 prjs)
;;          (map vector (reverse fs))
;;          (reduce (fn [expr [f [gs tree]]]
;;                    (prn expr)
;;                    (prn f gs tree)
;;                    `(let* [~gs (~f ~x)]
;;                       ~(prn gs)
;;                       ~((compile-ast tree) gs)))))))

;; (defmethod compile-ast :binding [{:keys [bind expr]}]
;;   (fn [gs]
;;     (prn gs)
;;     `(let* [~bind ~gs] ~expr)))

(declare compile-case)

(defn let-prjs [x ps branches then-k else-k]
  (let [letbs (map (fn [p b]
                     (let [gs (gensym)]
                       {:let [gs `(~p ~x)]
                        :branch #(compile-case gs b % else-k)}))
                   ps
                   branches)
        [s & btel] (reverse (map :branch letbs))]
    `(let* [~@(mapcat :let letbs)]
       ~(if btel
          (reduce (fn [x f] (f x)) (s then-k) btel)
          (s then-k)))))

(defn prj-pairs [x prjs prj-map then-k else-k]
  ()
  )

(defn compile-test [x [t & prjs] then-k else-k]
  (let [{:keys [isa? fs]} (-> t resolve meta :prj)]
    `(if (~@isa? ~x)
       ~(let-prjs x fs prjs then-k else-k)
       ~(else-k identity))))

(defn compile-bind [x binding then-k]
  `(let* [~binding ~x]
     ~then-k))

(defn compile-literal [x lit then-k else-k]
  `(if (= ~x ~lit)
     ~(then-k identity)
     ~(else-k identity)))

(defn compile-case [x match-tree expr else-k]
  (cond (vector? match-tree)
        (compile-test x match-tree expr else-k)
        (var? match-tree)
        (compile-bind x match-tree expr)
        :literal-be-specific?
        (compile-literal x match-tree expr else-k)))

'[[[Pair [Pair [Just a] [Just b]] c] [a b c]]
  [[Pair [Pair       d  [Just e]] f] [d e f]]
  [[Pair             g            h] [g h]]]


'[[[Pair [Pair [Just a] [Just b]] c] [a b c]]
  [[Pair [Pair       d  [Just e]] f] [d e f]]
  [[Pair             g            h] [g h]]]

'[[Pair [[[[Pair [Just a] [Just b]] c] [a b c]]
         [[[Pair       d  [Just e]] f] [d e f]]
         [[            g            h] [g h]]]]
  _                                    [0]]

(let [n (atom 0)]
  (defn gsym [& _]
    (symbol (str "_" (swap! n inc)))))

(def branch-1
  '{:type :dest
    :test Pair
    :gsym _0
    :prjs [_1 _2]
    :then {:type :dest
           :test Pair
           :gsym _1
           :prjs [_3 _4]
           :then {:type :dest
                  :test Just
                  :gsym _3
                  :prjs [_5]
                  :then {:type :bind
                         :bind [a]
                         :gsym _5
                         :then {:type :dest
                                :test Just
                                :gsym _4
                                :prjs [_6]
                                :then {:type :bind
                                       :bind [b]
                                       :gsym _6
                                       :then {:type :bind
                                              :bind [c]
                                              :gsym _2
                                              :then {:type :expr
                                                     :expr [a b c]}}}}}}}})

(declare case-tree)
(defn ctor-proj [tree expr p gs]
  (fn [k]
    (assoc (case-tree nil tree expr k)
           ;; :prjx p
           :gsym gs)))

(defn ctor-dest [[type & pattern] expr k]
  (let [{:keys [isa? fs]} (-> type resolve meta :prj)
        prjs (mapv gsym fs)
        then (reverse (map #(ctor-proj % expr %2 %3) pattern fs prjs))]
    ;; (prn type gs prjs)
    {:type :dest
     :test type
     ;; :isa? isa?
     :prjs prjs
     :then (reduce (fn [x f] (f x)) ((first then) k) (rest then))}))

(defn ctor-bind [a expr k]
  (let [node {:type :bind :bind [a]}]
    (if k
      (assoc node :then k)
      (assoc node :expr expr))))

(defn ctor-litr [& args])

(defn merge-pattern [a pattern expr k]
  (here be the next step)
  )

(defn merge-dest [{:keys [test] :as a} [type & pattern :as match] expr k]
  (if (= test type)
    (update a :then merge-pattern pattern expr k)
    (update a :else case-tree match expr k)))

(defn case-tree [{ta :type :as a} match expr k]
  (cond (nil? ta)
        (cond (vector? match)
              (ctor-dest match expr k)
              (var? match)
              (ctor-bind match expr k)
              :literal
              (ctor-litr match expr k))
        (= :dest ta)
        (cond (vector? match)
              (merge-dest a match expr k)
              (var? match)
              (if-not (:else a)
                (update a :else ctor-bind match expr k)
                (throw (Exception. "Trying to bind twice"))))
        (= :bind ta)
        (cond (vector? match)
              (throw
               (Exception. "Trying to match pattern after irrefutable bind"))
              (var? match)
              (throw
               (Exception. "Trying to bind twice")))))

(def case-1
  '[Pair [Pair [Just a] [Just b]] c])

(def case-2
  '[Pair [Pair a [Just b]] c])

(clojure.pprint/pprint
 (-> (case-tree nil case-1 '[a b c] nil)
     (case-tree case-2 '[a b c] nil)
     ))

(def branch-2
  '{:type :dest
    :test Pair
    :then {:type :dest
           :test Pair
           :then {:type :bind
                  :bind [d]
                  :then {:type :dest
                         :test Just
                         :then {:type :bind
                                :bind [e]
                                :then {:type :bind
                                       :bind [f]
                                       :then {:type :expr
                                              :expr [d e f]}}}}}}})

(declare merge-branches)

(defn merge-dest [a b]
  (if (= (:test a) (:test b))
    (update a :then merge-branches (:then b))
    (update a :else merge-branches b)))

(defn merge-bind [a b]
  (if (= (:bind a) (:bind b))
    (update a :then merge-branches (:then b))
    (-> a
        (update :bind conj (:bind b))
        (update :then merge-branches (:then b)))))

(defn merge-branches [{:keys [type] :as a} b]
  (prn a)
  (prn b)
  (if (= type (:type b))
    (case type
      :dest (merge-dest a b)
      :bind (merge-bind a b)
      :expr (throw (Exception. "Expr should be resolved before now")))
    (if b
      (update a :else merge-branches b)
      a)))

;; (merge-branches branch-1 branch-2)


;; (let [a '(Pair 1)]
;;   (compile-case a
;;                 '[Pair {[[Pair [Just a] [Just b]] c] [abc]
;;                         [a b] [a b]}]
;;                 '[a b c]
;;                 (fn else [f]
;;                   (compile-case a
;;                                 (fn else-2 [_]
;;                                   `(t/unmatched-case-error ~a))))))

;; (if (clojure.core/instance? lift.lang.__private.Pair (Pair 1))
;;   (let* [G__18182 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a))) (Pair 1))
;;          G__18183 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b))) (Pair 1))]
;;     (if (clojure.core/instance? lift.lang.__private.Pair G__18182)
;;       (let* [G__18184 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a))) G__18182)
;;              G__18185 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b))) G__18182)]
;;         (if (clojure.core/instance? lift.lang.__private.Just G__18184)
;;           (let* [G__18189 ((Prim (clojure.core/fn -prj-Just [x] (clojure.core/nth x 0)) ((Maybe (Var a)) -> (Var a))) G__18184)]
;;             (let* [a G__18189]
;;               (if (clojure.core/instance? lift.lang.__private.Just G__18185)
;;                 (let* [G__18186 ((Prim (clojure.core/fn -prj-Just [x] (clojure.core/nth x 0)) ((Maybe (Var a)) -> (Var a))) G__18185)]
;;                   (let* [b G__18186]
;;                     (let* [c G__18183] [a b c])))
;;                 (if (clojure.core/instance? lift.lang.__private.Pair (Pair 1))
;;                   (let* [G__18187 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a))) (Pair 1))
;;                          G__18188 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b))) (Pair 1))]
;;                     (let* [a G__18187]
;;                       (let* [b G__18188] [a b])))
;;                   (lift.lang.type/unmatched-case-error (Pair 1))))))
;;           (if (clojure.core/instance? lift.lang.__private.Pair (Pair 1))
;;             (let* [G__18190 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a))) (Pair 1))
;;                    G__18191 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b))) (Pair 1))]
;;               (let* [a G__18190]
;;                 (let* [b G__18191] [a b])))
;;             (lift.lang.type/unmatched-case-error (Pair 1)))))
;;       (if (clojure.core/instance? lift.lang.__private.Pair (Pair 1))
;;         (let* [G__18192 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a))) (Pair 1))
;;                G__18193 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b))) (Pair 1))]
;;           (let* [a G__18192]
;;             (let* [b G__18193] [a b])))
;;         (lift.lang.type/unmatched-case-error (Pair 1)))))
;;   (if (clojure.core/instance? lift.lang.__private.Pair (Pair 1))
;;     (let* [G__18194 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 0)) ((Pair (Var a), (Var b)) -> (Var a))) (Pair 1))
;;            G__18195 ((Prim (clojure.core/fn -prj-Pair [x] (clojure.core/nth x 1)) ((Pair (Var a), (Var b)) -> (Var b))) (Pair 1))]
;;       (let* [a G__18194]
;;         (let* [b G__18195] [a b])))
;;     (lift.lang.type/unmatched-case-error (Pair 1))))



;; [{:type :test,
;;   Pair
;;   {:type :project,
;;    :prjs
;;    [G__29600
;;     {:type :binding, :bind jc}
;;     G__29599
;;     {:type :test,
;;      Pair
;;      {:type :project,
;;       :prjs
;;       [G__29602
;;        {:type :test,
;;         Just
;;         {:type :project,
;;          :prjs [G__29604 {:type :binding, :bind b}],
;;          :next {:type :binding, :bind G__29600}}}
;;        G__29601
;;        {:type :test,
;;         Just
;;         {:type :project,
;;          :prjs [G__29603 {:type :binding, :bind a}],
;;          :next {:type :binding, :bind G__29602}}}]}}]}}]
