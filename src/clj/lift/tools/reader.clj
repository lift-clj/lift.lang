(ns lift.tools.reader
  (:require
   [clojure.tools.reader :as r]
   [clojure.tools.reader.reader-types :as rt :refer [to-pbr]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [lift.lang.type.base :as base])
  (:import
   [lift.lang.type.base Mark]))

(deftype SyntaxMarkingPushbackReader
    [rdr
     ^:unsynchronized-mutable mark-line
     ^:unsynchronized-mutable mark-col
     ^:unsynchronized-mutable mark
     ^:unsynchronized-mutable buf]
  rt/Reader
  (rt/read-char [reader]
    (let [line (rt/get-line-number rdr)
          col  (rt/get-column-number rdr)]
      (cond
        (and mark-line (= mark-line line) (= mark-col col))
        (do
          (set! mark-line nil)
          (set! mark "mark ")
          \#)
        (seq mark)
        (let [ch (first mark)]
          (set! mark (subs mark 1))
          ch)
        :else
        (rt/read-char rdr))))
  (rt/peek-char [reader] (rt/peek-char rdr))
  rt/IPushbackReader
  (rt/unread [reader ch]
    (if (seq mark)
      (set! mark (str ch mark))
      (rt/unread rdr ch)))
  rt/IndexingReader
  (rt/get-line-number [reader] (rt/get-line-number rdr))
  (rt/get-column-number [reader] (rt/get-column-number rdr))
  (rt/get-file-name [reader] (rt/get-file-name rdr))
  java.io.Closeable
  (close [this]
    (when (instance? java.io.Closeable rdr)
      (.close ^java.io.Closeable rdr))))

(defn ^java.io.Closeable syntax-marking-push-back-reader
  ([rdr mark-line mark-col]
   (SyntaxMarkingPushbackReader. rdr mark-line mark-col nil nil)))

(deftype CharSyntaxMarkingPushbackReader
    [rdr
     ^:unsynchronized-mutable cnt
     ^:unsynchronized-mutable mark-at
     ^:unsynchronized-mutable mark]
  rt/Reader
  (rt/read-char [reader]
    (cond
      (and mark-at (= mark-at cnt))
      (do
        (set! mark-at nil)
        (set! mark "mark ")
        \#)
      (seq mark)
      (let [ch (first mark)]
        (set! mark (subs mark 1))
        ch)
      :else
      (let [ch (rt/read-char rdr)]
        (set! cnt (inc cnt))
        ch)))
  (rt/peek-char [reader] (rt/peek-char rdr))
  rt/IPushbackReader
  (rt/unread [reader ch]
    (if (seq mark)
      (set! mark (str ch mark))
      (do
        (rt/unread rdr ch)
        (set! cnt (dec cnt)))))
  rt/IndexingReader
  (rt/get-line-number [reader] (rt/get-line-number rdr))
  (rt/get-column-number [reader] (rt/get-column-number rdr))
  (rt/get-file-name [reader] (rt/get-file-name rdr))
  java.io.Closeable
  (close [this]
    (when (instance? java.io.Closeable rdr)
      (.close ^java.io.Closeable rdr))))

(defn ^java.io.Closeable char-syntax-marking-push-back-reader
  ([rdr mark-at]
   (CharSyntaxMarkingPushbackReader. rdr 0 mark-at nil)))

;; (with-open [r (-> "test 1 2"
;;             (java.io.StringReader.)
;;             (rt/indexing-push-back-reader)
;;             (syntax-marking-push-back-reader 1 6))]
;;   (binding [r/*data-readers* {'mark (fn [x] `(mark ~x))}]
;;     (r/read r)
;;     (r/read r))
;;   )

(defn read-with-meta [{:keys [start end expr] :as expr-info}]
  {:start 1
   :end   (inc (- end start))
   :expr  nil ;(-> expr char-counting-push-back-reader r/read)
   })

(defn unify-position [expr topx]
  (let [start-expr (:start expr)
        {:keys [start end]} topx
        expr (read-with-meta expr)
        topx (read-with-meta topx)
        start-expr (- start-expr start)]
    [(-> expr (update :start + start-expr) (update :end + start-expr)) topx]))

(defn found? [expr node]
  (let [node-expr (:expr node)
        {:keys [column end-column] :as m} (meta node-expr)]
    (= expr {:start column :end end-column :expr node-expr})))

(defn drop-lines [n r]
  (if (pos? n)
    (do (rt/read-line r) (recur (dec n) r))
    r))

(defn top-level-sexp [string mark-at]
  (with-open [r (-> (java.io.StringReader. string)
                    (rt/indexing-push-back-reader)
                    (char-syntax-marking-push-back-reader mark-at))]
    (binding [r/*data-readers* {'mark (fn [x] (Mark. x))}]
      (r/read r))))

(defn read-namespace [filename]
  (with-open [r (-> filename java.io.FileReader. rt/indexing-push-back-reader)]
    (let [form (r/read r false nil)]
      (if (and (seq? form) (= 'ns (first form)))
        form
        (throw
         (Exception. (str "Namespace not first form in file: " filename)))))))