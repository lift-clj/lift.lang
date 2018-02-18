(ns lift.tools.reader
  (:require
   [clojure.tools.reader :as r]
   [clojure.tools.reader.reader-types :as rt :refer [to-pbr]]))

(deftype CharCountingPushbackReader
    [rdr ^:unsynchronized-mutable ^long column file-name]
  rt/Reader
  (rt/read-char [reader]
    (when-let [ch (rt/read-char rdr)]
      (set! column (inc column))
      ch))
  (rt/peek-char [reader]
    (rt/peek-char rdr))
  rt/IPushbackReader
  (rt/unread [reader ch]
    (set! column (dec column))
    (rt/unread rdr ch))
  rt/IndexingReader
  (rt/get-line-number [reader] 0)
  (rt/get-column-number [reader] (int column))
  (rt/get-file-name [reader] file-name)
  java.io.Closeable
  (close [this]
    (when (instance? java.io.Closeable rdr)
      (.close ^java.io.Closeable rdr))))

(defn ^java.io.Closeable char-counting-push-back-reader
  "Creates an IndexingPushbackReader from a given string or PushbackReader"
  ([s-or-rdr]
   (char-counting-push-back-reader s-or-rdr 1))
  ([s-or-rdr buf-len]
   (char-counting-push-back-reader s-or-rdr buf-len nil))
  ([s-or-rdr buf-len file-name]
   (CharCountingPushbackReader.
    (to-pbr s-or-rdr buf-len) 1 file-name)))

(defn read-with-meta [{:keys [start end expr] :as expr-info}]
  {:start 1
   :end   (inc (- end start))
   :expr  (-> expr char-counting-push-back-reader r/read)})

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
