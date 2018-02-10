;; (ns lift.t.core
;;   (:require [clojure.tools.reader :as r]))

;; ;; there are some problems:
;; ;; 1) how to load through into other namespaces?
;; ;; 2) how to use the right language?
;; ;; 3) how to seamlessly repl? - tooling

;; ;; (defmacro require [& decl])

;; ;; what's the goal?
;; ;; want to protect code
;; ;; want to do type based implementation choice
;; ;; hmm, can we even do that? Maybe it can only happen at runtime?
;; ;;


;; (require '[clojure.tools.reader :as r])
;; (require '[clojure.tools.reader.reader-types :refer [read-char unread]])
;; (require '[clojure.tools.reader.impl.utils :refer [whitespace?]])

;; (defn read-colon [reader initch opts pending-forms]
;;   (let [ch (read-char reader)]
;;     (if (or (nil? ch)
;;             (whitespace? ch)
;;             (#'r/macro-terminating? ch))
;;       (symbol ":")
;;       (do
;;         (unread reader ch)
;;         (#'r/read-keyword reader initch opts pending-forms)))))

;; (alter-var-root
;;  #'clojure.tools.reader/macros
;;  (fn [f]
;;    (fn [ch]
;;      (if (= ch \:)
;;        read-colon
;;        (f ch)))))
