(ns user
  (:require
   [clojure.tools.namespace.repl :refer [refresh]]
   [lift.lang :as lift]
   [lift.lang.inference :as infer]))

;; (def dcl (clojure.lang.DynamicClassLoader.))

;; (defn dynamically-load-class! [class-loader class-name]
;;   (let [class-reader (clojure.asm.ClassReader. ^String class-name)]
;;     (when class-reader
;;       (let [bytes (.-b class-reader)]
;;         (.defineClass class-loader
;;                       class-name
;;                       bytes
;;                       "")))))

;; (dynamically-load-class! dcl "lift.lang.Type")
;; (dynamically-load-class! dcl "lift.lang.Instance")
