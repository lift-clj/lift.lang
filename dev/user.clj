(ns user
  (:require
   [clojure.java.shell :as sh]
   [clojure.tools.namespace.repl :refer [refresh]]
   ;; [lift.lang :as lift]
   ;; [lift.lang.inference :as infer]
   ))

(defn load-class [classname]
  (let [class-loader (clojure.lang.DynamicClassLoader.)
        class-reader (clojure.asm.ClassReader. classname)]
    (when class-reader
      (let [bytes (.-b class-reader)]
        (.defineClass class-loader
                      classname
                      bytes
                      "")))))

(def classes
  ["lift.lang.Instance"
   "lift.lang.Tagged"])

(defn reload-java-classes []
  (let [ret (sh/sh "lein" "with-profiles" "-dev" "javac")]
    (if (-> ret :exit zero?)
      (doseq [class classes]
        (load-class class))
      (throw (Exception. (str "Could not compile:\n"
                              (:out ret)
                              (:err ret)))))))

;; (reload-java-classes)
