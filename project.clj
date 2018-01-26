(defproject lift/lift.lang "0.1.0"
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/tools.reader "1.1.1"]
                 [org.clojure/test.check "0.10.0-alpha2"]
                 [rhizome "0.2.9"]
                 [org.clojure/data.xml "0.0.8"]
                 [riddley "0.1.14"]
                 [lift/f "0.1.0"]]
  :profiles {:dev {:source-paths ["dev"]}}
  ;;:repl-options {:nrepl-middleware [lift.lang/repl]}
  )
