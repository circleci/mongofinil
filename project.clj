(defproject org.clojars.dlowe/mongofinil "0.1.34"
  :description "A library for Mongoid-like models"
  :dependencies [[org.clojure/clojure "1.3.0"]

                 ;; DB
                 [congomongo "0.1.8"]

                 ;; Misc
                 [clj-time "0.4.4"]
                 [org.clojure/tools.logging "0.2.3"]

                 [bultitude "0.1.7"]
                 [lein-midje "2.0.3"]
                 [midje "1.3.1"]]

  :dev-dependencies [[lein-test-out "0.1.1"]
                     [midje "1.3.1" :exclusions [org.clojure/clojure]]
                     [bultitude "0.1.7"]
                     [lein-midje "2.0.3"]
                     [bond "0.2.3" :exclusions [org.clojure/clojure]]
                     [clojure-source "1.2.1"]]
  :profiles {:dev
             {:dependencies
              [[lein-test-out "0.1.1"]
               [midje "1.4.0" :exclusions [org.clojure/clojure]]
               [bultitude "0.1.7"]
               [lein-midje "2.0.3"]
               [bond "0.2.3" :exclusions [org.clojure/clojure]]]}}

  :plugins [[lein-midje "2.0.3"]])
