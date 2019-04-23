(defproject circleci/mongofinil "0.2.20"
  :description "A library for Mongoid-like models"
  :dependencies [[org.clojure/clojure "1.6.0"]

                 ;; DB
                 [circleci/congomongo "0.4.5-58d755baa9dc5daeab67516058ccd2a590766a2f"]

                 ;; Misc
                 [clj-time "0.9.0"]
                 [org.clojure/tools.logging "0.2.3"]

                 [bultitude "0.1.7"]
                 [lein-midje "3.0.1"]
                 [midje "1.5.1"]]

  :dev-dependencies [[lein-test-out "0.1.1"]
                     [midje "1.5.1" :exclusions [org.clojure/clojure]]
                     [bultitude "0.1.7"]
                     [lein-midje "3.0.1"]
                     [bond "0.2.6" :exclusions [org.clojure/clojure]]
                     [clojure-source "1.2.1"]]
  :profiles {:dev
             {:dependencies
              [[lein-test-out "0.1.1"]
               [midje "1.5.1" :exclusions [org.clojure/clojure]]
               [bultitude "0.1.7"]
               [lein-midje "3.0.1"]
               [bond "0.2.6" :exclusions [org.clojure/clojure]]]}}

  :plugins [[lein-midje "3.0.1"]])
