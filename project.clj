(defproject circleci/mongofinil "0.2.19"
  :description "A library for Mongoid-like models"

  :dependencies [[org.clojure/clojure "1.10.0-beta7"]
                 [circleci/congomongo "0.4.5-58d755baa9dc5daeab67516058ccd2a590766a2f"]
                 
                 [clj-time "0.9.0"]
                 [circleci/bond "0.3.2"]
                 [midje "1.6.0" :exclusions [org.clojure/clojure]]
                 ]

  ; :profiles {:dev {:dependencies [[clj-time "0.9.0"]
                                  ; [circleci/bond "0.3.2"]]}}

  :plugins [[lein-midje "3.2.1"]])
