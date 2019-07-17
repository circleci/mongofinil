(defproject circleci/mongofinil "0.2.21"
  :description "A library for Mongoid-like models"
  :dependencies [[congomongo "1.1.0"]
                 [clj-time "0.14.0"]
                 [org.clojure/tools.logging "0.4.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure ~(or (System/getenv "CLOJURE_VERSION")
                                                            "1.8.0")]
                                  [circleci/bond "0.3.1"]
                                  [midje "1.9.9" :exclusions [clj-time
                                                              org.clojure/clojure]]]}}
  :plugins [[lein-midje "3.2.1"]])
