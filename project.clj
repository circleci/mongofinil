(defproject mongofinil "0.1.11"
  :description "A library for Mongoid-like models"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/clojure-contrib "1.3-b7125d79301cbc1ce44b24c5b29e57685202041a"]

                 ;; DB
                 [congomongo "0.1.8"]

                 ;; Misc
                 [swank-clojure "1.3.4"]

                 [midje "1.3.1"]]

  :dev-dependencies [[lein-test-out "0.1.1"]
                     [midje "1.3.1" :exclusions [org.clojure/clojure]]
                     [lein-midje "1.0.7"]
                     [clojure-source "1.2.1"]])
