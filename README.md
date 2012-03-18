# A No-RM for Clojure and Mongo.

Mongofinil reads a declarative specification of your models, and generates a namespaced DSL for that model.

It takes some inspiration from Mongoid, from the Rails world, but attempts to be idiomatic clojure.


### Installation

```clojure
    [mongofinil "0.1.11"]
```

### Usage

First declare your model:

```clojure
(ns circle.model.user
  (:require [mongofinil.core :as mongofinil]))

(mongofinil/defmodel :users
  :fields
  [{:name :admin :default false}
   {:name :login :required true :findable true}
   {:name :bot :default false}
   {:name :emails :findable true :default []}
   {:name :name :findable true}
   {:name :github_access_token :findable true}
   {:name :sent_first_build_email :default false}
   {:name :created_at}])
```


Now use it:

```clojure
(require 'circle.model.user)
(def u (user/find-one-by-email "paul@circleci.com"))
(def u (user/find-by-id "a mongo id")
(user/save! u)
(user/validate! u) ; throws if doesn't match the validators
(user/all)
(user/nu {:email "..." :login "pbiggar"}) ; doesnt save in the DB, does validate
(user/create! {:email "..." :login "..."}) ; does save to DB, does validate
(user/set-fields u {:login "..."}) ; atomically set fields
(user/push! u :alist 5)  ; atomically push into arrays
(user/pull! u :alist 5) ; atomically remove from arrays
```




#### Notes
  - :keyword true returns keywords for keys instead of strings
  - :use-refs returns refs instead of lists and hashes.
  - The order of fields matters for defaults. Defaults are run in the order the fields are listed
  - set-fields! returns the new, updated object. Updates refs if necessary

### TODO
 - defapi
 - Warn if findable fields don't have indexes
   - Add :unique attribute for findable fields, and warn if the index isn't unique


### A plug for our service:

[Circle](http://circleci.com) is an easy to use Continuous Integration service for web apps: [CircleCI.com](http://circleci.com)

### License

MIT
