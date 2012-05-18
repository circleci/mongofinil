(ns mongofinil.test-core
  (:require [somnium.congomongo :as congo]
            [mongofinil.core :as core]
            [mongofinil.testing-utils :as utils])
  (:use midje.sweet
        [clojure.core.incubator :only (-?>)])
  (:import org.bson.types.ObjectId))


(utils/setup-test-db)
(utils/setup-midje)

(core/defmodel :xs
  :fields [{:name :x :findable true}]
  :hooks {:update (fn [row]
                    (assoc row :hook-count (-> row :hook-count (or 0) inc)))})

;.;. First they ignored you, then they laughed at you, then they fought you, now
;.;. you've won. -- Not quite Gandhi
(fact "hooks are triggered"
  (let [orig (create! {})]
    orig => (contains {:hook-count 1})
    (set-fields! orig {:x :y}) => (contains {:hook-count 2})
    (add-to-set! orig :a :y) => (contains {:hook-count 2 :a ["y"]})))
