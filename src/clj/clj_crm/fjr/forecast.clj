(ns clj-crm.fjr.forecast
  (:require [clj-crm.db.user :as duser]
            [datomic.api :as d]
            [clj-crm.db.core :refer [conn]]
            [clojure.string :as string]))

(defn- pipeline-view
  [db [eid t-d u-n p-d]]
  (let [datum (d/pull db '[*] eid)
        t-n (name t-d)
        p-n (name p-d)]
    (merge datum
           {:teamName t-n
            :userName u-n
            :productName p-n})))

(defn pipeline-reports
  [db q-span eids]
  (->> (d/q '[:find ?e ?t-d ?u-n ?p-d
              :in $ ?q [?s ...]
              :where [?e :pipeline/year-quarterly ?q]
              [?e :pipeline/sales ?s]
              [?s :user/name ?u-n]
              [?s :user/team ?t]
              [?t :db/ident ?t-d]
              [?e :pipeline/product ?p]
              [?p-e :product/type ?p]
              [?p-e :product/type-id ?p-d]]
            db q-span eids)
       (map #(pipeline-view db %))))

(defn- target-view
  [db [eid t-d u-n]]
  (let [datum (d/pull db '[:target/year-quarterly :target/revenue] eid)
        t-n (name t-d)]
    (merge datum
           {:teamName t-n
            :userName u-n})))

(defn target-reports
  [db q-span eids]
  (->> (d/q '[:find ?e ?t-d ?u-n
              :in $ ?q [?s ...]
              :where [?e :target/year-quarterly ?q]
              [?e :target/user ?s]
              [?s :user/name ?u-n]
              [?s :user/team ?t]
              [?t :db/ident ?t-d]]
            db q-span eids)
       (map #(target-view db %))))
