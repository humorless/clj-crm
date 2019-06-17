(ns clj-crm.db.user
  (:require [datomic.api :as d]))

(defn u-eid->teamName
  [db eid]
  (-> (d/q '[:find ?t-keyword .
             :in $ ?u-eid
             :where
             [?u-eid :user/team ?t-eid]
             [?t-eid :db/ident ?t-keyword]]
           db eid)
      name))

(defn- fmt-user-tuple
  [[u-name t-keyword]]
  (vector u-name (name t-keyword)))

(defn u-eid->userName-teamName-tuple
  "Auxiliary data functions used to retrieve necessary user/team name."
  [db eid]
  (-> (d/q '[:find [?u-name ?t-keyword]
             :in $ ?u-eid
             :where
             [?u-eid :user/name ?u-name]
             [?u-eid :user/team ?t-eid]
             [?t-eid :db/ident ?t-keyword]]
           db eid)
      fmt-user-tuple))

(defn t-eid->sales-eids
  "Find out all the u-eids belongs to this team
   The user role must be sales"
  [db eid]
  (d/q '[:find [?u-eid ...]
         :in $ ?t-eid
         :where
         [?u-eid :user/team ?t-eid]
         [?u-eid :user/roles :user.roles/sales]]
       db eid))

(defn sales-eids
  [db]
  (d/q '[:find [?u-eid ...]
         :in $
         :where
         [?u-eid :user/roles :user.roles/sales]] db))

(defn u-eid->chan-type
  [db u-eid]
  (d/q '[:find ?chan-keyword .
         :in $ ?u
         :where
         [?u :user/channel ?c]
         [?c :db/ident ?chan-keyword]]
       db u-eid))
