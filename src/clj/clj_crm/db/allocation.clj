(ns clj-crm.db.allocation
  (:require
   [clj-crm.db.core :refer [conn]]
   [datomic.api :as d]))


;; Allocation export service API


(defn allo-eids [db]
  (d/q '[:find [?e ...]
         :in $
         :where [?e :allo/sales _]] db))

(defn allo-direct-eids [db]
  (d/q '[:find [?e ...]
         :in $
         :where [?e :allo/sales _]
         [?e :allo/product :product.cat/all]] db))

(defn allo-non-direct-eids [db]
  (d/q '[:find [?e ...]
         :in $
         :where [?e :allo/sales _]
         (not [?e :allo/product :product.cat/all])] db))

(defn allo-eid->allocation
  "Transfrom allocation eid -> {HashMap with allocation fields}"
  [db eid]
  (d/pull db '[{:allo/sales [:user/email]}
               {:allo/customer [:customer/id :customer/name]}
               {:allo/product [:db/ident]}
               :allo/time] eid))

;; rev-allo export service API

(defn rev-allo-eids [db]
  (d/q '[:find [?e ...]
         :in $
         :where [?e :rev-allo/sales _]] db))

(defn rev-allo-eid->allocation
  "Transfrom rev-allo eid -> {HashMap with allocation fields}"
  [db eid]
  (d/pull db '[{:rev-allo/sales [:user/email]}
               :rev-allo/customer-id
               {:rev-allo/customer [:customer/id :customer/name]}
               :rev-allo/time
               :rev-allo/source] eid))
