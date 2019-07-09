(ns clj-crm.db.report
  (:require [clj-crm.db.user :as duser]
            [clj-crm.db.revenue :as drevenue]
            [datomic.api :as d]
            [clj-crm.db.core :refer [conn]]
            [clojure.string :as string]))

;; stream-revenues-db (mapcat #(u-eid->stream-revenues db %) u-eids)

(defn- order-full-join
  [db-all db-order]
  (d/q '[:find ?u-name ?t-keyword
         ?src ?pui ?o-cam-no ?o-cam-name ?o-cam-status
         ?io-t ?s-enum ?sd ?ed ?np
         ?c-id ?c-name-en ?c-name ?c-tax-id ?b-type
         ?d-id ?d-name-en ?d-name ?d-tax-id
         :in $A $O
         :where [$O ?o ?u]
         [$A ?u :user/name ?u-name]
         [$A ?u :user/team ?t]
         [$A ?t :db/ident ?t-keyword]
         [$A ?o :order/source ?src]
         [$A ?o :order/product-unique-id ?pui]
         [$A ?o :order/campaign-no ?o-cam-no]
         [$A ?o :order/campaign-name ?o-cam-name]
         [$A ?o :order/campaign-status ?o-cam-status]
         [$A ?o :order/io-writing-time ?io-t]
         [$A ?o :order/service-category-enum ?s-enum]
         [$A ?o :order/terms-start-date ?sd]
         [$A ?o :order/terms-end-date ?ed]
         [$A ?o :order/product-net-price ?np]
         [$A ?o :order/customer ?c]
         [$A ?c :customer/id ?c-id]
         [$A ?c :customer/name-en ?c-name-en]
         [$A ?c :customer/name ?c-name]
         [$A ?c :customer/tax-id ?c-tax-id]
         [$A ?c :customer/business-type ?b-type]
         [$A ?o :order/channel ?d]
         [$A ?d :customer/id ?d-id]
         [$A ?d :customer/name-en ?d-name-en]
         [$A ?d :customer/name ?d-name]
         [$A ?d :customer/tax-id ?d-tax-id]]
       db-all db-order))

(defn- u-eid->ou-tuples
  [db u-eid]
  (->> (drevenue/u-eid->orders db u-eid)
       (map #(vector (first %) u-eid))))

(defn- u-eids->order-full-join-reports
  " orders-db is the form [[o p-keyword c pui]]"
  [db u-eids]
  (let [orders-db (mapcat #(u-eid->ou-tuples db %) u-eids)
        order-fj (order-full-join db orders-db)]
    order-fj))

;; Public API
(comment
  (defn u-eids->full-join-reports
    [db u-eids]
    (let [o-r (u-eids->order-full-join-reports db u-eids)
          s-r (u-eids->stream-full-join-reports db u-eids)]
      (concat o-r s-r))))
