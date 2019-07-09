(ns clj-crm.db.report
  (:require [clj-crm.db.user :as duser]
            [clj-crm.db.revenue :as drevenue]
            [datomic.api :as d]
            [clj-crm.db.core :refer [conn]]
            [clojure.string :as string]))

;; stream-revenues-db (mapcat #(u-eid->stream-revenues db %) u-eids)

(defn- user-view
  [db ocdut]
  (let [[o c d u-name t-keyword] ocdut]
    {:user/name u-name
     :team/name (name t-keyword)}))

(defn- order-view
  [db ocdut]
  (let [[o c d u-name t-keyword] ocdut]
    (d/pull db '[:order/product-unique-id
                 :order/io-writing-time] o)))

(defn- u-eid->ou-tuples
  [db u-eid]
  (->> (drevenue/u-eid->orders db u-eid)
       (map #(vector (first %) u-eid))))

(defn- ou-tuples->ocdut-tuples
  [db ou-ts]
  (d/q '[:find ?o ?c ?d ?u-name ?t-keyword
         :in $ [[?o ?u]]
         :where
         [?u :user/name ?u-name]
         [?u :user/team ?t]
         [?t :db/ident ?t-keyword]
         [?o :order/customer ?c]
         [?o :order/channel ?d]]
       db ou-ts))

(defn- u-eids->order-full-join-reports
  " orders-db is the form [[o p-keyword c pui]]"
  [db u-eids]
  (let [ou-tuples (mapcat #(u-eid->ou-tuples db %) u-eids)
        ocdut-tuples (ou-tuples->ocdut-tuples db ou-tuples)]
    (let [user-xs (map #(user-view db %) ocdut-tuples)
          order-xs (map #(order-view db %) ocdut-tuples)]
      (map merge user-xs order-xs))))


;; Public API


(defn u-eids->full-join-reports
  [db u-eids]
  (let [o-r (u-eids->order-full-join-reports db u-eids)]
    (concat o-r [])))
