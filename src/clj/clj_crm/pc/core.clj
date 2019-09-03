(ns clj-crm.pc.core
  (:require
   [datomic.api :as d]
   [taoensso.nippy :as nippy]
   [clj-crm.fjr.core :as fjr]
   [clj-crm.config :refer [env]]
   [clj-crm.db.revenue :as drevenue]
   [clj-crm.db.core :as dcore]))

(defn delete-db []
  (d/delete-database (:auxi-database-url env)))

(defn encode-and-store
  [tx data]
  (let [v-bytes (nippy/freeze data)]
    @(d/transact dcore/auxi-conn [{:booking/tx tx :booking/bytes v-bytes}])))

(defn load-and-decode
  [tx]
  (let [db (d/db dcore/auxi-conn)
        v (d/q '[:find ?b .
                 :in $ ?t
                 :where [?e :booking/tx ?t]
                 [?e :booking/bytes ?b]]
               db tx)]
    (if (nil? v)
      v
      (nippy/thaw v))))

(defn pre-compute-and-store
  [tx db time-span eids]
  (let [{other-stream-ru-tuples :stream other-order-ru-tuples :order} (drevenue/u-eids->other-ru-tuples db eids)
        stream-ru-tuples (mapcat #(drevenue/u-eid->stream-ru-tuples db %) eids)
        order-ru-tuples (mapcat #(drevenue/u-eid->order-ru-tuples db %) eids)
        stream-reports (fjr/stream-ru-tuples->full-join-reports db time-span (concat stream-ru-tuples other-stream-ru-tuples))
        order-reports (fjr/order-ru-tuples->full-join-reports db time-span (concat order-ru-tuples other-order-ru-tuples))
        data {:stream stream-reports :order order-reports}
        k-tx (if (some? tx) tx (d/t->tx (d/basis-t db)))]
    (encode-and-store k-tx data)))
