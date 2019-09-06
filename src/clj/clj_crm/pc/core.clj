(ns clj-crm.pc.core
  (:require
   [datomic.api :as d]
   [clojure.tools.logging :as log]
   [taoensso.nippy :as nippy]
   [clj-crm.fjr.core :as fjr]
   [clj-crm.config :refer [env]]
   [clj-crm.db.user :as duser]
   [clj-crm.db.revenue :as drevenue]
   [clj-crm.db.core :as dcore]))

(defn delete-db []
  (d/delete-database (:auxi-database-url env)))

(defn- ->time-span-str [time-span]
  (apply str (-> time-span vec sort)))

(defn encode-and-store
  [team tx time-span data]
  (log/info "encode-and-store with " team tx time-span)
  (let [ts (->time-span-str time-span)
        v-bytes (nippy/freeze data)]
    @(d/transact dcore/auxi-conn
                 [{:booking/team team :booking/tx tx
                   :booking/time-span ts :booking/bytes v-bytes}])))

(defn load-and-decode
  [team tx time-span]
  (log/info "load-and-decode with " team tx time-span)
  (let [db (d/db dcore/auxi-conn)
        ts (->time-span-str time-span)
        v (d/q '[:find ?b .
                 :in $ ?team-n ?tx ?ts
                 :where
                 [?e :booking/team ?team-n]
                 [?e :booking/tx ?tx]
                 [?e :booking/time-span ?ts]
                 [?e :booking/bytes ?b]]
               db team tx ts)]
    (if (nil? v)
      v
      (nippy/thaw v))))

(defn time-span-filter-fn
  [time-span o-t-e r-t-e]
  (fn [_  datom]
    (cond
      (and (= o-t-e (.a datom)) (not (contains? time-span (.v datom)))) false
      (and (= r-t-e (.a datom)) (not (contains? time-span (.v datom)))) false
      :else true)))

(defn all-compute-and-store
  [tx db* time-span]
  (let [order-time-eid (d/entid db* :accounting/month)
        revenue-time-eid (d/entid db* :rev-stream/accounting-time)
        t-f (time-span-filter-fn time-span order-time-eid revenue-time-eid)
        db (d/filter db* t-f)]
    (let [eids (duser/sales-eids db)
          {other-stream-ru-tuples :stream other-order-ru-tuples :order} (drevenue/u-eids->other-ru-tuples db eids)
          stream-ru-tuples (mapcat #(drevenue/u-eid->stream-ru-tuples db %) eids)
          order-ru-tuples (mapcat #(drevenue/u-eid->order-ru-tuples db %) eids)
          stream-reports (fjr/stream-ru-tuples->full-join-reports db time-span (concat stream-ru-tuples other-stream-ru-tuples))
          order-reports (fjr/order-ru-tuples->full-join-reports db time-span (concat order-ru-tuples other-order-ru-tuples))
          data {:stream stream-reports :order order-reports}]
      (future (encode-and-store "null" tx time-span data))
      data)))

(defn my-compute-and-store
  [tx db* time-span user-lookup-ref teamName]
  (let [order-time-eid (d/entid db* :accounting/month)
        revenue-time-eid (d/entid db* :rev-stream/accounting-time)
        t-f (time-span-filter-fn time-span order-time-eid revenue-time-eid)
        db (d/filter db* t-f)]
    (let [u-eids (duser/u-eid->same-team-u-eids db user-lookup-ref)
          stream-ru-tuples (mapcat #(drevenue/u-eid->stream-ru-tuples db %) u-eids)
          order-ru-tuples (mapcat #(drevenue/u-eid->order-ru-tuples db %) u-eids)
          stream-reports (fjr/stream-ru-tuples->full-join-reports db time-span stream-ru-tuples)
          order-reports (fjr/order-ru-tuples->full-join-reports db time-span order-ru-tuples)
          data {:stream stream-reports :order order-reports}]
      (future (encode-and-store teamName tx time-span data))
      data)))
