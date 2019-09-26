(ns clj-crm.pc.core
  (:require
   [datomic.api :as d]
   [clojure.tools.logging :as log]
   [taoensso.nippy :as nippy]
   [clojure.set :as cset]
   [clj-crm.fjr.core :as fjr]
   [clj-crm.config :refer [env]]
   [clj-crm.pc.revenue :as pc-r]
   [clj-crm.db.user :as duser]
   [clj-crm.db.revenue :as drevenue]
   [clj-crm.db.core :as dcore :refer [conn]]))

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

(defn- all-rev-stream-eids [db]
  (into #{}
        (d/q '[:find [?e ...]
               :where [?e :rev-stream/accounting-time]]
             db)))

(defn- this-time-span-rev-stream-eids [db time-span]
  (into #{}
        (d/q '[:find [?e ...]
               :in $ [?t ...]
               :where [?e :rev-stream/accounting-time ?t]]
             db time-span)))

;; How to verify
;; (not-this-rev-stream-eids (d/db conn) #{"2019-02" "2019-03"})

(defn- not-this-rev-stream-eids [db time-span]
  (let [eids-s1 (all-rev-stream-eids db)
        eids-s2 (this-time-span-rev-stream-eids db time-span)]
    (cset/difference eids-s1 eids-s2)))

(defn- time-span-filter-fn
  [discard-eids]
  (fn [_  datom]
    (cond
      (contains? discard-eids (.e datom)) false
      :else true)))

(defn- db-exclude-rev-stream-outside-time-span
  [db time-span]
  (let [discard-eids (not-this-rev-stream-eids db time-span)
        t-f (time-span-filter-fn discard-eids)]
    (d/filter db t-f)))

;; How to verify
;; (seq (d/datoms filtered-db :aevt :rev-stream/accounting-time))

(defn all-compute-and-store
  [tx db* time-span]
  (let [db (db-exclude-rev-stream-outside-time-span db* time-span)]
    (binding [pc-r/*time-span* time-span pc-r/*tx* tx]
      (let [eids (duser/sales-eids db)
            {other-stream-ru-tuples :stream other-order-ru-tuples :order} (doall (drevenue/u-eids->other-ru-tuples db eids))
            stream-ru-tuples (doall (mapcat #(drevenue/u-eid->stream-ru-tuples db %) eids))
            order-ru-tuples (doall (mapcat #(drevenue/u-eid->order-ru-tuples db %) eids))
            stream-reports (fjr/stream-ru-tuples->full-join-reports db time-span (concat stream-ru-tuples other-stream-ru-tuples))
            order-reports (fjr/order-ru-tuples->full-join-reports db time-span (concat order-ru-tuples other-order-ru-tuples))
            data {:stream stream-reports :order order-reports}]
        (future (encode-and-store "null" tx time-span data))
        data))))

(defn my-compute-and-store
  [tx db* time-span user-lookup-ref teamName]
  (let [db (db-exclude-rev-stream-outside-time-span db* time-span)]
    (binding [pc-r/*time-span* time-span pc-r/*tx* tx]
      (let [u-eids (duser/u-eid->same-team-u-eids db user-lookup-ref)
            stream-ru-tuples (doall (mapcat #(drevenue/u-eid->stream-ru-tuples db %) u-eids))
            order-ru-tuples (doall (mapcat #(drevenue/u-eid->order-ru-tuples db %) u-eids))
            stream-reports (fjr/stream-ru-tuples->full-join-reports db time-span stream-ru-tuples)
            order-reports (fjr/order-ru-tuples->full-join-reports db time-span order-ru-tuples)
            data {:stream stream-reports :order order-reports}]
        (future (encode-and-store teamName tx time-span data))
        data))))
