(ns clj-crm.pc.revenue
  (:require
   [datomic.api :as d]
   [clojure.tools.logging :as log]
   [taoensso.nippy :as nippy]
   [clj-crm.db.core :as dcore]))

;; *time-span* and *tx* are bound by pc.core namespace
(def ^:dynamic *time-span* nil)
(def ^:dynamic *tx* nil)

(defn- ->time-span-str [time-span]
  (apply str (-> time-span vec sort)))

(defn revenues-encode-and-store
  [u-eid r-type tx time-span data]
  (log/info "encode-and-store revenues with " u-eid r-type tx time-span)
  (let [ts (->time-span-str time-span)
        v-bytes (nippy/freeze data)]
    @(d/transact dcore/auxi-conn
                 [{:revenues/u-eid u-eid :revenues/type r-type :revenues/tx tx
                   :revenues/time-span ts :revenues/bytes v-bytes}])))

(defn revenues-load-and-decode
  [u-eid r-type tx time-span]
  (log/info "load-and-decode revenues with " u-eid r-type tx time-span)
  (let [db (d/db dcore/auxi-conn)
        ts (->time-span-str time-span)
        v (d/q '[:find ?b .
                 :in $ ?u ?t ?tx ?ts
                 :where
                 [?e :revenues/u-eid ?u]
                 [?e :revenues/type ?t]
                 [?e :revenues/tx ?tx]
                 [?e :revenues/time-span ?ts]
                 [?e :revenues/bytes ?b]]
               db u-eid r-type tx ts)]
    (if (nil? v)
      v
      (nippy/thaw v))))
