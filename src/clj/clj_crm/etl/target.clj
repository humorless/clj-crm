(ns clj-crm.etl.target
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clojure.string :as string]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::start-date double?)
(spec/def ::end-date double?)
(spec/def ::sales string?)
(spec/def ::target double?)

(spec/def ::rev-target
  (spec/*
   (spec/keys :req-un
              [::start-date ::end-date ::sales ::target])))

(def ^:private columns-map
  {:A :start-date
   :B :end-date
   :C :sales
   :D :target})

(defn- str->inst*
  [ds]
  (utility/y-m->dt* (str (int ds))))

(defn- str->inst
  [ds]
  (utility/y-m->dt (str (int ds))))

(defn- basic-mapping
  [{sd :start-date ed :end-date r :target}]
  {:target/start-date (str->inst* sd)
   :target/end-date (str->inst ed)
   :target/revenue  (long r)})

(defn- user-mapping
  [table {u :sales}]
  (if-some [u-eid (get table u)]
    {:target/user u-eid}
    (throw (ex-info "u-eid is nil" {:causes u :desc "u-eid not matched"}))))

(defn- data->data-txes
  [data]
  (let [db (d/db conn)
        table (utility/user-name->u-eid db)
        sdata (set data)]
    (let [basic (map basic-mapping sdata)
          user  (map #(user-mapping table %) sdata)]
      (map merge basic user))))

(def ^:private check-raw
  (utility/check-raw-fn ::rev-target))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment (def raw
           (get-raw-from-excel "/home/vagrant/clo/clj-crm/etl_test/" "dev_target.xlsx")))
