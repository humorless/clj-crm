(ns clj-crm.etl.target
  (:require
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clojure.string :as string]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(defn- tp-fmt?
  "Valid time period format is `2019 Q1`"
  [s]
  (re-matches #"[0-9]{4} Q[1-4]{1}" s))

(spec/def ::time-period tp-fmt?)
(spec/def ::sales string?)
(spec/def ::target double?)

(spec/def ::rev-target
  (spec/*
   (spec/keys :req-un
              [::time-period ::sales ::target])))

(def ^:private columns-map
  {:A :time-period
   :B :sales
   :C :target})

(defn- excel-fmt->db-fmt
  [s]
  (let [l-s (clojure.string/lower-case s)
        [y q] (string/split l-s #"\s")]
    (str y "-" q)))

(defn- basic-mapping
  [{tp :time-period r :target}]
  {:target/year-quarterly (excel-fmt->db-fmt tp)
   :target/revenue (long r)})

(defn- user-mapping
  [table {u :sales}]
  (if-some [u-eid (get table u)]
    {:target/user u-eid}
    (throw (ex-info "u-eid is nil" {:causes u :desc "u-eid not matched"}))))

(defn- data->data-txes
  [data]
  (let [db (d/db conn)
        table (utility/user-name->u-eid db)]
    (let [basic-xs (map basic-mapping data)
          user-xs  (map #(user-mapping table %) data)]
      (->> (map merge basic-xs user-xs)
           (mapv #(vector :fn/upsert-target %))))))

(def ^:private check-raw
  (utility/check-raw-fn ::rev-target))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "target.xlsx")))
(comment (def raw
           (get-raw-from-excel "/home/vagrant/clo/clj-crm/etl_test/" "dev_target.xlsx")))
(comment (def data
           (data->data-txes raw)))
(comment (utility/fn-txes->pure-txes :fn/upsert-target data))
