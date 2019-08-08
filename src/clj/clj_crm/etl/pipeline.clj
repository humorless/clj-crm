(ns clj-crm.etl.pipeline
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clojure.string :as string]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::time-period string?)
(spec/def ::sales string?)
(spec/def ::product string?)
(spec/def ::channel (spec/nilable string?))
(spec/def ::client (spec/nilable string?))
(spec/def ::campaign-name (spec/nilable string?))
(spec/def ::revenue (spec/nilable double?))
(spec/def ::prob (spec/nilable double?))
(spec/def ::status (spec/nilable string?))
(spec/def ::note (spec/nilable string?))

(spec/def ::pipeline
  (spec/*
   (spec/keys :req-un
              [::time-period ::sales ::product ::channel ::client
               ::campaign-name ::revenue ::prob ::status ::note])))

(def ^:private columns-map
  {:A :time-period
   :C :sales
   :D :product
   :E :channel
   :F :client
   :G :campaign-name
   :I :revenue
   :J :prob
   :K :status
   :M :note})

(defn- u-mapping
  [table {u :sales}]
  (let [u-eid (get table u)]
    (when (nil? u-eid)
      (throw (ex-info "u-eid is nil" {:causes u :desc "user name not matched"})))
    {:pipeline/sales u-eid}))

(defn- p-mapping
  [table {sc :product}]
  (let [p-enum (get table sc)]
    (when (nil? p-enum)
      (throw (ex-info "p-enum is nil" {:causes sc :desc "product not matched"})))
    {:pipeline/product p-enum}))

(defn- excel-fmt->db-fmt
  [s]
  (let [l-s (clojure.string/lower-case s)
        [y q] (string/split l-s #"\s")]
    (str y "-" q)))

(defn- basic-mapping
  ""
  [{t-p :time-period chan :channel c :client
    c-name :campaign-name r :revenue p :prob
    s :status n :note}]
  (utility/compact
   {:pipeline/year-quarterly (excel-fmt->db-fmt t-p)
    :pipeline/sales-channel-name chan
    :pipeline/customer-name c
    :pipeline/campaign-name c-name
    :pipeline/revenue (if (nil? r) 0 (long r))
    :pipeline/prob (if (nil? p) 0 p)
    :pipeline/status s
    :pipeline/note n}))

(defn- data->data-txes
  [data]
  (let [db (d/db conn)
        u-table (utility/user-name->u-eid db)
        p-table (utility/service-category->enum db)]
    (let [basic-xs (map basic-mapping data)
          user-xs  (map #(u-mapping u-table %) data)
          product-xs (map #(p-mapping p-table %) data)]
      (mapv merge basic-xs user-xs product-xs))))

(def ^:private check-raw
  (utility/check-raw-fn ::pipeline))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "pipeline.xlsx")))
