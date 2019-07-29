(ns clj-crm.etl.utility
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [dk.ative.docjure.spreadsheet :as spreadsheet]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as spec]
   [clj-time.format :as time.format]
   [clj-time.coerce :as time.coerce]
   [clj-time.core :as time.core]))

(defn compact
  "remove the nil value key from a hashmap

  (compact {:a nil :b 1})
  ;; => {:b 1}"
  [m]
  (into {} (remove #(nil? (val %)) m)))

(def ^:private td-fmt-y-m (time.format/formatter "yyyyMM"))

(defn yearmonth->year-month
  "change `201905` to `2019-05`"
  [in-str]
  {:pre [(string? in-str)]}
  (let [front (apply str (take 4 in-str))
        back (apply str (take-last 2 in-str))]
    (str front "-" back)))

(defn y-m->dt* [y-m-str]
  {:pre [(string? y-m-str)]}
  (let [f-d (time.format/parse td-fmt-y-m y-m-str)]
    (time.coerce/to-date f-d)))

(defn y-m->dt [y-m-str]
  {:pre [(string? y-m-str)]}
  (let [f-d (time.format/parse td-fmt-y-m y-m-str)
        l-d (time.core/last-day-of-the-month f-d)
        boundary (time.core/plus l-d (time.core/days 1))
        tz-boundary (time.core/from-time-zone boundary (time.core/time-zone-for-offset 8))]
    (time.coerce/to-date tz-boundary)))

(defn service-category->enum
  "create a mapping table that can lookup enum from service category name."
  [db]
  (into {}  (d/q '[:find ?name ?enum
                   :where
                   [?e :product/type-id ?name]
                   [?e :product/type ?t]
                   [?t :db/ident ?enum]]
                 db)))

(defn user-name->u-eid
  "create a mapping table that can lookup user-eid from user name."
  [db]
  (into {} (d/q '[:find ?name ?e
                  :where
                  [?e :user/name ?name]]
                db)))

(defn tax-id->c-eid
  "create a mapping table that can lookup customer-eid from customer tax-id."
  [db]
  (into {} (d/q '[:find ?tax-id ?e
                  :where
                  [?e :customer/tax-id ?tax-id]]
                db)))

(defn neon-code->c-eid
  "create a mapping table that can lookup customer-eid from customer neon-code."
  [db]
  (into {} (d/q '[:find ?nc ?e
                  :where
                  [?e :customer/neon-code ?nc]]
                db)))

(defn check-raw-fn
  "assemble schema and validate fn"
  [schema]
  (fn check-raw
    [data]
    (if (spec/valid? schema data)
      data
      (let [desc (spec/explain-str schema data)]
        (throw (ex-info desc {:causes data :desc desc}))))))

(defn get-raw-from-excel-fn*
  "assemble columns-map and get-raw-from-excel fn
   The * means `get` title and data"
  [columns-map]
  (fn get-raw-from-excel
    [addr filename]
    (try
      (with-open [stream (io/input-stream (str addr filename))]
        (let [title+orders (->> (spreadsheet/load-workbook stream)
                                (spreadsheet/select-sheet "Sheet0")
                                (spreadsheet/select-columns columns-map))]
          title+orders))
      (catch Exception e
        (let [desc "get-raw-from-excel error"]
          (throw (ex-info desc (Throwable->map e))))))))

(defn get-raw-from-excel-fn
  "assemble columns-map and get-raw-from-excel fn"
  [columns-map]
  (comp rest (get-raw-from-excel-fn* columns-map)))

(defn sync-data-fn
  "assemble get, validate, transform, push to db"
  [get-raw-from-excel check-raw data->tx-data]
  (fn sync-data
    [url filename]
    (log/info "sync-data triggered!")
    (let [raw (get-raw-from-excel url filename)
          _ (check-raw raw) ;; schema validation, but it may `rest` on raw
          tx-data (data->tx-data raw)]
      (do (log/info "tx-data write into db, length: " (count tx-data))
          (log/info "first item of tx-data" (first tx-data))
          (when (seq tx-data)
            @(d/transact conn tx-data))))))

(defn fn-txes->pure-txes
  "Example of fn-k  :fn/upsert-target"
  [fn-k data]
  (let [db (d/db conn)
        f (dcore/query-db-fn db fn-k)
        data-only (map second data)]
    (mapv #(f db %) data-only)))
