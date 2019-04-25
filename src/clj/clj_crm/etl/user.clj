(ns clj-crm.etl.user
  (:require [clojure.set :as cs]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :as dcore :refer [conn]]
            [datomic.api :as d]
            [dk.ative.docjure.spreadsheet :as spreadsheet]
            [clojure.java.io :as io])
  (:import [java.io StringWriter]))

(defn- team-m->team-tx-m
  "m is of the {HashMap} form that just reading the data from excel.
   However, we need to do certain transformation to get tx-map."
  [db m]
  (let [team-name (:user/team m)
        t-eid (d/q '[:find ?e . :in $ ?n :where [?e :team/name ?n]] db team-name)
        r-str (:user/roles m)
        r-keyword (keyword r-str)]
    (assoc m :user/team t-eid :user/roles r-keyword)))

(defn- get-users-from-excel
  "Read the excel file, retrieve the user data,
   and then transform the data into db-transaction-form

  Implementation details:
  rest - remove the title row
  set  - remove duplicated rows"
  [addr filename]
  (with-open [stream (io/input-stream (str addr filename))]
    (->> (spreadsheet/load-workbook stream)
         (spreadsheet/select-sheet "Sheet0")
         (spreadsheet/select-columns {:A :user/email
                                      :B :user/name
                                      :C :user/roles
                                      :D :user/team})
         rest
         (map #(team-m->team-tx-m (d/db conn) %))
         set)))

(defn- u-eid->user+team+role
  "Transfrom user eid -> {HashMap with user fields}"
  [db eid]
  (d/pull db '[:user/email :user/name  {:user/roles [*]} {:user/team [*]}] eid))

(defn- tx-user
  "transform the {HashMap} data into db-transaction-form"
  [m]
  (let [r-keyword (get-in m [:user/roles :db/ident])
        t-eid (get-in m [:user/team :db/id])]
    (assoc m :user/roles r-keyword
           :user/team t-eid)))

(defn- get-users-from-db [db]
  (let [eids (dcore/user-eids db)
        query-result (map #(u-eid->user+team+role db %) eids)
        data (map tx-user  query-result)]
    (set data)))

(defn sync-data
  "From Excel file, get the current users
   From DB, get the users inside DB.
   Calculate the difference. Find out the new users in Excel but not in DB.
   Write into database"
  [url filename]
  (log/info "etl.user sync-data triggered!")
  (let [e-user-rel (get-users-from-excel url filename)
        d-user-rel (get-users-from-db (d/db conn))
        tx-data (vec (cs/difference e-user-rel d-user-rel))]
    (do (log/info "etl.user tx-data write into db, length: " (count tx-data))
        (log/info "etl.user first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
