(ns clj-crm.etl.user
  (:require [clojure.set :as cs]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :as dcore :refer [conn]]
            [datomic.api :as d]
            [dk.ative.docjure.spreadsheet :as spreadsheet]
            [clojure.java.io :as io]
            [buddy.hashers :as hs])
  (:import [java.io StringWriter]))

(defn- team-m->team-tx-m
  "m is of the {HashMap} form that just reading the data from excel.
   However, we need to do certain transformation to get tx-map."
  [db m]
  (let [t-str (:user/team m)
        t-ident (keyword t-str)
        r-str (:user/roles m)
        r-ident (keyword r-str)
        pwd-str (:user/pwd m)
        pwd-hash (hs/derive pwd-str)]
    (assoc m :user/team t-ident :user/roles r-ident
           :user/pwd pwd-hash)))

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
                                      :D :user/team
                                      :E :user/pwd})
         rest
         (map #(team-m->team-tx-m (d/db conn) %))
         set)))

(defn- u-eid->user+team+role+pwd
  "Transfrom user eid -> {HashMap with user fields}"
  [db eid]
  (d/pull db '[:user/email :user/name  {:user/roles [*]} {:user/team [*]} :user/pwd] eid))

(defn- tx-user
  "transform the {HashMap} data into db-transaction-form"
  [m]
  (let [r-ident (get-in m [:user/roles :db/ident])
        t-ident (get-in m [:user/team :db/ident])]
    (assoc m :user/roles r-ident
           :user/team t-ident)))

(defn- get-users-from-db [db]
  (let [eids (dcore/user-eids db)
        query-result (map #(u-eid->user+team+role+pwd db %) eids)
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
