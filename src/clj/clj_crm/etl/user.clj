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
        pwd-hash (hs/derive pwd-str)
        c-str (:user/channel m)
        c-ident (keyword c-str)]
    (if c-ident
      (assoc m :user/team t-ident :user/roles r-ident
             :user/pwd pwd-hash :user/channel c-ident)
      (assoc (dissoc m :user/channel)
             :user/team t-ident :user/roles r-ident
             :user/pwd pwd-hash))))

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
                                      :E :user/pwd
                                      :F :user/channel}) ;; Column F is nullable
         rest
         (map #(team-m->team-tx-m (d/db conn) %))
         set)))

(defn- u-eid->fields
  "Transfrom user eid -> {HashMap with user fields}"
  [db eid]
  (d/pull db '[:user/email :user/name  {:user/roles [*]}
               {:user/team [*]} {:user/channel [*]} :user/pwd] eid))

(defn- tx-user
  "transform the {HashMap} data into db-transaction-form"
  [m]
  (let [r-ident (get-in m [:user/roles :db/ident])
        t-ident (get-in m [:user/team :db/ident])
        c-ident (get-in m [:user/channel :db/ident])]
    (if c-ident ;; when c-ident is nil, skip this attribute
      (assoc m :user/roles r-ident
             :user/team t-ident
             :user/channel c-ident)
      (assoc m :user/roles r-ident
             :user/team t-ident))))

(defn- get-users-from-db [db]
  (let [eids (dcore/user-eids db)
        query-result (map #(u-eid->fields db %) eids)
        data (map tx-user  query-result)]
    (set data)))

(defn sync-data
  "From Excel file, get the current users
   Write into database"
  [url filename]
  (log/info "sync-data triggered!")
  (let [e-user-rel (get-users-from-excel url filename)
        tx-data (vec e-user-rel)]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
