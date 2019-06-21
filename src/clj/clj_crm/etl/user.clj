(ns clj-crm.etl.user
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [dk.ative.docjure.spreadsheet :as spreadsheet]
   [clojure.java.io :as io]
   [buddy.hashers :as hs]
   [clojure.spec.alpha :as spec]))

(spec/def ::email string?)
(spec/def ::name string?)
(spec/def ::roles string?)
(spec/def ::team string?)
(spec/def ::pwd string?)
(spec/def ::channel (spec/nilable string?))

(spec/def ::user
  (spec/keys :req-un
             [::email ::name ::roles ::team ::pwd ::channel]))

(defn- check-users [state]
  (if (every? #(spec/valid? ::user %) state)
    state
    (throw (ex-info "schema error of user" {:causes state
                                            :desc "user schema error"}))))

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
         (spreadsheet/select-columns {:A :email
                                      :B :name
                                      :C :roles
                                      :D :team
                                      :E :pwd
                                      :F :channel}) ;; Column F is nullable
         rest)))

(defn- raw-m->user-m
  "m is of the {HashMap} form that just reading the data from excel.
   However, we need to do certain transformation to get tx-map."
  [db m]
  (let [{t-str :team r-str :roles
         pwd-str :pwd c-str :channel
         e :email n :name} m
        t-ident (keyword t-str)
        r-ident (keyword r-str)
        pwd-hash (hs/derive pwd-str)
        c-ident (keyword c-str)]
    (if c-ident
      {:user/email e  :user/name n :user/team t-ident
       :user/roles r-ident :user/pwd pwd-hash :user/channel c-ident}
      {:user/email e :user/name n :user/team t-ident
       :user/roles r-ident :user/pwd pwd-hash})))

(defn- raw->tx [db raw-data]
  (->> raw-data
       (map #(raw-m->user-m db %))
       vec))

(defn sync-data
  "From Excel file, get the current users
   Write into database"
  [url filename]
  (log/info "sync-data triggered!")
  (let [db (d/db conn)
        e-user-raw (get-users-from-excel url filename)
        users (check-users e-user-raw)
        tx-data (raw->tx db users)]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
