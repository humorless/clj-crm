(ns clj-crm.etl.user
  (:require
   [clojure.tools.logging :as log]
   [buddy.hashers :as hs]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::email string?)
(spec/def ::name string?)
(spec/def ::roles string?)
(spec/def ::team string?)
(spec/def ::pwd (spec/nilable string?))
(spec/def ::channel (spec/nilable string?))

(spec/def ::user
  (spec/*
   (spec/keys :req-un
              [::email ::name ::roles ::team]
              :opt-un
              [::pwd ::channel])))

(def ^:private columns-map
  {:A :email
   :B :name
   :C :roles
   :D :team
   :E :pwd
   :F :channel})

(defn- pwd-hasher [pwd-str]
  (if (nil? pwd-str)
    nil
    (hs/derive pwd-str)))

(defn- raw-m->user-m
  "m is of the {HashMap} form that just reading the data from excel.
   However, we need to do certain transformation to get tx-map."
  [m]
  (let [{t-str :team r-str :roles
         pwd-str :pwd c-str :channel
         e :email n :name} m
        t-ident (keyword t-str)
        r-ident (keyword r-str)
        nilable-pwd-hash (pwd-hasher pwd-str)
        nilable-c-ident (keyword c-str)]
    (utility/compact {:user/email e  :user/name n :user/team t-ident :user/roles r-ident
                      :user/pwd nilable-pwd-hash :user/channel nilable-c-ident})))

(defn- data->data-txes [data]
  (mapv raw-m->user-m data))

(def ^:private check-raw
  (utility/check-raw-fn ::user))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "dev_user.xlsx")))
