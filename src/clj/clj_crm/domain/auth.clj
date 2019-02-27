(ns clj-crm.domain.auth
  (:require [clj-crm.db.core :refer [conn find-one-by upsert-user!]]
            [clj-crm.middleware :refer [token]]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            [buddy.hashers :as hs]))

(defn user-register!
  "if not exists email, store (email, hash(pass)) into db"
  [screenname email pass]
  (log/info screenname "wants to register by" email)
  (let [db (d/db conn)]
    (when-not (find-one-by db :user/email email)
      (upsert-user! conn {:user-name screenname
                          :pwd (hs/derive pass)
                          :email email
                          :status :user.status/active
                          :roles  :user.roles/sales}))))

(defn user-auth
  "return 'true' when email/pass pair correct, otherwise return nil"
  [email pass]
  (let [db (d/db conn)]
    (when-let [user (find-one-by db :user/email email)] ;;get user record from db
      (when (hs/check pass (:user/pwd user)) ;;compare user pass
        true))))

(defn user-datum
  "return the 'user map'"
  [email]
  (let [u (find-one-by (d/db conn) :user/email email)
        jwe-token (token (:user/email u))
        username (:user/name u)]
  {:token jwe-token :email email :username username}))
