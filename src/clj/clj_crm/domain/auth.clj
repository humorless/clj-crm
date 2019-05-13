(ns clj-crm.domain.auth
  (:require [clj-crm.db.core :refer [conn find-one-by]]
            [clj-crm.middleware :refer [token]]
            [clojure.tools.logging :as log]
            [datomic.api :as d]
            [buddy.hashers :as hs]))

(defn modify-password
  "user is {:user \"userA1@example.com\", :exp 1557735934} "
  [user pass]
  (let [tx-user {:user/email (:user user)
                 :user/pwd (hs/derive pass)}]
    @(d/transact conn [tx-user])))

(defn register-user
  "if not exists email, store (email, hash(pass)) into db"
  [screenname email pass role team-id]
  (log/info screenname "wants to register by email/role/team-id" email role team-id)
  (let [db (d/db conn)]
    (when-not (find-one-by db :user/email email)
      (let [tx-user {:user/name screenname
                     :user/pwd (hs/derive pass)
                     :user/email email
                     :user/status :user.status/active
                     :user/roles role
                     :user/team team-id}]
        @(d/transact conn [tx-user])))))

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
        eid (:db/id u)
        jwe-token (token (:user/email u))
        username (:user/name u)
        r (:user/roles u)]
    {:token jwe-token :email email :username username :eid eid :role r}))
