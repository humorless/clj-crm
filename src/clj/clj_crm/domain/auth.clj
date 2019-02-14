(ns clj-crm.domain.auth
  (:require [clj-crm.db.core :refer [conn find-one-by]]
            [clj-crm.middleware :refer [token]]
            [datomic.api :as d]
            [buddy.hashers :as hs]))

(defn user-auth
  "return 'jwe token' when email/pass pair correct, otherwise return nil"
  [email pass]
  (let [db (d/db conn)]
    (when-let [user (find-one-by db :user/email email)] ;;get user record from db
      (when (hs/check pass (:user/pwd user)) ;;compare user pass
        (token (:user/email user)))))) ;; encrypt :user/email into jwe token
