(ns clj-crm.domain
  (:require [clj-crm.db.core :refer [conn find-by-allow-nil]]
            [datomic.api :as d]
            [buddy.hashers :as hs]))

(defn user-auth
  "return 'user entity Map' when email/pass pair correct, otherwise return nil"
  [email pass]
  (let [db (d/db conn)]
    (when-let [user (find-by-allow-nil db :user/email email)]
      (when (hs/check pass (:user/pwd user)) 
        user))))
