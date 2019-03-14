(ns clj-crm.domain.query
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(defn query-command-switch
  "Input:
   user-q is the form: {:user \"userA1@example.com\"
                         :q   \"all-customers\"}
   Output:
   :all-customers"
  [user-q]
  (keyword (:q user-q)))

;; example usage: (dispatch-q {:user "userA1@example.com" :q "all-customers"}))
(defmulti dispatch-q query-command-switch)

(defn all-teams []
  [:ok (map dcore/marshal-entity (dcore/find-all-by (d/db conn) :team/name))])

(defmethod dispatch-q :all-requests
  [user-q]
  (log/info "at all-requests, user-q as" user-q)
  (let [query-result (dcore/get-active-requests (d/db conn))
        data (mapv dcore/marshal-request query-result)]
    data))

(defmethod dispatch-q :my-requests
  [user-q]
  (log/info "at my-requests, user-q as" user-q)
  (let [email (:user user-q)
        user-lookup-ref [:user/email email]
        query-result (dcore/get-customers-of-open-requests-by-user (d/db conn) user-lookup-ref)
        data (mapv dcore/marshal-request query-result)]
    data))

(defmethod dispatch-q :all-customers
  [user-q]
  (log/info "at all-customers, user-q as" user-q)
  (let [db (d/db conn)
        query-result (dcore/get-left-joined-customers db)
        data (mapv #(dcore/marshal-left-joined-customer db %) query-result)]
    data))

(defmethod dispatch-q :my-customers
  [user-q]
  (log/info "at my-customers, user-q as" user-q)
  (let [email (:user user-q)
        user-lookup-ref [:user/email email]
        query-result (dcore/get-allo-customers-by-user (d/db conn)  user-lookup-ref)
        data (mapv dcore/marshal-entity query-result)]
    data))

(s/defschema PageSchema {:page-size s/Int
                         :page-index s/Int})
(s/defschema QuerySchema {(s/required-key :q) s/Str
                          (s/optional-key :pagination) PageSchema})

(defn query
  " Input:
    q is in the form: [Query]
    user is in the form: {:user userA1@example.com, :exp ...}

    Output:
    Return the result as the form of [status result], status maybe :ok :error.
    Example of return value is [:ok \"Hello World\"]"
  [q user req]
  ;; log user query
  (log/info "query as" q)
  (log/info "user as" user)
  (let [uid (:user user)
        user-q (assoc q :user uid)]
    [:ok (dispatch-q user-q)]))
