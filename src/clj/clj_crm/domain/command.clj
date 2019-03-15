(ns clj-crm.domain.command
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(defn switch
  "Input:
   user-c is the form: {:user \"userA1@example.com\"
                         :c   \"new-request\"}
   Output:
   :new-requests"
  [user-c]
  (keyword (:c user-c)))

(defmulti dispatch-c switch)

(defmethod dispatch-c :reject-request
  [user-c]
  (log/info "at reject-request, user-c as" user-c)
  (let [req-id (get-in user-c [:req-app :req-id])
        req-status (keyword (get-in user-c [:req-app :req-status]))
        tx-data (dcore/tx-reject-request req-id req-status)]
    (log/info "at reject-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-c :approve-request
  [user-c]
  (log/info "at approve-request, user-c as" user-c)
  (let [req-id (get-in user-c [:req-app :req-id])
        req-status (keyword (get-in user-c [:req-app :req-status]))
        tx-data (dcore/tx-approve-request req-id req-status)]
    (log/info "at approve-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-c :new-request
  [user-c]
  (log/info "at new-request, user-c as" user-c)
  (let [email (:user user-c)
        user-lookup-ref [:user/email email]
        add-list (get-in user-c [:req :add-list])
        remove-list (get-in user-c [:req :remove-list])
        tx-data [{:req/sales                user-lookup-ref
                  :req/add-customer-list    add-list
                  :req/remove-customer-list remove-list
                  :req/status               :req.status/open}]]
    (log/info "at new-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(s/defschema newReqSchema {:add-list #{s/Int}
                           :remove-list #{s/Int}})

;;appReqSchema is for approve and reject
;;  `req-status` string need to be in this schema to do concurrency control
;;  Reasonable `req-status` string may be "req.status/open", "req.status/modified"
(s/defschema appReqSchema {:req-id s/Int
                           :req-status s/Str})

(s/defschema CommandSchema {(s/required-key :c) s/Str
                            (s/optional-key :req) newReqSchema
                            (s/optional-key :req-app) appReqSchema})

(defn command
  " Input:
    c is in the form: [Command]
    user is in the form: {:user userA1@example.com, :exp ...}

    Output:
    Return the result as the form of [status result], status maybe :ok :error.
    Example of return value is [:ok \"Hello World\"]"
  [c user req]
  ;; log user query
  (log/info "command as" c)
  (log/info "user as" user)
  (let [uid (:user user)
        user-c (assoc c :user uid)]
    [:ok (dispatch-c user-c)]))
