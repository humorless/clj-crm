(ns clj-crm.domain.command
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(defn switch
  "Input:
   user-c is the form: {:user \"userA1@example.com\"
                         :c   :new-request}
   Output:
   :new-requests"
  [user-c]
  (:c user-c))

(defmulti dispatch-c switch)

(defmethod dispatch-c :modify-request
  [user-c]
  (log/info "at modify-request, user-c as" user-c)
  (let [id (get-in user-c [:req-op :id])
        stamp (get-in user-c [:req-op :stamp])
        add-list (get-in user-c [:req-op :add-list])
        remove-list (get-in user-c [:req-op :remove-list])
        tx-data (dcore/tx-modify-request id stamp add-list remove-list)]
    (log/info "at modify-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-c :reject-request
  [user-c]
  (log/info "at reject-request, user-c as" user-c)
  (let [id (get-in user-c [:req-op :id])
        stamp (get-in user-c [:req-op :stamp])
        tx-data (dcore/tx-reject-request id stamp)]
    (log/info "at reject-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-c :approve-request
  [user-c]
  (log/info "at approve-request, user-c as" user-c)
  (let [id (get-in user-c [:req-op :id])
        stamp (get-in user-c [:req-op :stamp])
        tx-data (dcore/tx-approve-request id stamp)]
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
                  :req/add-customer-items    add-list
                  :req/remove-customer-items remove-list
                  :req/status               :req.status/open
                  :req/stamp                0}]]
    (log/info "at new-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(s/defschema customerItemSchema {:customerItem/customer s/Int
                                 :customerItem/product  s/Keyword})

(s/defschema newReqSchema {:add-list #{customerItemSchema}
                           :remove-list #{customerItemSchema}})

;; opReqSchema is for approve/reject/modify
;; :stamp is to make sure that even two admin operate on the same request
;; the request's approval/rejection/modification will be logically strict.
(s/defschema opReqSchema {:id s/Int
                          :stamp s/Int
                          (s/optional-key :add-list) #{s/Int}
                          (s/optional-key :remove-list) #{s/Int}})

(s/defschema CommandSchema {(s/required-key :c) s/Keyword
                            (s/optional-key :req) newReqSchema
                            (s/optional-key :req-op) opReqSchema})

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
