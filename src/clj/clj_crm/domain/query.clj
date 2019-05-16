(ns clj-crm.domain.query
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(defn query-command-switch
  "Input:
   user-q is the form: {:user \"userA1@example.com\"
                         :q   :all-customers}
   Output:
   :all-customers"
  [user-q]
  (:q user-q))

;; example usage: (dispatch-q {:user "userA1@example.com" :q "all-customers"}))
(defmulti dispatch-q query-command-switch)

(defn all-teams []
  (let [db (d/db conn)
        eids (dcore/team-enum-eids db)
        query-result (map #(dcore/eid->enum db %) eids)
        data (mapv #(dcore/recur-marshal db %) query-result)]
    [:ok data]))

(defn all-users []
  (let [db (d/db conn)
        eids (dcore/user-eids db)
        query-result (map #(dcore/u-eid->user db %) eids)
        data (mapv #(dcore/recur-marshal db %) query-result)]
    [:ok data]))

(defn all-products []
  (let [db (d/db conn)
        eids (dcore/product-enum-eids db)
        query-result (map #(dcore/eid->enum db %) eids)
        data (mapv #(dcore/recur-marshal db %) query-result)]
    [:ok data]))

(defmethod dispatch-q :all-requests
  [user-q]
  (log/info "at all-requests, user-q as" user-q)
  (let [db (d/db conn)
        eids (dcore/active-request-eids db)
        req-maps (map #(dcore/r-eid->req db %) eids)
        hdb (d/history db)
        txInsts (map #(dcore/r-eid->request-open-time hdb %) eids)
        query-result (map #(assoc %1 :req/time %2) req-maps txInsts)
        data (mapv #(dcore/recur-marshal db %) query-result)]
    data))

(defmethod dispatch-q :my-requests
  [user-q]
  (log/info "at my-requests, user-q as" user-q)
  (let [email (:user user-q)
        user-lookup-ref [:user/email email]
        db (d/db conn)
        eids (dcore/user-active-request-eids db user-lookup-ref)
        req-maps (map #(dcore/r-eid->req db %) eids)
        hdb (d/history db)
        txInsts (map #(dcore/r-eid->request-open-time hdb %) eids)
        query-result (map #(assoc %1 :req/time %2) req-maps txInsts)
        data (mapv #(dcore/recur-marshal db %) query-result)]
    data))

(defmethod dispatch-q :all-customers
  [user-q]
  (log/info "at all-customers, user-q as" user-q)
  (let [db (d/db conn)
        eids (dcore/customer-eids db)
        query-result (map #(dcore/c-eid->cust+sales+prod db %) eids)
        data (mapv #(dcore/recur-marshal db %) query-result)]
    data))

(defmethod dispatch-q :my-customers
  [user-q]
  (comment
    "To test this method:

    (dispatch-q {:q :my-customer-report
                 :user \"userA1@example.com\"})")
  (log/info "at my-customer-report, user-q as" user-q)
  (let [db (d/db conn)
        email (:user user-q)
        user-lookup-ref [:user/email email]
        eids (dcore/user-customer-eids db user-lookup-ref)
        query-result (map #(dcore/c-eid->cust+sales+prod db %) eids)
        data (mapv #(dcore/recur-marshal db %) query-result)]
    data))

(defmethod dispatch-q :my-revenues
  [user-q]
  (log/info "at my-revenues, user-q as" user-q)
  (let [db (d/db conn)
        email (:user user-q)
        user-lookup-ref [:user/email email]
        orders (dcore/u-eid->orders db user-lookup-ref)
        revenues (dcore/orders->revenue-report db orders)]
    revenues))

(defmethod dispatch-q :all-revenues
  [user-q]
  (log/info "at all-revenues, user-q as" user-q)
  (let [db (d/db conn)
        eids (dcore/user-eids db)
        u-names (map #(second (second (dcore/u-eid->user db %))) eids)
        orderss (map #(dcore/u-eid->orders db %) eids)
        revenuess (map #(dcore/orders->revenue-report db %) orderss)
        data (zipmap u-names revenuess)]
    data))

(s/defschema QuerySchema {(s/required-key :q) s/Keyword})

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
