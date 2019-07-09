(ns clj-crm.domain.query
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [clj-crm.db.revenue :as drevenue]
            [clj-crm.db.report :as dreport]
            [clj-crm.db.allocation :as dallo]
            [clj-crm.db.user :as duser]
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

(defmethod dispatch-q :all-revenues
  [user-q]
  (log/info "at all-revenues, user-q as" user-q)
  (let [tx (:tx user-q)
        db (if (and (not= 0 tx) (some? tx))
             (d/as-of (d/db conn) tx)
             (d/db conn))
        eids (duser/sales-eids db)
        team-user-m (group-by #(duser/u-eid->teamName db %) eids)
        team-data (map #(drevenue/t-u-entry->revenue-report db %) team-user-m)
        sales-data (map #(drevenue/u-eid->revenue-report db %) eids)
        other-report (drevenue/u-eids->other-revenue-report db eids)
        total-report (drevenue/total-revenue-report db)
        data (concat team-data sales-data [other-report total-report])
        sorted-data (sort-by (juxt :teamName :salesName) data)]
    (map drevenue/place-holder->total sorted-data)))

(defmethod dispatch-q :my-full-join-reports
  [user-q]
  (log/info "at my-full-join-reports, user-q as" user-q)
  (let [tx (:tx user-q)
        db (if (some? tx)
             (d/as-of (d/db conn) tx)
             (d/db conn))
        email (:user user-q)
        user-lookup-ref [:user/email email]
        u-eids (duser/u-eid->same-team-u-eids db user-lookup-ref)
        report (dreport/u-eids->full-join-reports db u-eids)]
    report))

(defmethod dispatch-q :my-revenues
  [user-q]
  (log/info "at my-revenues, user-q as" user-q)
  (let [tx (:tx user-q)
        db (if (some? tx)
             (d/as-of (d/db conn) tx)
             (d/db conn))
        email (:user user-q)
        user-lookup-ref [:user/email email]
        teamName (duser/u-eid->teamName db user-lookup-ref)
        eids (duser/t-eid->sales-eids db (keyword "user.team" teamName)) ;; eids belongs to the same team
        sales-data (map #(drevenue/u-eid->revenue-report db %) eids)
        customer-data (mapcat #(drevenue/u-eid->customer-revenue-report-v db %) eids)
        team-datum (drevenue/t-u-entry->revenue-report db [teamName eids])
        data (concat [team-datum] sales-data customer-data)
        sorted-data (sort-by (juxt :teamName :salesName :customerName) data)]
    (map drevenue/place-holder->total sorted-data)))

(defmethod dispatch-q :all-streams
  [user-q]
  (log/info "at all-streams, user-q as" user-q)
  (let [db (d/db conn)
        s-eids (drevenue/rev-stream-eids db)
        data (map #(drevenue/s-eid->rev-stream db %) s-eids)]
    data))

(defmethod dispatch-q :all-orders
  [user-q]
  (log/info "at all-orders, user-q as" user-q)
  (let [db (d/db conn)
        o-eids (drevenue/order-eids db)
        data (map #(drevenue/o-eid->order db %) o-eids)]
    data))

(defmethod dispatch-q :tag-tx-history
  [user-q]
  (log/info "at tag-tx-history, user-q as" user-q)
  (let [db (d/db conn)
        tag-txes (dcore/tag-tx-list db)
        data (conj tag-txes ["now" (d/t->tx (d/basis-t db))])]
    data))

(defmethod dispatch-q :allocation
  [user-q]
  (log/info "at allocation, user-q as" user-q)
  (let [db (d/db conn)
        eids (dallo/allo-eids db)
        allocations (map #(dallo/allo-eid->allocation db %) eids)]
    allocations))

(defmethod dispatch-q :rev-allo
  [user-q]
  (log/info "at rev-allo, user-q as" user-q)
  (let [db (d/db conn)
        eids (dallo/rev-allo-eids db)
        allocations (map #(dallo/rev-allo-eid->allocation db %) eids)]
    allocations))

(defmethod dispatch-q :target
  [user-q]
  (log/info "at target, user-q as" user-q)
  (let [db (d/db conn)
        eids (drevenue/target-eids db)
        data (map #(drevenue/target-eid->target db %) eids)]
    data))

(s/defschema QuerySchema {(s/required-key :q) s/Keyword
                          (s/optional-key :tx) s/Int})

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
