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
        eids (dcore/customer-eids db)
        query-result (map #(dcore/c-eid->cust+sales db %) eids)
        data (mapv #(dcore/marshal-left-joined-customer db %) query-result)]
    data))

(defn pull-inventory
  "Input is a ordinary customer map

  {:id ...
   :name ...
   :name-en ...
   :inventory-type
   :eid ...}"
  [c-map]
  (let [invent-part (assoc {}
                           :inventory-type (:inventory-type c-map)
                           :eid (:eid c-map))
        s (:sales c-map)]
    (if (nil? s)
      invent-part
      (assoc invent-part :sales s))))

(defn un-join-customer-inventory
  "Input is a vector of a pair of [tax-id customerVector]

   customerVector is in the form of [customerMap, customerMap]"
  [[tax-id customers]]
  (let [c (first customers)
        c-inherent (dissoc c :eid :sales :inventory-type :rp-id)
        i-types (mapv pull-inventory customers)]
    (assoc c-inherent :inventory i-types)))

(defmethod dispatch-q :customer-report
  [user-q]
  (log/info "at all-customers, user-q as" user-q)
  (let [db (d/db conn)
        eids (dcore/customer-eids db)
        query-result (map #(dcore/c-eid->cust+sales db %) eids)
        customers (mapv #(dcore/marshal-left-joined-customer db %) query-result)
        tax-customer-pairs (group-by :tax-id customers)
        data (map un-join-customer-inventory tax-customer-pairs)]
    data))

(defmethod dispatch-q :my-customer-report
  [user-q]
  (comment
    "To test this method:

    (dispatch-q {:q \"my-customer-report\"
                 :user \"userA1@example.com\"})")
  (log/info "at my-customer-report, user-q as" user-q)
  (let [db (d/db conn)
        email (:user user-q)
        user-lookup-ref [:user/email email]
        eids (dcore/customer-eids-by-user db user-lookup-ref)
        query-result (map #(dcore/c-eid->cust+sales db %) eids)
        customers (mapv #(dcore/marshal-left-joined-customer db %) query-result)
        tax-customer-pairs (group-by :tax-id customers)
        data (map un-join-customer-inventory tax-customer-pairs)]
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
