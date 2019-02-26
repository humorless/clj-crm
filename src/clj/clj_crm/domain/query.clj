(ns clj-crm.domain.query
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(defn query-command-switch
  "Input:
   user-q is the form: {:user \"ggyy8@gmail.com\"
                         :q   \"all-customers\"}
   Output:
   :all-customers"
  [user-q]
  (keyword (:q user-q)))

;; example usage: (dispatch-q {:user "ggyy8@gmail.com" :q "all-customers"}))
(defmulti dispatch-q query-command-switch)

(defn marshal-customer
  "for input's field, remove the namespace of keyword, replace :db/id as :eid
   Also, for the enumeration like :customer/business-type and :customer/inventory-type, do the necessary marshalling
   Input:

   {CUSTOMER-MAP}"
  [customer]
  (let [db (d/db conn)
        erase-namespace #(keyword (name %))
        eid (:db/id customer)
        c (dissoc customer :db/id)]
    (reduce (fn [acc [k v]]
              (if-let [enum (:db/id v)]
                (into acc {(erase-namespace k) (d/ident db enum)}) ;; handle the :business-type/:inventory-type
                (into acc {(erase-namespace k) v})))
            {:eid eid}
            c)))

(defn marshal-request
  "for input's field, remove the namespace of keyword
   Input:

   {:req/add-customer-list    [{CUSTOMER-MAP} ...]
    :req/remove-customer-list [{CUSTOMER-MAP} ...]}  "
  [req]
  (let [erase-namespace #(keyword (name %))]
    (reduce (fn [acc [k v]]
              (into acc {(erase-namespace k) (mapv marshal-customer v)}))
            {}
            req)))

(defmethod dispatch-q :my-requests
  [user-q]
  (log/info "at my-requests, user-q as" user-q)
  (let [email (:user user-q)
        user-lookup-ref [:user/email email]
        query-result (dcore/get-customers-of-open-requests-by-user (d/db conn) user-lookup-ref)
        data (mapv marshal-request query-result)]
    data))

(defmethod dispatch-q :all-customers
  [user-q]
  (log/info "at all-customers, user-q as" user-q)
  (let [query-result (dcore/find-all-by (d/db conn) :customer/id)
        data (mapv dcore/marshal-entity query-result)]
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
    user is in the form: {:user ggyy8@gmail.com, :exp ...}

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
