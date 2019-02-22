(ns clj-crm.domain.query
  (:require [clj-crm.db.core :refer [conn find-one-by get-allo-customers-by-user]]
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

(defmethod dispatch-q :all-customers
  [user-q]
  [:customer1
   :customer2
   :customer3])

(defn marshal-customer
  "Input: customer as type (EntityMap)
   Ouput: data suitable for transfer to network"
  [c]
  {:id (:customer/id c)
   :name (:customer/name c)
   :name-en (:customer/name-en c)
   :tax-id (:customer/tax-id c)
   :inventory-type (:customer/inventory-type c)})

(defmethod dispatch-q :my-customers
  [user-q]
  (log/info "user-q as" user-q)
  (let [email (:user user-q)
        user-lookup-ref [:user/email email]
        data (mapv marshal-customer (get-allo-customers-by-user  (d/db conn)  user-lookup-ref))]
    data))

(s/defschema Query {:q s/Str})

(defn query
  " Input:
    queries is the form: [Query]
    user is the form: {:user ggyy8@gmail.com, :exp ...}

    Output:
    Return the result as the form of [status result], status maybe :ok :error.
    Example of return value is [:ok \"Hello World\"]"
  [queries user req]
  (log/info "queries as" queries)
  (log/info "user as" user)
  ;; check user queries
  (let [uid (:user user)
        user-queries (map #(assoc % :user uid) queries)]
    [:ok (mapv dispatch-q user-queries)]))
