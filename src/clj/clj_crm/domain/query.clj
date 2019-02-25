(ns clj-crm.domain.query
  (:require [clj-crm.db.core :refer [conn find-one-by get-allo-customers-by-user marshal-entity]]
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

(defmethod dispatch-q :my-customers
  [user-q]
  (log/info "user-q as" user-q)
  (let [email (:user user-q)
        user-lookup-ref [:user/email email]
        data (mapv marshal-entity (get-allo-customers-by-user  (d/db conn)  user-lookup-ref))]
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
