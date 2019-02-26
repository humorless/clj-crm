(ns clj-crm.domain.command
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(defn switch
  "Input:
   user-c is the form: {:user \"ggyy8@gmail.com\"
                         :c   \"new-request\"}
   Output:
   :new-requests"
  [user-c]
  (keyword (:c user-c)))

(defmulti dispatch-c switch)

(defmethod dispatch-c :new-request
  [user-c]
  (log/info "at new-request, user-c as" user-c)
  (let [email (:user user-c)
        user-lookup-ref [:user/email email]
        req (:req user-c)
        tx-data (conj req [:user user-lookup-ref])]
    (log/info "at new-request, email as" email)
    (log/info "at new-request, ulr as" user-lookup-ref)
    (log/info "at new-request, req as" req)
    (log/info "at new-request, tx-data as" tx-data)
    (dcore/insert-open-request conn tx-data)))

(s/defschema ReqSchema {:add-list #{s/Int}
                        :remove-list #{s/Int}})
(s/defschema CommandSchema {(s/required-key :c) s/Str
                            (s/optional-key :req) ReqSchema})

(defn command
  " Input:
    c is in the form: [Command]
    user is in the form: {:user ggyy8@gmail.com, :exp ...}

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
