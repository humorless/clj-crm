(ns clj-crm.domain.query
  (:require [clj-crm.db.core :refer [conn find-one-by upsert-user!]]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

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
  [:ok "Hello world"])
