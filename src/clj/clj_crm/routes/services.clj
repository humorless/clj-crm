(ns clj-crm.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [clj-crm.domain.enumeration :as enum]
            [clj-crm.domain.auth :refer [user-datum user-auth register-user modify-password]]
            [clj-crm.domain.query :as dq]
            [clj-crm.domain.command :as dc]
            [clj-crm.etl.core :as etl]
            [clj-crm.cron.core :as cron]
            [clojure.tools.logging :as log]
            [compojure.api.meta :refer [restructure-param]]
            [buddy.auth.accessrules :refer [restrict]]
            [buddy.auth :refer [authenticated?]]))

(defn access-error [_ _]
  (unauthorized {:error "unauthorized"}))

(defn wrap-restricted [handler rule]
  (restrict handler {:handler  rule
                     :on-error access-error}))

(defmethod restructure-param :auth-rules
  [_ rule acc]
  (update-in acc [:middleware] conj [wrap-restricted rule]))

(defmethod restructure-param :current-user
  [_ binding acc]
  (update-in acc [:letks] into [binding `(:identity ~'+compojure-api-request+)]))

(def service-routes
  (api
   {:swagger {:ui "/swagger-ui"
              :spec "/swagger.json"
              :data {:info {:version "1.0.0"
                            :title "CRM system API"
                            :description "Customer booking, pipeline, AR calculation Services"}}}}

   (GET "/authenticated" []
     :auth-rules authenticated?
     :header-params [authorization :- s/Str]
     :current-user user
     :description "Authorization header expects the following format 'Token {token}', for example: Token eyJhbGciOiJ..."
     (ok {:user user}))

   (GET "/api/teams" req
     :summary     "Return all the team names and entity ids"
     (let [[status result] (dq/all-teams)]
       (case status
         :ok (ok {:result result})
         :error (bad-request {:reason result}))))

   (GET "/api/users" req
     :summary     "Return all the user names and related data."
     (let [[status result] (dq/all-users)]
       (case status
         :ok (ok {:result result})
         :error (bad-request {:reason result}))))

   (GET "/api/products" req
     :summary     "Return all the product enumerations"
     (let [[status result] (dq/all-products)]
       (case status
         :ok (ok {:result result})
         :error (bad-request {:reason result}))))

   (POST "/api/login" req
     :body-params [email :- s/Str, password :- s/Str]
     :summary     "User uses email/password to login"
     (if (user-auth email password)
       (ok {:user (user-datum email)})
       (unauthorized {:error "wrong auth data"})))

   (POST "/api/teams" req
     :body-params [teamName :- s/Str]
     :summary     "input the $teamName to create correspond team enumeration as :user.team/$teamName"
     (if (enum/add-team teamName)
       (ok)
       (bad-request)))

   (POST "/api/products" req
     :body-params [productName :- s/Str]
     :summary     "input the $productName to create correspond product enumeration as :product.type/$productName. Note that the added productName must be of default revenue calculation rule. Delta revenue calculation is not supported for new added productName."
     (if (enum/add-product productName)
       (ok)
       (bad-request)))

   (GET "/api/user" req
     :auth-rules authenticated?
     :header-params [authorization :- s/Str]
     :current-user user
     :summary     "User use jwe token to get the user-datum"
     (ok {:user (user-datum (:user user))}))

   (PUT "/api/user" req
     :auth-rules authenticated?
     :header-params [authorization :- s/Str]
     :current-user user
     :body-params [username :- s/Str, password :- s/Str]
     :summary     "modify new password"
     (log/info "debug /api/user" req)
     (if (modify-password user password)
       (ok {:result :password-changed})
       (ok {:result :password-not-changed})))

   (POST "/api/sync" req
     :body-params [filename :- s/Str, cmd :- s/Str]
     :summary     "Sync data from excel file."
     :description "filename refers to the excel filename. cmd can be [customer|user|allocation|rev-allo|lamp|gui|agp|lap|target|pipeline]"
     (try (if-let [r (etl/sync-data cmd filename)]
            (ok {:result :insert-done})
            (ok {:result :already-sync}))
          (catch clojure.lang.ExceptionInfo e
            (bad-request {:reason (ex-data e)}))
          (catch java.util.concurrent.ExecutionException e
            (bad-request {:reason (.getCause e)}))))

   (GET "/api/cron" req
     :summary     "Get the now version query cache re-calculation cronjob status"
     (ok (cron/jobs)))

   (POST "/api/cron" req
     :body-params [quarters :- [s/Str], year :- s/Int]
     :summary     "Install the now version query cache re-calculation cronjob into this backend process"
     :description "The example quarters can be `[\"q3\" , \"q4\"]`, which re-calculate only Q1 and Q2. The example year can be `2019`. The year and quarters corresponds to `Time Period` of UI. "
     (try
       (cron/install-jobs quarters year)
       (ok {:result :install-done})
       (catch Exception e
         (bad-request {:reason (.getCause e)}))))

   (POST "/api/delete" req
     :body-params [table-name :- s/Keyword]
     :summary "API used to delete content of table."
     :description "possible table names are: rev-allo, non-direct-allo, direct-allo, target, pipeline"
     (try (if-let [r (dc/dispatch-del table-name)]
            (ok {:result :table-delete-success}))
          (catch clojure.lang.ExceptionInfo e
            (bad-request {:reason (ex-data e)}))
          (catch java.util.concurrent.ExecutionException e
            (bad-request {:reason (.getCause e)}))))

   (POST "/api/delete-target" req
     :body-params [y-q :- s/Str]
     :summary "API used to delete content of target table by year-quarter."
     :description "possible quarter values are: `2019-q3`"
     (try (if-let [r (dc/delete-target y-q)]
            (ok {:result :target-delete-success}))
          (catch clojure.lang.ExceptionInfo e
            (bad-request {:reason (ex-data e)}))
          (catch java.util.concurrent.ExecutionException e
            (bad-request {:reason (.getCause e)}))))

   (POST "/api/delete-order" req
     :body-params [etl-source :- s/Str]
     :summary "API used to delete the order table."
     :description "possible etl-source are: `lamp`, `gui`"
     (try (if-let [r (dc/delete-order etl-source)]
            (ok {:result :order-delete-success}))
          (catch clojure.lang.ExceptionInfo e
            (bad-request {:reason (ex-data e)}))
          (catch java.util.concurrent.ExecutionException e
            (bad-request {:reason (.getCause e)}))))

   (POST "/api/delete-order-gui" req
     :body-params [accounting-time :- s/Str]
     :summary "API used to delete the order table with etl-source gui."
     :description "possible accouting-time format is `2019-05`"
     (try (if-let [r (dc/delete-order-gui accounting-time)]
            (ok {:result :order-delete-success}))
          (catch clojure.lang.ExceptionInfo e
            (bad-request {:reason (ex-data e)}))
          (catch java.util.concurrent.ExecutionException e
            (bad-request {:reason (.getCause e)}))))

   (POST "/api/delete-rev-stream" req
     :body-params [etl-source :- s/Str, accounting-time :- s/Str]
     :summary "API used to delete the rev-stream table."
     :description "Possible etl-source are: `lap`, `agp`. Possible accounting-time format is `2019-05`"
     (try (if-let [r (dc/delete-rev-stream etl-source accounting-time)]
            (ok {:result :rev-stream-delete-success}))
          (catch clojure.lang.ExceptionInfo e
            (bad-request {:reason (ex-data e)}))
          (catch java.util.concurrent.ExecutionException e
            (bad-request {:reason (.getCause e)}))))

   (POST "/api/transaction" req
     :body-params [date-str :- s/Str, queryable :- s/Bool]
     :summary     "Record the transaction tag, which is represented by date-str. If queryable set as false, then this tag will not show up at the query API - tag-tx-history"
     :description "date-str denotes the date string showing on the UI. Example: 2019-05-23-v1"
     (try (if-let [r (dc/transact-tag-tx date-str queryable)]
            (ok {:result :tag-tx-written}))
          (catch clojure.lang.ExceptionInfo e
            (bad-request {:reason (ex-data e)}))
          (catch java.util.concurrent.ExecutionException e
            (bad-request {:reason (.getCause e)}))))

   (context "/api" []
     :auth-rules authenticated?
     :header-params [authorization :- s/Str]
     :current-user user
     :description "Authorization header expects the following format 'Token {token}', for example: Token eyJhbGciOiJ..."
     :tags ["Restricted API"]
     (POST "/query" req
       :body [q dq/QuerySchema {:description "Query"}]
       :summary "frontend use query api to do all kind of read operations"
       (let [[status result] (dq/query q user req)]
         (case status
           :ok (ok {:result result})
           :error (bad-request {:reason result}))))

     (POST "/command" req
       :body [c dc/CommandSchema {:description "Command"}]
       :summary "frontend use command api to do all kind of write operations"
       (let [[status result] (dc/command c user req)]
         (case status
           :ok (ok {:result result})
           :error (bad-request {:reason result})))))))
