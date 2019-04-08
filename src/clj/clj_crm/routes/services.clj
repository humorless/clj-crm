(ns clj-crm.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [clj-crm.domain.auth :refer [user-datum user-auth register-user]]
            [clj-crm.domain.query :as dq]
            [clj-crm.domain.command :as dc]
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

   (POST "/api/register" req
     :body-params [email :- s/Str, password :- s/Str, screenname :- s/Str, role :- s/Str, team-id :- Long]
     :summary     "User uses email/password to register, UI default role is sales"
     :description "possilbe value of role could be: sales, lead, manager"
     (if (register-user screenname email password (keyword "user.roles" role) team-id)
       (ok {:user (user-datum email)})
       (bad-request)))

   (POST "/api/login" req
     :body-params [email :- s/Str, password :- s/Str]
     :summary     "User uses email/password to login"
     (if (user-auth email password)
       (ok {:user (user-datum email)})
       (unauthorized {:error "wrong auth data"})))

   (GET "/api/user" req
     :auth-rules authenticated?
     :header-params [authorization :- s/Str]
     :current-user user
     :summary     "User use jwe token to get the user-datum"
     (ok {:user (user-datum (:user user))}))

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
           :error (bad-request {:reason result}))))

     (GET "/plus" []
       :return       Long
       :query-params [x :- Long, {y :- Long 1}]
       :summary      "x+y with query-parameters. y defaults to 1."
       (ok (+ x y))))))
