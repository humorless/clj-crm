(ns clj-crm.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [clj-crm.domain.auth :refer [user-auth user-register!]]
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
                            :title "Sample API"
                            :description "Sample Services"}}}}

   (GET "/authenticated" []
     :auth-rules authenticated?
     :current-user user
     (ok {:user user}))

   (POST "/api/register" req
     :body-params [email :- s/Str, password :- s/Str, screenname :- s/Str]
     :summary     "User uses email/password to register, default role is sales"
     (if (user-register! screenname email password)
       (ok)
       (bad-request)))

   (POST "/api/login" req
     :body-params [email :- s/Str, password :- s/Str]
     :summary     "User uses email/password to login"
     (if-let [token-datum (user-auth email password)]
       (ok {:user {:token token-datum}})
       (unauthorized {:error "wrong auth data"})))

   (context "/api" []
     :auth-rules authenticated?
     :tags ["thingie"]
     (GET "/plus" []
       :return       Long
       :query-params [x :- Long, {y :- Long 1}]
       :summary      "x+y with query-parameters. y defaults to 1."
       (ok (+ x y))))))
