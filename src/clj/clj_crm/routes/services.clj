(ns clj-crm.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [clj-crm.domain.auth :refer [user-auth]]
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

(def loginResp
  {(s/optional-key :token) s/Str
   (s/optional-key :error) s/Str})

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
   (context "/api" []
     :tags ["thingie"]
     (POST "/login" req
       :return      loginResp
       :body-params [email :- s/Str, pass :- s/Str]
       :summary     "User uses email/pass to login"
       (if-let [token-datum (user-auth email pass)]
         (ok {:token token-datum})
         (unauthorized {:error "wrong auth data"})))

     (GET "/plus" []
       :return       Long
       :query-params [x :- Long, {y :- Long 1}]
       :summary      "x+y with query-parameters. y defaults to 1."
       (ok (+ x y)))

     (POST "/minus" []
       :return      Long
       :body-params [x :- Long, y :- Long]
       :summary     "x-y with body-parameters."
       (ok (- x y)))

     (GET "/times/:x/:y" []
       :return      Long
       :path-params [x :- Long, y :- Long]
       :summary     "x*y with path-parameters"
       (ok (* x y)))

     (POST "/divide" []
       :return      Double
       :form-params [x :- Long, y :- Long]
       :summary     "x/y with form-parameters"
       (ok (/ x y)))

     (GET "/power" []
       :return      Long
       :header-params [x :- Long, y :- Long]
       :summary     "x^y with header-parameters"
       (ok (long (Math/pow x y)))))))
