(ns clj-crm.db.core
  (:require [datomic.api :as d]
            [io.rkn.conformity :as c]
            [mount.core :refer [defstate]]
            [clj-crm.config :refer [env]]))

;; create datomic database is idempotent operation
(defstate conn
  :start (do (-> env :database-url d/create-database) (-> env :database-url d/connect))
  :stop (-> conn .release))

;; fresh-conn is for testing purpose db connection
(comment
  (def fresh-conn
    (let [uri "datomic:mem://test"]
      (d/delete-database uri)
      (d/create-database uri)
      (d/connect uri))))

(def norms-map (c/read-resource "schema.edn"))

;; verification
;; (d/touch (ffirst (find-all-by (d/db conn) :conformity/conformed-norms))))
(defn setup-app-schema [conn]
  (c/ensure-conforms conn norms-map (keys norms-map)))

(defn show-app-schema [conn]
  (let [system-ns #{"db" "db.type" "db.install" "db.part"
                    "db.lang" "fressian" "db.unique" "db.excise"
                    "db.cardinality" "db.fn" "db.sys" "db.bootstrap"
                    "db.alter"}]
    (d/q '[:find ?ident
           :in $ ?system-ns
           :where
           [?e :db/ident ?ident]
           [(namespace ?ident) ?ns]
           [((comp not contains?) ?system-ns ?ns)]]
         (d/db conn) system-ns)))

;; (-> conn show-transaction count) to show the number of transaction
(defn show-transaction
  "show all the transaction data"
  [conn]
  (seq (d/tx-range (d/log conn) nil nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn only
  "Return the only item from a query result"
  [query-result]
  (assert (= 1 (count query-result)))
  (assert (= 1 (count (first query-result))))
  (ffirst query-result))

(defn qe
  "Returns the single entity returned by a query."
  [query db & args]
  (let [res (apply d/q query db args)]
    (d/entity db (only res))))

(defn qes
  "Returns the entities returned by a query, assuming that
   all :find results are entity ids."
  [query db & args]
  (->> (apply d/q query db args)
       (mapv (fn [items]
               (mapv (partial d/entity db) items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn upsert-user!
  "Given the user entity has :db.unique/identity attribute, Datomic will upsert"
  [conn {:keys [user-name pwd email status roles]}]
  @(d/transact conn [{:user/name      user-name
                      :user/pwd       pwd
                      :user/email     email
                      :user/status    status
                      :user/roles     roles}]))

;; Given that A.attr is primary key
;; Similar usage pattern to SQL expression: `SELECT * FROM A WHERE A.attr = val`
(defn find-by
  "Returns the unique entity identified by attr and val."
  [db attr val]
  (qe '[:find ?e
        :in $ ?attr ?val
        :where [?e ?attr ?val]]
      db attr val))

;; Similar usage pattern to SQL expression: `SELECT * FROM A`
(defn find-all-by
  "Returns all entities possessing attr."
  [db attr]
  (qes '[:find ?e
         :in $ ?attr
         :where [?e ?attr]]
       db attr))

;; example:
;;   (d/touch (find-one-by (d/db conn) :user/email  "ggyy8@gmail.co"))
;;   => NPE
;;   (:user/pwd (find-one-by (d/db conn) :user/email "ggyy8@gmail.com"))
;;   => $pwd
(defn find-one-by
  "Given db value and an (attr/val), return the user as EntityMap (datomic.query.EntityMap)"
  [db attr val]
  (d/entity db
            (d/q '[:find ?e .
                   :in $ ?attr ?val
                   :where [?e ?attr ?val]]
                 db attr val)))

(defn get-open-requests-by-user
  "get the open request currently submitted by user -- sales' own request
   e.g.:
   (map d/touch (get-open-requests-by-user (d/db conn) [:user/email \"ggyy8@gmail.com\"]))"
  [db user]
  (->> (d/q '[:find ?e .
              :in $ ?u
              :where
              [?e :req/sales ?u]
              [?e :req/status :req.status/open]]
            db user)
       (map #(d/entity db %))))

(defn get-allo-customers-by-user
  "
  get the customers list currently allocated by user -- sales' own customer list
  example usage: (map d/touch(get-allo-customers-by-user (d/db conn) [:user/email \"ggyy8@gmail.com\"]))
  "
  [db user]
  (->>  (d/q '[:find [?c ...]
               :in $ ?u
               :where
               [?e :allo/sales ?u]
               [?e :allo/customer ?c]]
             db user)
        (map #(d/entity db %))))

(defn get-requests-by-status
  "
  get the ([request-entity, #inst] ... ) or () when there is no requests
  example usage: (get-requests-by-status (d/db conn) :req.status/open)
  Note: This query will bring request's created time along with request entity.
  "
  [db status]
  (->>  (d/q '[:find ?e ?inst
               :in $ ?v
               :where
               [?e :req/status ?v ?tx true]
               [?tx :db/txInstant ?inst]]
             db status)
        (map (fn [[req-eid inst]] [(d/entity db req-eid) inst]))))

(defn marshal-entity
  "Input: data of class 'datomic.query.EntityMap'
   Ouput: data map suitable for transfer to network

   Note: when transfer to network, keyword's namespace will be removed.
   e.g.
   ':customer/id' -> ':id'

   However, the reverse ref is special case:
   ':db/id'       -> ':eid'"
  [c]
  (let [erase-namespace #(keyword (name %)) ;; remove namespace from keyword
        entity-map (d/touch c)]
    (reduce (fn [acc [k v]]
              (into acc {(erase-namespace k) v}))
            {:eid (:db/id entity-map)} ;; prepare :eid in inital map
            (seq c))))

(comment
  ;; example of upsert-user!
  (upsert-user! conn {:user-name "Laurence Chen"
                      :pwd "bcrypt+sha512$7b58b1516abd049081f655555b154270$12$1f97671825888b5dd330ba8e489774b2b1b076c55e991ba6"
                      :email "ggyy8@gmail.com"
                      :status :user.status/active
                      :roles  :user.roles/sales}))
