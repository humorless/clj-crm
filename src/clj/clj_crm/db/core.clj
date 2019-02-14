(ns clj-crm.db.core
  (:require [datomic.api :as d]
            [io.rkn.conformity :as c]
            [mount.core :refer [defstate]]
            [clj-crm.config :refer [env]]))

;; create datomic database is idempotent operation
(defstate conn
  :start (do (-> env :database-url d/create-database) (-> env :database-url d/connect))
  :stop (-> conn .release))

(def norms-map (c/read-resource "schema.edn"))

;; verification
;; (d/touch (ffirst (find-all-by (d/db conn) :conformity/conformed-norms))))
(defn setup-app-schema [conn]
  (c/ensure-conforms conn norms-map [:clj-crm/norm1]))

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
;;   (d/touch (find-one-by (d/db conn) :user/email  "humorless@gmail.co"))
;;   => NPE
;;   (:user/pwd (find-one-by (d/db conn) :user/email "humorless@gmail.com"))
;;   => $pwd
(defn find-one-by
  "Given db value and an (attr/val), return the user as EntityMap (datomic.query.EntityMap)"
  [db attr val]
  (d/entity db
    (d/q '[:find ?e .
           :in $ ?attr ?val
           :where [?e ?attr ?val]]
     db attr val)))

(comment
  ;; example of upsert-user!
  (upsert-user! conn {:user-name "Laurence Chen"
                      :pwd "bcrypt+sha512$7b58b1516abd049081f655555b154270$12$1f97671825888b5dd330ba8e489774b2b1b076c55e991ba6"
                      :email "humorless@gmail.com"
                      :status :user.status/active
                      :roles  [:user.roles/sales :user.roles/read-only]}))

(comment
  ;; traditional way to create module public read API
  (defn get-user-by-email
    "Given db value and an email, return the user as Entity (datomic.query.EntityMap)"
    [db email]
    ;; find specification using single scalar form
    (->> (d/q '[:find ?e . :in $ ?m
                :where [?e :user/email ?m]]
              db email)
         (d/entity db)))

  ;; traditional way to create schema
  (defn create-schema []
    (let [schema [{:db/ident              :user/id
                   :db/valueType          :db.type/string
                   :db/cardinality        :db.cardinality/one
                   :db.install/_attribute :db.part/db}
                  {:db/ident              :user/first-name
                   :db/valueType          :db.type/string
                   :db/cardinality        :db.cardinality/one
                   :db.install/_attribute :db.part/db}
                  {:db/ident              :user/last-name
                   :db/valueType          :db.type/string
                   :db/cardinality        :db.cardinality/one
                   :db.install/_attribute :db.part/db}
                  {:db/ident              :user/email
                   :db/valueType          :db.type/string
                   :db/cardinality        :db.cardinality/one
                   :db.install/_attribute :db.part/db}]]
      @(d/transact conn schema)))
  ;; example codes along with luminus-generation
  (defn entity [conn id]
    (d/entity (d/db conn) id))

  (defn touch [conn results]
    "takes 'entity ids' results from a query
     e.g. '#{[272678883689461] [272678883689462] [272678883689459] [272678883689457]}'"
    (let [e (partial entity conn)]
      (map #(-> % first e d/touch) results)))

  (defn find-user [conn id]
    (let [user (d/q '[:find ?e :in $ ?id
                      :where [?e :user/id ?id]]
                    (d/db conn) id)]
      (touch conn user))))
