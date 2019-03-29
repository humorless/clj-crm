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

;; verification
;; => (map d/touch (find-all-by (d/db conn) :conformity/conformed-norms)))
(defn setup-app-db [fname]
  (let [norms-map (c/read-resource fname)]
    (c/ensure-conforms conn norms-map (keys norms-map))))

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
       (map (partial d/entity db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn upsert-user!
  "Given the user entity has :db.unique/identity attribute, Datomic will upsert"
  [conn {:keys [user-name pwd email status roles team]}]
  @(d/transact conn [{:user/name      user-name
                      :user/pwd       pwd
                      :user/email     email
                      :user/status    status
                      :user/roles     roles
                      :user/team      team}]))

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
  "Returns all entities possessing attr.

   Output is `(entity ...)`"
  [db attr]
  (qes '[:find [?e ...]
         :in $ ?attr
         :where [?e ?attr]]
       db attr))

;; example:
;;   (d/touch (find-one-by (d/db conn) :user/email  "userA1@example.co"))
;;   => NPE
;;   (:user/pwd (find-one-by (d/db conn) :user/email "userA1@example.com"))
;;   => $pwd
(defn find-one-by
  "Given db value and an (attr/val), return the user as EntityMap (datomic.query.EntityMap)"
  [db attr val]
  (d/entity db
            (d/q '[:find ?e .
                   :in $ ?attr ?val
                   :where [?e ?attr ?val]]
                 db attr val)))

(defn get-customers-of-open-requests-by-user
  "For sales'own request, pull out its add-customer-list and remove-customer-list

  Output is `({REQ} ...)` or `()`
  {REQ} is of the form:
  {:req/add-customer-list    [{CUSTOMER-MAP} ...]
   :req/remove-customer-list [{CUSTOMER-MAP} ...]
   :req/time ...}"
  [db user]
  (let [req-get #(d/pull db '[{:req/add-customer-list [*]} {:req/remove-customer-list [*]}] %)]
    (->> (d/q '[:find ?e ?inst
                :in $ ?u
                :where
                [?e :req/sales ?u]
                [?e :req/status :req.status/open ?tx true]
                [?tx :db/txInstant ?inst]]
              db user)
         (map (fn [[req-eid inst]]
                (assoc (req-get req-eid) :req/time inst))))))

(defn marshal-customer
  "for input's field, remove the namespace of keyword, replace :db/id as :eid
   Also, for the enumeration like :customer/business-type and :customer/inventory-type, do the necessary marshalling
   Input:

   {CUSTOMER-MAP}"
  [db customer]
  (let [erase-namespace #(keyword (name %))
        eid (:db/id customer)
        c (dissoc customer :db/id)]
    (reduce (fn [acc [k v]]
              (if-let [enum (:db/id v)]
                (into acc {(erase-namespace k) (d/ident db enum)}) ;; handle the :business-type/:inventory-type
                (into acc {(erase-namespace k) v})))
            {:eid eid}
            c)))

(defn marshal-sales
  "Input:
   {:user/name \"sales name A1\"
    :user/team {:db/id     ...
                :team/name ...} }

   Outupt:
   {:name AAA
    :team BBB }"
  [db sales]
  (let [sname (:user/name sales)
        tname (get-in sales [:user/team :team/name])]
    (when sname
      {:name sname
       :team tname})))

(defn marshal-request
  "for input's field, remove the namespace of keyword
   Input:

   {:req/add-customer-list    [{CUSTOMER-MAP} ...]
    :req/remove-customer-list [{CUSTOMER-MAP} ...]
    :req/time #inst ...}

   {:req/add-customer-list    [{CUSTOMER-MAP} ...]
    :req/remove-customer-list [{CUSTOMER-MAP} ...]
    :req/time #inst
    :req/status ...
    :db/id      ...
    :req/sales  ... }

   There may be two forms of request"
  [req]
  (let [db (d/db conn)
        erase-namespace #(keyword (name %))
        t (:req/time req)
        eid (:db/id req)
        sales (:req/sales req)
        status-enum (get-in req [:req/status :db/id])]
    (reduce (fn [acc [k v]]
              (into acc {(erase-namespace k) (mapv #(marshal-customer db  %) v)}))
            {:allotime t
             :eid eid
             :sales (marshal-sales db sales)
             :status (d/ident db status-enum)}
            (dissoc req :req/time :db/id :req/sales :req/status))))

(defn marshal-left-joined-customer
  "Input is  database and `{LJ-CUSTOMER-MAP}`"
  [db lj-c]
  (let [sid  (get-in lj-c [:allo/_customer 0 :allo/sales :db/id])
        sname (get-in lj-c [:allo/_customer 0 :allo/sales :user/name])
        team-id (get-in lj-c [:allo/_customer 0 :allo/sales :user/team :db/id])
        team-name (get-in lj-c [:allo/_customer 0 :allo/sales :user/team :team/name])
        atime (get-in lj-c [:allo/_customer 0 :allo/time])
        c (dissoc lj-c :allo/_customer)
        c-with-sales (assoc c :sales {:eid sid
                                      :name sname
                                      :team {:name team-name
                                             :eid team-id}
                                      :allotime atime})]
    (if sid
      (marshal-customer db c-with-sales)
      (marshal-customer db c))))

(defn get-left-joined-customers
  "Output is `({LJ-CUSTOMER-MAP} ...)`

  {LJ-CUSTOMER-MAP} is in the form of:
  `
  {
   [normal-customer-field]*
   :allo/_customer [{:allo/sales {:db/id ...
                                  :user/name ...}}]}
  `
  However, field `:allo/_customer` does not always exist
  According to the business constraints, every customer should
  be allocated by only one sales
  "
  [db]
  (->> (d/q '[:find [?c ...]
              :in $
              :where
              [?c :customer/id]]
            db)
       (map #(d/pull db '[{:allo/_customer [{:allo/sales [:user/name
                                                          :db/id
                                                          {:user/team [:team/name :db/id]}]}
                                            :allo/time]}
                          *] %))))

(defn customer-eids-by-user
  "get the customers list currently allocated by user -- sales' own customer list"
  [db user]
  (d/q '[:find [?c ...]
         :in $ ?u
         :where
         [?e :allo/sales ?u]
         [?e :allo/customer ?c]]
       db user))

(defn c-eid->cust+sales
  "Transform customer eid -> {HashMap} with customer fields and sales fields

   the {HashMap} is in the form of
  `
  {
   [normal-customer-field]*
   :allo/_customer [{:allo/sales {:db/id ...
                                  :user/name ...}}]}
  `
  However, field `:allo/_customer` does not always exist
  According to the business constraints, every customer should
  be allocated by only one sales
  "
  [db c-eid]
  (d/pull db '[{:allo/_customer [{:allo/sales [:user/name
                                               :db/id
                                               {:user/team [:team/name :db/id]}]}
                                 :allo/time]}
               *] c-eid))

(defn get-open-requests-by-user
  "get the open request currently submitted by user -- sales' own request
   e.g.:
   (map d/touch (get-open-requests-by-user (d/db conn) [:user/email \"userA1@example.com\"]))

   Output is `(entity ...)`
   () or (#:db{:id 17592186045470} ...) "
  [db user]
  (->> (d/q '[:find [?e ...]
              :in $ ?u
              :where
              [?e :req/sales ?u]
              [?e :req/status :req.status/open]]
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

;; (request-open-time-by-eid (d/history (d/db conn)) 17592186045481))
(defn- request-open-time-by-eid
  "
  input is:
  hdb -> historyDB of the database,
  eid -> eid of a request.

  outuput is:
  the creation time of this request no matter
  if this request has been approved/modified/rejected.
  "
  [hdb eid]
  (d/q '[:find ?inst .
         :in $ ?v
         :where
         [?v :req/status :req.status/open ?tx true]
         [?tx :db/txInstant ?inst]]
       hdb eid))

(defn- active-request-eids
  "Output is `(req-eid ...)`"
  [db]
  (let [active-status [:req.status/open :req.status/modified]]
    (d/q '[:find [?e ...]
           :in $ [?status ...]
           :where
           [?e :req/status ?status]]
         db active-status)))

(defn get-active-requests
  "Output is `({REQ} ...)` or `()`

  {REQ} is of the form:
  {:req/add-customer-list    [{CUSTOMER-MAP} ...]
   :req/remove-customer-list [{CUSTOMER-MAP} ...]
   :req/status
   :req/sales ...
   :req/time ...}"
  [db]
  (let [hdb (d/history db)
        aq-eids (active-request-eids db)
        txInsts (map #(request-open-time-by-eid hdb %) aq-eids)
        join-req-fn #(d/pull db '[:db/id
                                  :req/status
                                  {:req/add-customer-list [*]}
                                  {:req/remove-customer-list [*]}
                                  {:req/sales [:user/name
                                               {:user/team [*]}]}] %)
        aq-maps (map join-req-fn aq-eids)]
    (map #(assoc %1 :req/time %2) aq-maps txInsts)))

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

;; (tx-reject-request 17592186045489 :req.status/open))
(defn tx-reject-request
  "for request n, mark it as rejected"
  [^long n statusNow]
  (let [eid (d/entid (d/db conn) statusNow)]
    [[:db.fn/cas n :req/status eid :req.status/rejected]]))

(defn- add-allo-table
  "add the allo table by vector of map

   Output is tx-data"
  [sid customer-list txInst]
  (let [sids (repeat sid)
        txInsts (repeat txInst)]
    (mapv (fn [s c t]
            {:allo/sales s
             :allo/customer c
             :allo/time t})
          sids
          customer-list
          txInsts)))

(defn- retract-allo-table
  "retract the allo table by :db.fn/retractEntity
   d/q -> calcaute the entity id as vector

   Output is tx-data"
  [db sid customer-list]
  (->> (d/q '[:find [?e ...]
              :in $ ?sid [?cid ...]
              :where
              [?e :allo/sales ?sid]
              [?e :allo/customer ?cid]]
            db sid customer-list)
       (mapv (fn [x] [:db.fn/retractEntity x]))))

(defn- request-content-by-eid
  "input is:
   eid -> eid of a request.

   outuput is: [sid add-customer-list remove-customer-list]
   add-customer-list     - [eid ...]
   remove-customer-list  - [eid ...]
  "
  [db eid]
  [(d/q '[:find ?s .        :in $ ?e :where [?e :req/sales ?s]] db eid)
   (d/q '[:find [?list ...] :in $ ?e :where [?e :req/add-customer-list ?list]] db eid)
   (d/q '[:find [?list ...] :in $ ?e :where [?e :req/remove-customer-list ?list]] db eid)])

;; (tx-approve-request 17592186045489 :req.status/open))
;; (tx-approve-request 17592186045489 :req.status/modified))
(defn tx-approve-request
  "for request n, mark it as approved
   First synchronize the allocation table.
   After sync, mark the request as approved."
  [^long n statusNow]
  (let [db (d/db conn)
        ;; prepare eids, txInst
        txInst (request-open-time-by-eid (d/history db) n)
        [sid add-list remove-list] (request-content-by-eid db n)
        ;; prepare tx-add-allo, tx-retract-allo
        tx-add-allo (add-allo-table sid add-list txInst)
        tx-retract-allo (retract-allo-table db sid remove-list)
        ;; prepare tx-app-req
        status-eid (d/entid db statusNow)
        tx-app-req [[:db.fn/cas n :req/status status-eid :req.status/approved]]
        tx-data (into [] (concat tx-add-allo tx-retract-allo tx-app-req))]
    tx-data))

;; 要設法測完 :
;; (1) request with add-customer-list, remove-customer-list
;; (2) request with add-customer-list only
;; (3) request with remove-customer-list only
