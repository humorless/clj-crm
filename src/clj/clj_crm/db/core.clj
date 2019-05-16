(ns clj-crm.db.core
  (:require [datomic.api :as d]
            [io.rkn.conformity :as c]
            [mount.core :refer [defstate]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-time.core :as time.core]
            [clj-time.format :as time.format]
            [clj-time.periodic :as time.periodic]
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

;; Given that every db-fn entity has :db/ident
;; The (setup-db-fn) has `upsert` semantic, which means that
;; if we modify a db-fn :xxx and (setup-db-fn), and the :xxx will be updated.
(defn setup-db-fn []
  (let [data-fn-tx (c/read-resource "data-functions.edn")]
    @(d/transact conn data-fn-tx)))

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

(defn query-db-fn
  "Given db-fn-name, ex: :fn/replace-to-many
   Return the db function, which can be tested at REPL"
  [db db-fn-name]
  (d/q '[:find ?f .
         :in $ ?name
         :where
         [?e :db/ident ?name]
         [?e :db/fn ?f]]
       db db-fn-name))

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

(defn- marshal-field-name
  "if the field-name is :db/id, transform it to :eid
   else remove the namespace of the field-name"
  [field-name]
  (if (= field-name :db/id)
    :eid
    (keyword (name field-name))))

(defn recur-marshal
  "recursively marshal out the {HashMap} of data"
  [db m]
  (cond
    (string? m) m
    (number? m) m
    (= (class m) java.util.Date) m ;; for #inst
    (vector? m) (mapv #(recur-marshal db %) m) ;; for vector
    (and (= (class m) clojure.lang.PersistentArrayMap) (= (count m) 1)) (d/ident db (:db/id m)) ;; for enumeration
    :else (reduce (fn [acc [k v]]
                    (into acc {(marshal-field-name k) (recur-marshal db v)}))
                  {} m)))

(defn user-eids
  [db]
  (d/q '[:find [?e ...] :in $ ?a :where [?e ?a]] db :user/name))

(defn u-eid->user
  "Transfrom user eid -> {HashMap with user fields}"
  [db eid]
  (d/pull db '[:user/email :user/name :user/roles :user/team] eid))

(defn team-enum-eids
  "all the team enumeration eids"
  [db]
  (d/q '[:find [?e ...]
         :in $ ?nsp
         :where [?e :db/ident ?attr]
         [(namespace ?attr) ?nsp]]     ;;Datomic Function expression binds the ?nsp variable
       db "user.team"))

(defn product-enum-eids
  "all the product enumeration eids"
  [db]
  (d/q '[:find [?e ...]
         :in $ ?nsp
         :where [?e :db/ident ?attr]
         [(namespace ?attr) ?nsp]]     ;;Datomic Function expression binds the ?nsp variable
       db "product.type"))

(defn eid->enum
  [db eid]
  (d/pull db '[:db/id] eid))

(defn customer-eids
  "all the customer eids"
  [db]
  (d/q '[:find [?c ...]
         :in $
         :where
         [?c :customer/id]]
       db))

(defn user-customer-eids
  "get the customers list currently allocated by user -- sales' own customer list"
  [db user]
  (d/q '[:find [?c ...]
         :in $ ?u
         :where
         [?e :allo/sales ?u]
         [?e :allo/customer ?c]]
       db user))

(defn c-eid->cust+sales+prod
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
                                               :user/team]}
                                 :allo/time
                                 :allo/product]}
               *] c-eid))

;; (r-eid->request-open-time (d/history (d/db conn)) 17592186045481))
(defn r-eid->request-open-time
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
         :in $ ?e
         :where
         [?e :req/status :req.status/open ?tx true]
         [?tx :db/txInstant ?inst]]
       hdb eid))

;; (active-request-eids [[45 :req/status :req.status/modified] [46 :req/status :req.status/open] [47 :req/status :req.status/rejected
(defn active-request-eids
  "Output is `(req-eid ...)`"
  [db]
  (let [active-status [:req.status/open :req.status/modified]]
    (d/q '[:find [?e ...]
           :in $ [?status ...]
           :where
           [?e :req/status ?status]]
         db active-status)))

(defn user-active-request-eids
  "Output is `(req-eid ...)`"
  [db user]
  (let [active-status [:req.status/open :req.status/modified]]
    (d/q '[:find [?e ...]
           :in $ ?u  [?status ...]
           :where
           [?e :req/sales ?u]
           [?e :req/status ?status]]
         db user active-status)))

(defn r-eid->req
  "Transform request eid -> {HashMap}"
  [db eid]
  (d/pull db '[:db/id
               :req/status
               :req/stamp
               {:req/add-customer-items [{:customerItem/customer [*]} *]}
               {:req/remove-customer-items [{:customerItem/customer [*]} *]}
               {:req/sales [:user/name
                            :user/team]}] eid))

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

(defn tx-modify-request
  "for request e, modify its add-customer-items and remove-customer-items
   Note that: add-items and remove-items can be nil"
  [^long e stamp add-items remove-items]
  [[:fn/replace-to-many e :req/add-customer-items add-items]
   [:fn/replace-to-many e :req/remove-customer-items remove-items]
   [:db.fn/cas e :req/stamp stamp (inc stamp)]
   [:db/add e :req/status :req.status/modified]])

(defn tx-reject-request
  "for request e, mark it as rejected"
  [^long e stamp]
  [[:db.fn/cas e :req/stamp stamp (inc stamp)]
   [:db/add e :req/status :req.status/rejected]])

(defn- tx-add-allo-table
  "add the allo table by vector of map

   Output is tx-data"
  [db sid customer-items txInst]
  (let [sids (repeat sid)
        txInsts (repeat txInst)
        c-p-rels (d/q '[:find ?c ?p
                        :in $ [?ci ...]
                        :where
                        [?ci :customerItem/customer ?c]
                        [?ci :customerItem/product ?p]]
                      db customer-items)]
    (mapv (fn [s c-p-tuple t]
            {:allo/sales s
             :allo/customer (first c-p-tuple)
             :allo/product  (second c-p-tuple)
             :allo/time t})
          sids
          c-p-rels
          txInsts)))

(defn- tx-retract-allo-table
  "retract the allo table by :db.fn/retractEntity
   d/q -> calcaute the entity id as vector

   Output is tx-data"
  [db sid customer-items]
  (->> (d/q '[:find [?e ...]
              :in $ ?sid [?ci ...]
              :where
              [?e :allo/sales ?sid]
              [?e :allo/customer ?c]
              [?e :allo/product ?p]
              [?ci :customerItem/customer ?c]
              [?ci :customerItem/product ?p]]
            db sid customer-items)
       (mapv (fn [x] [:db.fn/retractEntity x]))))

(defn- r-eid->request-content
  "input is:
   eid -> eid of a request.

   outuput is: [sid add-customer-items remove-customer-items]
   add-customer-items     - [eid ...]
   remove-customer-items  - [eid ...]
  "
  [db eid]
  [(d/q '[:find ?s .        :in $ ?e :where [?e :req/sales ?s]] db eid)
   (d/q '[:find [?ci ...] :in $ ?e :where [?e :req/add-customer-items ?ci]] db eid)
   (d/q '[:find [?ci ...] :in $ ?e :where [?e :req/remove-customer-items ?ci]] db eid)])

;; (tx-approve-request 17592186045489 0))
(defn tx-approve-request
  "for request e, mark it as approved
   First synchronize the allocation table.
   After sync, mark the request as approved."
  [^long e stamp]
  (let [db (d/db conn)
        ;; prepare eids, txInst
        txInst (r-eid->request-open-time (d/history db) e)
        [sid add-list remove-list] (r-eid->request-content db e)
        ;; prepare tx-add-allo, tx-retract-allo
        tx-add-allo (tx-add-allo-table db sid add-list txInst)
        tx-retract-allo (tx-retract-allo-table db sid remove-list)
        ;; prepare tx-req-status
        tx-req-status [[:db.fn/cas e :req/stamp stamp (inc stamp)]
                       [:db/add e :req/status :req.status/approved]]
        tx-data (into [] (concat tx-add-allo tx-retract-allo tx-req-status))]
    tx-data))

(def order-match-rules
  '[[(direct-allo-customer-order ?a ?o)
     [?a :allo/customer ?c]
     [?o :order/customer ?c]]
    [(direct-allo-product-order ?a ?o ?p-keyword)
     [?a :allo/product ?p-category]
     [?p :product/category ?p-category]
     [?p :product/type ?sc]
     [?o :order/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]
    [(allo-time-order ?a ?o ?less)
     [?a :allo/time ?a-t]
     [?o :order/io-writing-time ?o-t]
     [(compare ?a-t ?o-t) ?less]]
    [(indirect-allo-customer-order ?a ?o)
     [?a :allo/customer ?c]
     [?o :order/channel ?c]]
    [(indirect-allo-product-order ?a ?o ?p-keyword)
     [?a :allo/product ?sc]
     [?o :order/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]])

(defn- agency-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-order ?a ?o)
         (indirect-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]
         (not-join [?o ?less]
                   (direct-allo-customer-order ?b ?o)
                   (direct-allo-product-order ?b ?o _)
                   (allo-time-order ?b ?o ?less))]
       db order-match-rules u-eid -1))

(defn- reseller-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-order ?a ?o)
         (indirect-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]]
       db order-match-rules u-eid -1))

(defn- direct-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (direct-allo-customer-order ?a ?o)
         (direct-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]]
       db order-match-rules u-eid -1))

(defn u-eid->orders [db u-eid]
  (let [chan  (d/q '[:find ?chan-keyword .
                     :in $ ?u
                     :where
                     [?u :user/channel ?chan]
                     [?chan :db/ident ?chan-keyword]]
                   db u-eid)]
    (case chan
      :user.channel/direct (direct-u-eid->orders db u-eid)
      :user.channel/reseller (reseller-u-eid->orders db u-eid)
      :user.channel/agency (agency-u-eid->orders db u-eid)
      [])))

(defn- o-eid->normal-revenues
  [db o-eid]
  (d/q '[:find ?pui ?m ?p-keyword ?r
         :in $ ?o
         :where
         [?o :order/product-unique-id ?pui]
         [?o :order/accounting-data ?ad]
         [?o :order/service-category-enum ?sc]
         [?sc :db/ident ?p-keyword]
         [?ad :accounting/month ?m]
         [?ad :accounting/revenue ?r]] db o-eid))

(defn- o-eid->delta-revenues
  "specify net-revenue at the starting month, just like a delta revenue"
  [db o-eid]
  (let [date->month (fn [s]
                      (string/join "-" (butlast (string/split s #"-"))))]
    (->>  (d/q '[:find ?pui ?sd ?p-keyword ?np
                 :in $ ?o
                 :where
                 [?o :order/product-unique-id ?pui]
                 [?o :order/service-category-enum ?sc]
                 [?sc :db/ident ?p-keyword]
                 [?o :order/product-net-price ?np]
                 [?o :order/terms-start-date ?sd]] db o-eid)
          (mapv #(update % 1 date->month)))))

(def td-fmt-date (time.format/formatter "yyyy-MM-dd"))

(def td-fmt-y-m (time.format/formatter "yyyy-MM"))

(defn- date-str->dt [date-str]
  (time.format/parse td-fmt-date date-str))

(defn- dt->y-m-str [dt]
  (time.format/unparse td-fmt-y-m dt))

(defn- days-of-closed-closed-dt
  [s-dt e-dt]
  (time.core/in-days (time.core/interval s-dt (time.core/plus e-dt (time.core/days 1)))))

(defn- revenue-per-day
  [total-revenue days]
  (/ total-revenue days))

(defn- frequencies-by [f coll]
  (let [gp (group-by f coll)]
    (zipmap (keys gp)  (map #(count (second %)) gp))))

(defn- month-dts->month-revenue-tuple
  [pui p-name r-p-d [y-m-str number-of-day]]
  (let [revenue (* r-p-d number-of-day)
        r (Math/round (double revenue))]
    [pui y-m-str p-name r]))

(defn- o-eid->day-revenues
  "specify net-revenue according to how many days per month has"
  [db o-eid]
  (let [[pui p-name np sd ed]  (d/q '[:find  [?pui ?p-keyword ?np ?sd ?ed]
                                      :in $ ?o
                                      :where
                                      [?o :order/product-unique-id ?pui]
                                      [?o :order/service-category-enum ?sc]
                                      [?sc :db/ident ?p-keyword]
                                      [?o :order/product-net-price ?np]
                                      [?o :order/terms-start-date ?sd]
                                      [?o :order/terms-end-date ?ed]] db o-eid)
        s-dt (date-str->dt sd) ;; dt stands for class #clj-time/date-time
        e-dt (date-str->dt ed)
        span-days-int (days-of-closed-closed-dt s-dt e-dt)
        r-p-d  (revenue-per-day np span-days-int)]
    (->> (time.periodic/periodic-seq s-dt (time.core/days 1))
         (take span-days-int)
         (frequencies-by #(dt->y-m-str %))
         (map #(month-dts->month-revenue-tuple pui p-name r-p-d %))
         (sort-by second))))

(defn- o-tuple->revenues
  "Example output: #{[17592186045536 \"2019-02\" :product.type/line_now 100]
                     [17592186045536 \"2019-03\" :product.type/more_tab 200] ...}"
  [db [o-eid p-type & more]]
  (case p-type
    :product.type/line_point (o-eid->delta-revenues db o-eid)
    :product.type/SS (o-eid->delta-revenues db o-eid)
    :product.type/line_point_code_tw (o-eid->day-revenues db o-eid)
    :product.type/today (o-eid->day-revenues db o-eid)
    (o-eid->normal-revenues db o-eid)))

(def quarter-table
  {"01" :q1
   "02" :q1
   "03" :q1
   "04" :q2
   "05" :q2
   "06" :q2
   "07" :q3
   "08" :q3
   "09" :q3
   "10" :q4
   "11" :q4
   "12" :q4})

(defn- month-lookup [tuple]
  (let [date-str (second tuple)
        month-str (second (string/split date-str #"-"))]
    (keyword month-str)))

(defn- quarter-lookup [tuple]
  (let [date-str (second tuple)
        month-str (second (string/split date-str #"-"))]
    (get quarter-table month-str)))

(defn- year-lookup [tuple]
  (let [date-str (second tuple)
        year-str (first (string/split date-str #"-"))]
    (keyword year-str)))

(defn- update-map [m f]
  (reduce-kv (fn [m k v]
               (assoc m k (f v))) {} m))

(defn- sum-over-tuples [tuples]
  (walk/walk last #(apply + %) tuples))

(defn- group-by-year [coll]
  (group-by year-lookup coll))

(defn- group-by-quarter [coll]
  (group-by quarter-lookup coll))

(defn- group-by-month [coll]
  (group-by month-lookup coll))

(defn- sum-and-raw [tuples]
  {:sum (sum-over-tuples tuples)
   :raw tuples})

(defn orders->revenue-report
  [db orders]
  (let [revenues     (mapcat #(o-tuple->revenues db %) orders) ;; vector of revenue tuple
        y-revenues   (group-by-year revenues)
        y-q-revenues (update-map y-revenues group-by-quarter)
        y-m-revenues (update-map y-revenues group-by-month)
        y-q-sum-revenues (update-map y-q-revenues
                                     #(update-map % sum-and-raw))
        y-m-sum-revenues (update-map y-m-revenues
                                     #(update-map % sum-and-raw))]
    [y-q-sum-revenues y-m-sum-revenues]))
