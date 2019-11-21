(ns clj-crm.domain.command
  (:require [clj-crm.db.core :as dcore :refer [conn]]
            [clj-crm.db.allocation :as dallo]
            [schema.core :as s]
            [clojure.tools.logging :as log]
            [clj-crm.db.user :as duser]
            [datomic.api :as d]))

(defn transact-tag-tx
  [date-str query-opt]
  (if (= date-str "now")
    (throw (ex-info "date-str as now is not allowed" {:causes "date-str equal now"}))
    (let [{eid :db/id} (d/pull (d/db conn) '[:db/id] [:history/tag date-str])
          tx-datum {:history/tag date-str
                    :history/queryable query-opt
                    :history/tx (d/t->tx (d/next-t (d/db conn)))}]
      (if (some? eid)
        @(d/transact conn [(dissoc tx-datum :history/tx)])
        @(d/transact conn [tx-datum])))))

(defn switch
  "Input:
   user-c is the form: {:user \"userA1@example.com\"
                         :c   :new-request}
   Output:
   :new-requests"
  [user-c]
  (:c user-c))

(defmulti dispatch-c switch)

(defmethod dispatch-c :modify-request
  [user-c]
  (log/info "at modify-request, user-c as" user-c)
  (let [id (get-in user-c [:req-op :id])
        stamp (get-in user-c [:req-op :stamp])
        add-list (get-in user-c [:req-op :add-list])
        remove-list (get-in user-c [:req-op :remove-list])
        tx-data (dcore/tx-modify-request id stamp add-list remove-list)]
    (log/info "at modify-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-c :reject-request
  [user-c]
  (log/info "at reject-request, user-c as" user-c)
  (let [id (get-in user-c [:req-op :id])
        stamp (get-in user-c [:req-op :stamp])
        tx-data (dcore/tx-reject-request id stamp)]
    (log/info "at reject-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-c :approve-request
  [user-c]
  (log/info "at approve-request, user-c as" user-c)
  (let [db (d/db conn)
        id (get-in user-c [:req-op :id])
        stamp (get-in user-c [:req-op :stamp])
        tx-data (dcore/tx-approve-request db id stamp)]
    (log/info "at approve-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-c :new-request
  [user-c]
  (log/info "at new-request, user-c as" user-c)
  (let [email (:user user-c)
        user-lookup-ref [:user/email email]
        add-list (get-in user-c [:req :add-list])
        remove-list (get-in user-c [:req :remove-list])
        tx-data [{:req/sales                user-lookup-ref
                  :req/add-customer-items    add-list
                  :req/remove-customer-items remove-list
                  :req/status               :req.status/open
                  :req/stamp                0}]]
    (log/info "at new-request, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(s/defschema customerItemSchema {:customerItem/customer s/Int
                                 :customerItem/product  s/Keyword})

(s/defschema newReqSchema {:add-list #{customerItemSchema}
                           :remove-list #{customerItemSchema}})

;; opReqSchema is for approve/reject/modify
;; :stamp is to make sure that even two admin operate on the same request
;; the request's approval/rejection/modification will be logically strict.
(s/defschema opReqSchema {:id s/Int
                          :stamp s/Int
                          (s/optional-key :add-list) #{s/Int}
                          (s/optional-key :remove-list) #{s/Int}})

(s/defschema CommandSchema {(s/required-key :c) s/Keyword
                            (s/optional-key :tx) s/Int
                            (s/optional-key :req) newReqSchema
                            (s/optional-key :req-op) opReqSchema})

(defn command
  " Input:
    c is in the form: [Command]
    user is in the form: {:user userA1@example.com, :exp ...}

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

(defmulti dispatch-del identity)

(defmethod dispatch-del :rev-allo
  [table-name]
  (log/info "at delete rev-allo" table-name)
  (let [db (d/db conn)
        eids (dallo/rev-allo-eids db)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete rev-allo, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-del :non-direct-allo
  [table-name]
  (log/info "at delete non-direct-allo" table-name)
  (let [db (d/db conn)
        eids (dallo/allo-non-direct-eids db)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete non-direct-allo, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-del :direct-allo
  [table-name]
  (log/info "at delete direct-allo" table-name)
  (let [db (d/db conn)
        eids (dallo/allo-direct-eids db)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete direct-allo, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defn- auxiliary-eids [db table-name]
  (d/q '[:find [?e ...]
         :in $ ?attr
         :where [?e ?attr _]] db (keyword (name table-name) "revenue")))

(defmethod dispatch-del :target
  [table-name]
  (log/info "at delete target" table-name)
  (let [db (d/db conn)
        eids (auxiliary-eids db table-name)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete target, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defmethod dispatch-del :pipeline
  [table-name]
  (log/info "at delete pipeline" table-name)
  (let [db (d/db conn)
        eids (auxiliary-eids db table-name)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete pipeline, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defn- rev-stream-eids-by
  [db etl-src a-t]
  (d/q '[:find [?e ...]
         :in $ ?s ?t
         :where
         [?e :rev-stream/source ?s]
         [?e :rev-stream/accounting-time ?t]]
       db etl-src a-t))

(defn- order-eids-by
  [db etl-src]
  (d/q '[:find [?e ...]
         :in $ ?s
         :where
         [?e :order/source ?s]]
       db etl-src))

(defn- order-gui-eids-by
  [db a-t]
  (d/q '[:find [?a ...]
         :in $ ?t
         :where
         [?e :order/source :etl.source/gui]
         [?e :order/accounting-data ?a]
         [?a :accounting/month ?t]]
       db a-t))

(defn- auxiliary-target-table-eids [db year-quarter]
  (d/q '[:find [?e ...]
         :in $ ?y-q
         :where [?e :target/year-quarterly ?y-q]]
       db year-quarter))

(defn delete-target
  "year-quarter is of type string"
  [year-quarter]
  (log/info "at delete target by year-quarter: " year-quarter)
  (let [db (d/db conn)
        eids (auxiliary-target-table-eids db year-quarter)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete target by year-quarter, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defn delete-order
  "etl-src is of type keyword"
  [etl-src]
  (log/info "at delete order" etl-src)
  (let [db (d/db conn)
        eids (order-eids-by db (keyword "etl.source" etl-src))
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete order, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defn delete-order-gui
  "a-t is of type string. a-t is for `accounting time`"
  [a-t]
  (log/info "at delete order gui, accounting-time is: " a-t)
  (let [db (d/db conn)
        eids (order-gui-eids-by db a-t)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete order, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))

(defn delete-rev-stream
  "etl-src is of type keyword
   a-t is of type string. a-t is for `accounting time`"
  [etl-src a-t]
  (log/info "at delete rev-stream" etl-src a-t)
  (let [db (d/db conn)
        eids (rev-stream-eids-by db (keyword "etl.source" etl-src) a-t)
        tx-data (mapv dcore/eid->retract-tx-v eids)]
    (log/info "at delete rev-stream, tx-data as" tx-data)
    (do @(d/transact conn tx-data)
        :cmd-success)))
