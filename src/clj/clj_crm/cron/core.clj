(ns clj-crm.cron.core
  (:require
   [chime :refer [chime-at]]
   [clj-time.core :as t]
   [clj-time.periodic :refer [periodic-seq]]
   [clojure.tools.logging :as log]
   [clj-crm.domain.query :as query]
   [clj-crm.fjr.time :as fjr-time]
   [clj-crm.config :refer [env]]
   [mount.core :refer [defstate]])
  (:import [org.joda.time DateTimeZone]))

;; cron-users should be ["userA1@example.com", "userB1@example.com"]
(defstate cron-users
  :start (-> env :cron-users)
  :stop [])

;; default cronjob start at 18:00 local time zone.
(def t-seq
  (->> (periodic-seq (.. (t/now)
                         (withZone (DateTimeZone/forID "Asia/Taipei"))
                         (withTime 18 0 0 0))
                     (-> 1 t/days))))

;; test-seq for only testing purpose
(def test-seq
  (->> (periodic-seq (t/now)
                     (-> 60 t/seconds))))

(def cancel-atom
  (atom (fn []
          (log/info "first time invoke the cancel-atom"))))
;; example job is
;; {:qs [:q1 :q2] :y 2019}}
(def jobs-atom (atom {}))

(defn quarter-str->int
  " input   \"q1\"
    output  0  "
  [k]
  (case k
    "q1" 0
    "q2" 1
    "q3" 2
    "q4" 3))

(defn- q-s->time-span
  [year-n q-s]
  (let [q-n (quarter-str->int q-s)]
    (fjr-time/year-quarter->y-m-set year-n q-n)))

(defn- ts->admin-query-request
  [ts]
  {:time-span ts
   :tx nil
   :user "cronjob"
   :q :all-full-join-reports})

(defn- ts->user-query-request-v
  [ts]
  (let [partial-req {:time-span ts
                     :tx nil
                     :q :my-full-join-reports}]
    (mapv #(assoc partial-req :user %) cron-users)))

(defn- execute
  [req]
  (log/info "cronjob cache calculation with query request: " req)
  (query/dispatch-q req)
  (Thread/sleep 10000))

(defn jobs->req-list [jobs]
  (let [year (:y jobs)
        qs   (:qs jobs)
        time-span-list (map #(q-s->time-span year %) qs)
        admin-req-list (map ts->admin-query-request time-span-list)
        user-req-list (mapcat ts->user-query-request-v time-span-list)]
    (concat admin-req-list user-req-list)))

(defn cache-calculator [t]
  (log/info "invoke cache calculation at time " t)
  (let [req-list (jobs->req-list @jobs-atom)]
    (run! execute req-list)))

(defn jobs []
  @jobs-atom)

(defn install-jobs
  "install the cronjobs"
  [quarters year]
  (do (reset! jobs-atom {:y year :qs quarters})
      (@cancel-atom) ;; cancel the last installed cronjobs
      (reset! cancel-atom
              (chime-at t-seq cache-calculator))))
