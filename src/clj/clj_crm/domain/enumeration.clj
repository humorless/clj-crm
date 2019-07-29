(ns clj-crm.domain.enumeration
  (:require [clj-crm.db.core :refer [conn]]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [datomic.api :as d]))

(def ^:private ident-tx-tmpl [:db/add #db/id [:db.part/user] :db/ident])

(defn- team-tx [t-n]
  (let [t-k (keyword "user.team" (string/lower-case t-n))
        tx (conj ident-tx-tmpl t-k)]
    [tx]))

(defn add-team [t-n]
  (let [tx (team-tx t-n)]
    (log/info "team tx is " tx)
    @(d/transact conn tx)))

;; the reture value of product-tx function is of the form:
(comment [[:db/add #db/id [:db.part/user] :db/ident :product.type/in_store_sales_promotion]
          {:product/type :product.type/in_store_sales_promotion :product/category :product.cat/all}
          {:product/type :product.type/in_store_sales_promotion :product/type-id "In Store Sales Promotion"}])

(defn- product-txes [p-n]
  (let [p-n-no-space (string/replace p-n #"\s" "_")
        p-k (keyword "product.type" (string/lower-case p-n-no-space))
        tx-ident (conj ident-tx-tmpl p-k)
        tx-category (zipmap [:product/type :product/category] [p-k :product.cat/all])
        tx-type-id (zipmap [:product/type :product/type-id] [p-k p-n])]
    [[tx-ident] [tx-category tx-type-id]]))

(defn add-product [p-n]
  (let [[tx-ident tx-maps] (product-txes p-n)]
    (log/info "product first tx is " tx-ident)
    @(d/transact conn tx-ident)
    (log/info "product second tx is " tx-maps)
    @(d/transact conn tx-maps)))
