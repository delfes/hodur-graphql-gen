(ns hodur-graphql-gen.loaders
  (:require [datascript.core :as d]))

(def ^:private selector
  '[* {:field/_parent
       [* {:field/type [*]
           :param/_parent
                       [* {:param/type [*]}]}]}])

(defn resolve-by-name [meta-db name]
  (-> (d/q '[:find [(pull ?e ?selector) ...]
             :in $ ?selector ?name
             :where
             [?e :type/name ?name]
             [?e :type/nature :user]]
           @meta-db
           selector
           name)
      first))

(defn load-types [meta-db]
  (d/q '[:find [(pull ?e ?selector) ...]
         :in $ ?selector
         :where
         [?e :lacinia/tag true]
         [?e :type/name]]
       @meta-db
       selector))

(defn load-queries [meta-db]
  (d/q '[:find [(pull ?e ?selector) ...]
         :in $ ?selector
         :where
         [?e :lacinia/query true]]
       @meta-db
       selector))

(defn load-mutations [meta-db]
  (d/q '[:find [(pull ?e ?selector) ...]
         :in $ ?selector
         :where
         [?e :lacinia/mutation true]]
       @meta-db
       selector))
