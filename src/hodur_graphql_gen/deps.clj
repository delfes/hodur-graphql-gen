(ns hodur-graphql-gen.deps
  (:require [clojure.set :as cset]
            [hodur-graphql-gen.loaders :as loaders]))

(declare deps-for-type)

(defn ^:private field-typename [field-type]
  (get-in field-type [:field/type :type/name]))

(defn ^:private field-nature [field-type]
  (get-in field-type [:field/type :type/nature]))

(defn ^:private user-type-fields [type]
  (filter #(= (field-nature %) :user)
          (:field/_parent type)))

(defn ^:private deps-for-typename [meta-db typename & [deps-set]]
  (let [type (first (loaders/resolve-by-name meta-db typename))]
    (deps-for-type meta-db type deps-set)))

(defn ^:private deps-for-enum-type [type]
  (map :field/name (:field/_parent type)))

(defn deps-for-type [meta-db type & [deps-set]]
  (let [type-set #{(:type/name type)}]
    (when (not (cset/subset? type-set deps-set))
      (let [children-deps-set (if (:type/union type)
                                (set (deps-for-enum-type type))
                                (set (map #(field-typename %)
                                          (user-type-fields type))))
            current-deps-set (cset/difference children-deps-set deps-set)
            new-deps-set (cset/union deps-set type-set)]
        (cset/union (apply cset/union (map #(deps-for-typename meta-db % new-deps-set)
                                           current-deps-set))
                    deps-set
                    type-set)))))
