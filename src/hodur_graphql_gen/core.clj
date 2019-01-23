(ns hodur-graphql-gen.core
  (:require [clojure.set :as cset]
            [clojure.string :as string]
            [hodur-graphql-gen.deps :as deps]
            [hodur-graphql-gen.loaders :as loaders]
            [hodur-engine.core :as hodur-engine]))

(defn ^:private generate-arguments [field]
  (when (:param/_parent field)
    (str "("
         (string/join " "
                      (map #(str (:param/name %)
                                 ": $"
                                 (:field/name field)
                                 "_"
                                 (:param/name %))
                           (:param/_parent field)))
         ")")))

(defn ^:private generate-fragment-field [meta-db field]
  (let [field-name (:field/name field)
        field-type (or (:field/type field)
                       (first (loaders/resolve-by-name meta-db field-name)))]
    (if (or (= (:type/nature field-type) :primitive)
            (:type/enum field-type))
      (str field-name (generate-arguments field))
      (str field-name (generate-arguments field)
           " { "
           "...frag" (:type/name field-type)
           " }"))))

(defn ^:private generate-fragment [meta-db type]
  (let [type-name (:type/name type)
        fields (->> (:field/_parent type)
                    (map #(generate-fragment-field meta-db %)))]
    (str "fragment frag" type-name " on " type-name " { "
         (string/join " "
                      (if (:type/union type)
                        (map #(str " ... on " %) fields)
                        fields))
         " }")))

(defn ^:private params-list-field [field]
  (when (:param/_parent field)
    {(:field/name field) (:param/_parent field)}))

(defn ^:private params-list [type]
  (->> (:field/_parent type)
       (map params-list-field)
       flatten))

(defn ^:private cardinality-one-to-one? [cardinality]
  (and (= (first cardinality) 1)
       (or (nil? (second cardinality))
           (= (second cardinality) 1))))

(defn ^:private field-type-string [field]
  (let [base-type (-> field :param/type :type/name)
        cardinality (:param/cardinality field)]
    (if (or (nil? cardinality)
            (cardinality-one-to-one? cardinality))
      (str base-type
           (when (not= (:param/optional field) true)
             "!"))
      (str "[" base-type "!]"
           (when (not= (:param/optional field) true)
             "!")))))

(defn ^:private generate-params-for-field [[name arguments]]
  (map #(str "$"
             name
             "_"
             (:param/name %)
             ": "
             (field-type-string %))
       arguments))

(defn ^:private generate-params-query [types]
  (->> types
       (map params-list)
       flatten
       (filter some?)
       (apply merge)
       (map generate-params-for-field)))

(defn ^:private generate-query [meta-db type user-types]
  (let [fields (->> (:field/_parent type)
                    (map #(generate-fragment-field meta-db %)))]
    (str "query("
         (string/join ", " (flatten (generate-params-query user-types)))
         ") { "
         (string/join " "
                      (if (:type/union type)
                        (map #(str " ... on " %) fields)
                        fields))
         "}")))


(defn generate-full-query [meta-db]
  (let [root-query (first (loaders/load-queries meta-db))
        user-types (flatten (map #(loaders/resolve-by-name meta-db %)
                                 (cset/difference (deps/deps-for-type meta-db root-query)
                                                  #{(:type/name root-query)})))]
    (str (string/join "\n"
                      (->> user-types
                           (filter #(not= (:type/enum %) true))
                           (map #(generate-fragment meta-db %))))
         "\n"
         (generate-query meta-db root-query user-types))))


(comment
  (let [db (hodur-engine/init-schema
             '[^:lacinia/tag
               default

               ^:lacinia/query
               QueryRoot
               [^UserQueries users]

               UserQueries
               [^UserType
                findUser [^String namePattern]
                ^{:type RegularUser
                  :optional true}
                whoAmI
                ^{:type RegularUser
                  :cardinality [0 n]}
                findUsers [^{:type String
                             :cardinality [0 n]} namePatternList]]

               ^:union
               UserType
               [RegularUser
                AdminUser]

               RegularUser
               [^ID id
                ^String name]

               AdminUser
               [^ID id
                ^String name
                ^Permission permission]

               ^:enum
               Permission
               [FULL
                REGULATED]])
        root-type (first (loaders/load-queries db))]
    #_(deps/deps-for-type db root-type)
    (generate-full-query db)))
