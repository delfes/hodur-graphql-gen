(ns hodur-graphql-gen.core
  (:require [clojure.set :as cset]
            [clojure.string :as string]
            [hodur-graphql-gen.deps :as deps]
            [hodur-graphql-gen.loaders :as loaders]
            [hodur-engine.core :as hodur-engine]))

(defn ^:private frag-name [obj]
  (str "frag" (:type/name obj)))

(defn ^:private obj-field [obj field-name]
  (->> (:field/_parent obj)
       (filter (comp #{field-name} :field/name))
       first))

(defn ^:private generate-arguments [field]
  (when (:param/_parent field)
    (str "("
         (string/join ", "
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
                       (loaders/resolve-by-name meta-db field-name))]
    (if (or (= (:type/nature field-type) :primitive)
            (:type/enum field-type))
      (str field-name (generate-arguments field))
      (str field-name (generate-arguments field)
           " { "
           "..." (frag-name field-type)
           " }"))))

(defn ^:private generate-fragment [meta-db type]
  (let [fields (->> (:field/_parent type)
                    (filter :lacinia/tag)
                    (map (partial generate-fragment-field meta-db)))]
    (str "fragment " (frag-name type) " on " (:type/name type)
         " { "
         (if (empty? fields)
           "__typename"
           (string/join " "
                        (if (:type/union type)
                          (map (partial str " ... on ") fields)
                          fields)))
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

(def ^:private primitive-type-map
  {"String"   'String
   "Integer"  'Int
   "Float"    'Float
   "Boolean"  'Boolean
   "ID"       'ID
   "DateTime" 'String})

(defn ^:private get-type-reference
  [{:keys [type/name type/nature]}]
  (if (= :user nature)
    name
    (get primitive-type-map name)))

(defn ^:private field-type-string [field]
  (let [base-type (get-type-reference (:param/type field))
        cardinality (:param/cardinality field)]
    (if (or (nil? cardinality)
            (cardinality-one-to-one? cardinality))
      (str base-type
           (when-not (true? (:param/optional field))
             "!"))
      (str "[" base-type "!]"
           (when-not (true? (:param/optional field))
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
       (remove nil?)
       (apply merge)
       (map generate-params-for-field)))

(defn ^:private gen-field-query [mid-string obj-list fields-list]
  (let [obj (first obj-list)
        field-name (first fields-list)]
    (if-let [field (obj-field obj field-name)]
      (str " { "
           field-name
           (generate-arguments field)
           (gen-field-query mid-string (rest obj-list) (rest fields-list))
           " } ")
      mid-string)))

(defn ^:private params-for-fields [obj-list fields-list]
  (let [obj (first obj-list)
        field-name (first fields-list)]
    (when-let [field (obj-field obj field-name)]
      (cons (params-list-field field) (params-for-fields (rest obj-list)
                                                         (rest fields-list))))))

(defn ^:private generate-query [meta-db obj-list fields-list user-types]
  (when-let [obj-type (last obj-list)]
    (let [fields (->> (:field/_parent obj-type)
                      (filter :lacinia/tag)
                      (map (partial generate-fragment-field meta-db)))
          query-args (->> (params-for-fields obj-list fields-list)
                          flatten
                          (remove nil?)
                          (apply merge)
                          (map generate-params-for-field)
                          (concat (generate-params-query user-types))
                          flatten
                          (string/join ", "))]
      (str "query "
           (when-not (empty? query-args)
             (str "(" query-args ")"))
           (gen-field-query (str " { "
                                 (if (empty? fields-list)
                                   (string/join " "
                                                (if (:type/union obj-type)
                                                  (map (partial str " ... on ") fields)
                                                  fields))
                                   (str "..." (frag-name obj-type)))
                                 " } ")
                            obj-list
                            fields-list)))))

(defn ^:private generate-mutation [meta-db type user-types]
  (let [fields (->> (:field/_parent type)
                    (map (partial generate-fragment-field meta-db)))]
    (str "mutation ("
         (string/join ", " (flatten (generate-params-query (conj user-types type))))
         ") { "
         (string/join " "
                      (if (:type/union type)
                        (map (partial str " ... on ") fields)
                        fields))
         " }")))

(defn ^:private obj-path-list
  ([meta-db fields-list]
   (obj-path-list meta-db fields-list (first (loaders/load-queries meta-db))))
  ([meta-db fields-list parent]
   (let [obj (when-let [field (first fields-list)]
               (when-let [typename (->> (obj-field parent field)
                                        :field/type
                                        :type/name)]
                 (when-let [child (loaders/resolve-by-name meta-db typename)]
                   (obj-path-list meta-db (rest fields-list) child))))]
     (cons parent obj))))

(defn generate-full-query [meta-db & [fields-list]]
  (let [obj-list (obj-path-list meta-db fields-list)
        root-query (last obj-list)
        user-types (->> (cond-> (deps/deps-for-type meta-db root-query)
                                (empty? fields-list) (cset/difference #{(:type/name root-query)}))
                        (map (partial loaders/resolve-by-name meta-db))
                        flatten)]
    (str (string/join "\n"
                      (->> (remove (comp true? :type/enum) user-types)
                           (filter :lacinia/tag)
                           (map (partial generate-fragment meta-db))))
         "\n"
         (generate-query meta-db obj-list fields-list user-types))))

(defn generate-full-mutation [meta-db mutation-name]
  (let [mutations (loaders/load-mutations meta-db)
        mutation-root (first
                        (filter
                          (fn [mutation]
                            (some (comp #{mutation-name} :field/name)
                                  (:field/_parent mutation)))
                          mutations))
        filtered-mutation (->> (:field/_parent mutation-root)
                               (filter #(= mutation-name (:field/name %)))
                               (assoc mutation-root :field/_parent))
        user-types (flatten (map #(loaders/resolve-by-name meta-db %)
                                 (cset/difference (deps/deps-for-type meta-db filtered-mutation)
                                                  #{(:type/name filtered-mutation)})))]
    (str (string/join "\n"
                      (->> user-types
                           (filter #(not= (:type/enum %) true))
                           (map #(generate-fragment meta-db %))))
         "\n"
         (generate-mutation meta-db filtered-mutation user-types))))

(comment
  (def meta-db (hodur-engine/init-schema
                 '[^:lacinia/tag
                   default

                   ^:lacinia/query
                   QueryRoot
                   [^SystemQueries system
                    ^UserQueries users]

                   ^{:lacinia/tag false}
                   NotLaciniaModel
                   [^String someRandomValue]

                   ^:lacinia/query
                   QueryRoot
                   [^RandomQueries random
                    ^{:type NotLaciniaModel
                      :lacinia/tag false} somethingElse]

                   SystemQueries
                   [^Boolean online
                    ^Integer uptime [^{:type Integer} timezone]
                    ^{:type SystemProps
                      :lacinia/tag false} props]

                   SystemProps
                   [^String name
                    ^Integer cpus]

                   RandomQueries
                   [^String someInfo]

                   ^:lacinia/mutation
                   MutationRoot
                   [^RegularUser createUser [^String name]
                    ^bool removeUser [^RegularUser user]]

                   ^:lacinia/mutation
                   MngtMutationRoot
                   [^bool clearUser [^String name]]

                   UserQueries
                   [^UserType
                    findUser [^String namePattern]
                    ^{:type RegularUser
                      :cardinality [0 n]}
                    findUsers [^{:type String
                                 :cardinality [0 n]} namePatternList
                               ^Integer maxResults]]

                   ^:union
                   UserType
                   [RegularUser
                    AdminUser]

                   Team
                   [^AdminUser owner
                    ^{:type RegularUser
                      :cardinality [0 n]} participants]

                   RegularUser
                   [^ID id
                    ^String name
                    ^{:type Team
                      :optional true} team]

                   AdminUser
                   [^ID id
                    ^String name
                    ^Permission permission
                    ^{:type ID
                      :lacinia/tag false} internalId]

                   ^:enum
                   Permission
                   [FULL
                    REGULATED]]))
  (def root-type (first (loaders/load-queries meta-db)))
  #_(deps/deps-for-type meta-db root-type)
  (println (generate-full-query meta-db)))
