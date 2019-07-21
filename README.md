# hodur-graphql-gen
hodur-graphql-gen is a tool for generating full queries for testing purposes.
It uses model generated from [Hodur](https://github.com/hodur-org/hodur-engine),
making it integrated into project model domain.

### Getting started

Add `hodur-engine` and `hodur-graphql-gen` dependencies in your `deps.edn` file:
```
  {:deps
   {hodur/engine             {:mvn/version "0.1.6"}
    delfes/hodur-graphql-gen {:mvn/version "0.5.0"}}}
```

Require them:
```clojure
(require '[hodur-engine.core :as hodur]
         '[hodur-graphql-gen.core :as generator])
```

Using Hodur to generate our GraphQL model, for instance:
```clojure
(def meta-db (hodur/init-schema
              '[^:lacinia/tag
                default

                ^:lacinia/query
                QueryRoot
                [^UserQueries users]

                UserQueries
                [^User
                 findUser [^String namePattern]]
                 
                User
                [^ID id
                 ^String firstName
                 ^String lastName]]))
```

Now we get a full query with:
```clojure
(generator/generate-full-query meta-db)
```
Which generates:
```graphql
fragment fragUser on User {
  id
  firstName
  lastName
}

fragment fragUserQueries on UserQueries {
  findUser(namePattern: $findUser_namePattern) {
    ...fragUser
  }
}

query ($findUser_namePattern: String!) {
  users {
    ...fragUserQueries
  }
}
```

For selecting a specific node, we may pass a sequence of fields, for instance:
```clojure
(generator/generate-full-query meta-db ["users" "findUser"])
```

Mutations require a different method:
```clojure
(generator/generate-full-mutation meta-db "setUserInformation")
```

