# svb-api
Silicon Valley Bank Clojure Api Sdk

## Install

Add the following line to you dependencies:
```clj
[svb-api "0.1.0"]
```

Load the sdk:
```clj
(require '[svb-api.core :as api])
```

## Setup

Before using the SDK the api-key and secret should be set.  HMAC signing will automatically occur if the secret is set.

```clj
(api/set-api-key
  "test_abcdefghijklmnopqrstuvwxyz")

(api/set-secret
  "qwertyuipasdfghjklzxcvbnm1234567890")
```

## Examples
Refer to the test suite for an exhaustive list of examples.

```clj
; print current version of api
(println @(index))

; print last 3 ach transactions
(let [achs (list-achs :paging {:limit 3})
      ach-ids (map :id (:data @achs))]
  (println (map (comp :data deref get-ach) ach-ids)))
```

## Testing

The following environment variables should be set before running the test suite:

* **SVB_API_TEST_KEY** - Api Key
* **SVB_API_TEST_SECRET** - HMAC Signing Secret
* **SVB_API_ACH_TEST_ACCOUNT** - Ach Account Number

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
