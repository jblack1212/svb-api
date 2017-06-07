;;-----------------------------------------------------------------------------
;; SVB Api SDK
;;-----------------------------------------------------------------------------

(ns svb-api.core
  (:require
    [clojure.string :as string]
    [clojure.walk :as walk ]
    [clj-time.core :as t]
    [clj-time.coerce :as tc]
    [cheshire.core :as json]
    [org.httpkit.client :as http-client])
  (:import java.lang.Math
    org.apache.commons.codec.digest.HmacUtils))

;; - utils --------------------------------------------------------------------

(defn generate-hmac
  [secret timestamp method uri query body]
  (let [method-str (string/upper-case (name method))
        query-str (#'http-client/query-string query)
        data (string/join "\n" [timestamp method-str uri query-str body])]
    (HmacUtils/hmacSha256Hex secret data)))

(let [api-key (atom nil)
      secret (atom nil)]
  (defn set-api-key [k]
    (reset! api-key k))
  (defn clear-api-key []
    (reset! api-key nil))
  (defn set-secret [s]
    (reset! secret s))
  (defn clear-secret []
    (reset! secret nil))
  (defn generate-headers [timestamp method uri query body]
    (merge {"Authorization" (str "Bearer " @api-key)
            "X-Timestamp" timestamp}
           (when-not (nil? @secret)
             {"X-Signature" (generate-hmac @secret timestamp method uri query body)})
           (when-not (nil? body)
             {"Content-Type" "application/json; charset=utf-8"}))))

(def not-nil?
  (complement nil?))

(defn is-error?
  [obj]
  (contains? obj :error))

(defn is-ok?
  [obj]
  (contains? obj :data))

(defn get-timestamp []
  (quot (tc/to-long (t/now)) 1000))

(defn api-url
  [& path]
  (apply str "https://api.svb.com" path))

(defn json-data
  [data]
  (when-not (nil? data)
    (json/encode {:data data})))

(defn transform-response
  [response]
  (let [body (:body response)
        result (walk/keywordize-keys (json/parse-string body))]
    (if (is-error? result)
      (update-in result [:error] (fn [e] {:code (:status response)
                                          :message e}))
      result)))

(defn http
  [method uri & {:keys [query data]}]
  (let [body (json-data data)
        timestamp (get-timestamp)
        headers (generate-headers timestamp method uri query body)]
    (http-client/request {:url (api-url uri)
                          :method method
                          :headers headers
                          :query-params query
                          :body body}
                         transform-response)))

(defn http-file
  [uri & {:keys [query data]}]
  (let [timestamp (get-timestamp)
        headers (generate-headers timestamp :post uri query nil)
        metadata (:metadata data)]
    (http-client/request {:url (api-url uri)
                          :method :post
                          :headers headers
                          :multipart (concat [{:name "file"
                                               :content (:content data)
                                               :content-type (:mime_type data)
                                               :filename (:filename data)}]
                                       (when metadata
                                         [{:name "metadata"
                                           :content (json/encode (:metadata data))}]))}
                         transform-response)))

;; - root ---------------------------------------------------------------------

(defn index
  "Get api information."
  []
  (http :get "/v1"))

;; - accounts -----------------------------------------------------------------

(defn list-accounts
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/accounts" :query query)))

(defn get-account
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/accounts/" id)))

(defn list-account-transactions
  [id & {:keys [paging filters]}]
  {:pre [(not-nil? id)]}
  (let [query (merge nil
                     (when paging {:page paging})
                     (when filters {:filter filters}))]
    (http :get (str "/v1/accounts/" id "/transactions") :query query)))

;; - ach ----------------------------------------------------------------------

(defn list-achs
  [& {:keys [paging filters]}]
  (let [query (merge nil
                     (when paging {:page paging})
                     (when filters {:filter filters}))]
    (http :get "/v1/ach" :query query)))

(defn get-ach
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/ach/" id)))

(defn create-ach
  [data]
  (http :post "/v1/ach" :data data))

(defn cancel-ach
  [id]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/ach/" id) :data {:status "canceled"}))

;; - book ---------------------------------------------------------------------

(defn list-books
  [& {:keys [paging filters]}]
  (let [query (merge nil
                     (when paging {:page paging})
                     (when filters {:filter filters}))]
    (http :get "/v1/book" :query query)))

(defn get-book
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/book/" id)))

(defn create-book
  [data]
  (http :post "/v1/book" :data data))

(defn cancel-book
  [id]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/book/" id) :data {:status "canceled"}))

;; - wire ---------------------------------------------------------------------

(defn list-wires
  [& {:keys [paging filters]}]
  (let [query (merge nil
                     (when paging {:page paging})
                     (when filters {:filter filters}))]
    (http :get "/v1/wire" :query query)))

(defn get-wire
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/wire/" id)))

(defn create-wire
  [data]
  (http :post "/v1/wire" :data data))

(defn cancel-wire
  [id]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/wire/" id) :data {:status "canceled"}))

;; - virtual-cards ------------------------------------------------------------

(defn list-vncs
  [& {:keys [paging filters]}]
  (let [query (merge nil
                     (when paging {:page paging})
                     (when filters {:filter filters}))]
    (http :get "/v1/virtualcards" :query query)))

(defn get-vnc
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/virtualcards/" id)))

(defn create-vnc
  [data]
  (http :post "/v1/virtualcards" :data data))

(defn update-vnc
  [id data]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/virtualcards/" id) :data data))

(defn delete-vnc
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/virtualcards/" id)))

(defn list-vnc-realcards
  [& {:keys [paging filters]}]
  (let [query (merge nil
                     (when paging {:page paging})
                     (when filters {:filter filters}))]
    (http :get "/v1/realcards" :query query)))

(defn list-vnc-suppliers
  [& {:keys [paging filters]}]
  (let [query (merge nil
                     (when paging {:page paging})
                     (when filters {:filter filters}))]
    (http :get "/v1/suppliers" :query query)))

;; - onboarding:addresses -----------------------------------------------------

(defn list-addressess
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/addresses" :query query)))

(defn get-address
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/addresses/" id)))

(defn create-address
  [data]
  (http :post "/v1/addresses" :data data))

(defn update-address
  [id data]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/addresses/" id) :data data))

(defn delete-address
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/addresses/" id)))

;; - onboarding:companies -----------------------------------------------------

(defn list-companies
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/companies" :query query)))

(defn get-company
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/companies/" id)))

(defn create-company
  [data]
  (http :post "/v1/companies" :data data))

(defn update-company
  [id data]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/companies/" id) :data data))

(defn delete-company
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/companies/" id)))

;; - onboarding:documents -----------------------------------------------------

(defn list-documents
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/documents" :query query)))

(defn get-document
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/documents/" id)))

(defn create-document
  [data]
  (http :post "/v1/documents" :data data))

(defn delete-document
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/documents/" id)))

;; - onboarding:files ---------------------------------------------------------

(defn list-files
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/files" :query query)))

(defn get-file
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/files/" id)))

(defn create-file
  [data]
  (http-file "/v1/files" :data data))

(defn delete-file
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/files/" id)))

;; - onboarding:gov-idents ----------------------------------------------------

(defn list-gov-idents
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/gov_idents" :query query)))

(defn get-gov-ident
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/gov_idents/" id)))

(defn create-gov-ident
  [data]
  (http :post "/v1/gov_idents" :data data))

(defn update-gov-ident
  [id data]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/gov_idents/" id) :data data))

(defn delete-gov-ident
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/gov_idents/" id)))

;; - onboarding:parent_companies ----------------------------------------------

(defn list-parent-companies
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/parent_companies" :query query)))

(defn get-parent-company
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/parent_companies/" id)))

(defn create-parent-company
  [data]
  (http :post "/v1/parent_companies" :data data))

(defn update-parent-company
  [id data]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/parent_companies/" id) :data data))

(defn delete-parent-company
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/parent_companies/" id)))

;; - onboarding:persons -------------------------------------------------------

(defn list-persons
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/persons" :query query)))

(defn get-person
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/persons/" id)))

(defn create-person
  [data]
  (http :post "/v1/persons" :data data))

(defn update-person
  [id data]
  {:pre [(not-nil? id)]}
  (http :patch (str "/v1/persons/" id) :data data))

(defn delete-person
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/persons/" id)))

;; - onboarding:logins --------------------------------------------------------

(defn list-logins
  [& {:keys [paging]}]
  (let [query (merge nil
                     (when paging {:page paging}))]
    (http :get "/v1/logins" :query query)))

(defn get-login
  [id]
  {:pre [(not-nil? id)]}
  (http :get (str "/v1/logins/" id)))

(defn create-login
  [data]
  (http :post "/v1/logins" :data data))

(defn delete-login
  [id]
  {:pre [(not-nil? id)]}
  (http :delete (str "/v1/logins/" id)))
