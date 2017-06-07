;;-----------------------------------------------------------------------------
;; SVB Api SDK Test Suite
;;-----------------------------------------------------------------------------

(ns svb-api.core-test
  (:require [clojure.test :refer :all]
            [clj-time.core :as t]
            [svb-api.core :refer :all]))

;; - utils --------------------------------------------------------------------

(def tomorrow-str
  (str (t/plus (t/today) (t/days 1))))

(defn get-env [name default]
  (or (System/getenv name) default))

(defn small-uuid []
  (last (clojure.string/split (str (java.util.UUID/randomUUID)) #"-")))

;; - setup --------------------------------------------------------------------

(def api-test-key
  (get-env "SVB_API_TEST_KEY" nil))

(def api-test-secret
  (get-env "SVB_API_TEST_SECRET" nil))

(defn setup-fixture [f]
  (set-api-key api-test-key)
  (set-secret api-test-secret)
  (f))

(use-fixtures :once setup-fixture)

;; - root ---------------------------------------------------------------------

(deftest index-test
  (testing "index"
    (is (= "1" (:api_version @(index))))))

;; - accounts -----------------------------------------------------------------

(deftest accounts-test
  (let [accounts @(list-accounts)
        account-id (or (:id (first (:data accounts))) 0)]
    (testing "accounts"
      (testing "list"
        (is (is-ok? accounts)))
      (testing "get"
        (is (is-ok? @(get-account account-id)))))
    (testing "transations"
      (testing "list"
        (is (is-ok? @(list-account-transactions account-id)))))))

;; - ach ----------------------------------------------------------------------

(def ach-test-account-number
  (get-env "SVB_API_ACH_TEST_ACCOUNT" nil))

(def ach-params
  {:account_number ach-test-account-number
   :amount 55555
   :batch_id nil
   :currency "usd"
   :direction nil
   :effective_date tomorrow-str
   :metadata {:foo "bar"}
   :receiver_account_number "87654321"
   :receiver_account_type "savings"
   :receiver_name "Harry S Truman"
   :receiver_routing_number "122487022"
   :sec_code "ppd"
   :service "same-day"})

(deftest ach-test
  (let [credit-ach-id (atom nil)
        debit-ach-id (atom nil)]
    (testing "ach"
      (testing "create"
        (testing "debit"
          (let [debit-params (assoc ach-params :direction "debit")
                debit-ach @(create-ach debit-params)]
            (when debit-ach
              (reset! debit-ach-id (-> debit-ach :data :id)))
            (is (is-ok? debit-ach))))
        (testing "credit"
          (let [credit-params (assoc ach-params :direction "credit")
                credit-ach @(create-ach credit-params)]
            (when credit-ach
              (reset! credit-ach-id (-> credit-ach :data :id)))
            (is (is-ok? credit-ach)))))
      (testing "list"
        (testing "default"
          (is (is-ok? @(list-achs))))
        (testing "paging"
          (is (is-ok? @(list-achs :paging {:limit 2}))))
        (testing "filtering"
          (is (is-ok? @(list-achs :filters {:status "pending"
                                            :effective_date tomorrow-str})))))
      (testing "get"
        (let [achs @(list-achs)
              ach-id (-> achs :data first :id)]
          (is (is-ok? @(get-ach (or ach-id 0))))))
      (testing "cancel"
        (when @debit-ach-id
          (is (is-ok? @(cancel-ach @debit-ach-id))))
        (when @credit-ach-id
          (is (is-ok? @(cancel-ach @credit-ach-id))))))))

;; - book ---------------------------------------------------------------------

(def book-params
  {:account_number "12345678"
   :amount 12345
   :currency "usd"
   :direction "credit"
   :effective_date tomorrow-str
   :metadata {:foo "keen"}
   :receiver_account_number "12345678"
   :receiver_account_type "checking"
   :receiver_name "Mike Molson"})

(deftest book-test
  (let [new-book-id (atom nil)]
    (testing "book"
      (testing "create"
        (let [new-book @(create-book book-params)]
          (when new-book
            (reset! new-book-id (-> new-book :data :id)))
          (is (is-ok? new-book))))
      (testing "list"
        (testing "default"
          (is (is-ok? @(list-books))))
        (testing "paging"
          (is (is-ok? @(list-books :paging {:limit 2}))))
        (testing "filtering"
          (is (is-ok? @(list-books :filters {:status "pending"
                                             :effective_start tomorrow-str
                                             :effective_end tomorrow-str})))))
      (testing "get"
        (let [books @(list-books)
              book-id (-> books :data first :id)]
          (is (is-ok? @(get-book (or book-id 0))))))
      (testing "cancel"
        (when @new-book-id
          (is (is-ok? @(cancel-book @new-book-id))))))))


;; - book ---------------------------------------------------------------------

(def book-params
  {:account_number "12345678"
   :amount 12345
   :currency "usd"
   :direction "credit"
   :metadata {:foo "keen"}
   :receiver_account_number "12345678"
   :receiver_account_type "checking"
   :receiver_name "Mike Molson"})

(deftest book-test
  (let [new-book-id (atom nil)]
    (testing "book"
      (testing "create"
        (let [new-book @(create-book book-params)]
          (when new-book
            (reset! new-book-id (-> new-book :data :id)))
          (is (is-ok? new-book))))
      (testing "list"
        (testing "default"
          (is (is-ok? @(list-books))))
        (testing "paging"
          (is (is-ok? @(list-books :paging {:limit 2})))))
      (testing "get"
        (let [books @(list-books)
              book-id (-> books :data first :id)]
          (is (is-ok? @(get-book (or book-id 0))))))
      (testing "cancel"
        (when @new-book-id
          (is (is-ok? @(cancel-book @new-book-id))))))))

;; - wire ---------------------------------------------------------------------

(def wire-params
  {:account_number "12345678"
   :amount 12345
   :currency "usd"
   :effective_date tomorrow-str
   :metadata {:foo "keen"}
   :priority "high"
   :receiver_account_number "12345678"
   :receiver_routing_number "122487022"})

(deftest wire-test
  (let [new-wire-id (atom nil)]
    (testing "wire"
      (testing "create"
        (let [new-wire @(create-wire wire-params)]
          (when new-wire
            (reset! new-wire-id (-> new-wire :data :id)))
          (is (is-ok? new-wire))))
      (testing "list"
        (testing "default"
          (is (is-ok? @(list-wires))))
        (testing "paging"
          (is (is-ok? @(list-wires :paging {:limit 2}))))
        (testing "filtering"
          (is (is-ok? @(list-wires :filters {:status "pending"
                                             :effective_start tomorrow-str
                                             :effective_end tomorrow-str})))))
      (testing "get"
        (let [wires @(list-wires)
              wire-id (-> wires :data first :id)]
          (is (is-ok? @(get-wire (or wire-id 0))))))
      (testing "cancel"
        (when @new-wire-id
          (is (is-ok? @(cancel-wire @new-wire-id))))))))

;; - virtual-cards ------------------------------------------------------------

(def vnc-params
  {:currency "USD"
   :email_description "The description"
   :email_vendor "auth@sheraton.com"
   :metadata {:foo "keen"}
   :per_transaction_min 0
   :total_card_amount 123445
   :transactions_max 1
   :valid_ending_on tomorrow-str})

(deftest vnc-test
  (let [new-vnc-id (atom nil)]
    (testing "virtual card"
      (testing "create"
        (let [new-vnc @(create-vnc vnc-params)]
          (when new-vnc
            (reset! new-vnc-id (-> new-vnc :data :id)))
          (is (is-ok? new-vnc))))
      (testing "list"
        (testing "default"
          (is (is-ok? @(list-vncs))))
        (testing "paging"
          (is (is-ok? @(list-vncs :paging {:limit 2}))))
        (testing "filtering"
          (is (is-ok? @(list-vncs :filters {:metadata.foo "keen"})))))
      (testing "get"
        (let [vncs @(list-vncs)
              vnc-id (-> vncs :data first :id)]
          (is (is-ok? @(get-vnc (or vnc-id 0))))))
      (testing "update"
        (when @new-vnc-id
          (is (is-ok? @(update-vnc @new-vnc-id {:metadata {:foo "keen" :id 111}
                                                :per_transaction_min 10
                                                :per_transaction_max 100
                                                :transactions_max 5
                                                :total_card_amount 1234
                                                :valid_ending_on tomorrow-str})))))
      (testing "delete"
        (when @new-vnc-id
          (is (nil? @(delete-vnc @new-vnc-id)))))))
  (testing "real card"
    (is (is-ok? @(list-vnc-realcards))))
  (testing "suppliers"
    (is (is-ok? @(list-vnc-suppliers)))))

;; - onboarding:addresses -----------------------------------------------------

(def ob-address-params
  {:street_line1 "1 Hacker Way"
   :street_line2 "Building 10"
   :street_line3 "Desk 123"
   :city "Menlo Park"
   :state "CA"
   :postal_code "94025"
   :country "US"})

(deftest onboarding-address-test
  (let [new-address-id (atom nil)]
    (testing "onboarding address"
      (testing "create"
        (let [new-address @(create-address ob-address-params)]
          (when new-address
            (reset! new-address-id (-> new-address :data :id)))
          (is (is-ok? new-address))))
      (testing "list"
        (testing "default"
          (is (is-ok? @(list-addressess))))
        (testing "paging"
          (is (is-ok? @(list-addressess :paging {:limit 2})))))
      (testing "get"
        (let [addresss @(list-addressess)
              address-id (-> addresss :data first :id)]
          (is (is-ok? @(get-address (or address-id 0))))))
      (testing "update"
        (when @new-address-id
          (is (is-ok? @(update-address @new-address-id {:street_line1 "123 Hacker Way"
                                                        :street_line2 "Building 5"
                                                        :street_line3 "Desk 321"
                                                        :city "Santa Park"
                                                        :state "TX"
                                                        :postal_code "80503"
                                                        :country "US"})))))
      (testing "delete"
        (when @new-address-id
          (is (nil? @(delete-address @new-address-id))))))))

(defmacro with-ob-address
  [[symbol-name address-params] & body]
  `(let [~symbol-name (-> @(create-address ~address-params) :data :id)]
     (do ~@body)
     (when-not (nil? ~symbol-name)
       @(delete-address ~symbol-name))))

;; - onboarding:parent_companies ----------------------------------------------

(def ob-parent-company-params
  {:address_id nil
   :country "CA"
   :description "Artisinal hockey equipment, eh."
   :name "Medicine Hat Hockey Co"
   :metadata {:foo "keen"}
   :percent_ownership 100
   :source_of_funds "personal"})

(deftest onboarding-parent-company-test
  (with-ob-address [address-id ob-address-params]
    (let [new-parent-company-id (atom nil)]
      (testing "onboarding parent-company"
        (testing "create"
          (let [params (assoc ob-parent-company-params :address_id address-id)
                new-parent-company @(create-parent-company params)]
            (when new-parent-company
              (reset! new-parent-company-id (-> new-parent-company :data :id)))
            (is (is-ok? new-parent-company))))
        (testing "list"
          (testing  "default"
            (is (is-ok? @(list-parent-companies))))
          (testing  "paging"
            (is (is-ok? @(list-parent-companies :paging {:limit 2})))))
        (testing "get"
          (let [parent-companys @(list-parent-companies)
                parent-company-id (-> parent-companys :data first :id)]
            (is (is-ok? @(get-parent-company (or parent-company-id 0))))))
        (testing "update"
          (when @new-parent-company-id
            (is (is-ok? @(update-parent-company
                           @new-parent-company-id {:address_id address-id
                                                   :country "US"
                                                   :description "Hockey equipment."
                                                   :name "Medicine Hockey Co"
                                                   :metadata {:foo "keen" :id 1}
                                                   :percent_ownership 50
                                                   :source_of_funds "family"})))))
        (testing "delete"
          (when @new-parent-company-id
            (is (nil? @(delete-parent-company @new-parent-company-id)))))))))

(defmacro with-ob-parent-company
  [[symbol-name parent-company-params address-params] & body]
  `(with-ob-address [address-id# ~address-params]
     (let [params# (assoc ~parent-company-params :address_id address-id#)
           ~symbol-name (-> @(create-parent-company params#) :data :id)]
      (do ~@body)
      (when-not (nil? ~symbol-name)
        @(delete-parent-company ~symbol-name)))))

;; - onboarding:logins --------------------------------------------------------

;
; Note: Attempt to generate a unique login using a small uuid, since logins
;       are required to be unique even after they are deleted, the test may
;       potentially fail.
;

(defn random-login-name [prefix]
  (str prefix (small-uuid)))

(def ob-login-params
  {:login_name nil})

(deftest onboarding-login-test
  (let [new-login-id (atom nil)]
    (testing "onboarding login"
      (testing "create"
        (let [params (assoc ob-login-params :login_name (random-login-name "test"))
              new-login @(create-login params)]
          (when new-login
            (reset! new-login-id (-> new-login :data :id)))
          (is (is-ok? new-login))))
      (testing "list"
        (testing  "default"
          (is (is-ok? @(list-logins))))
        (testing  "paging"
          (is (is-ok? @(list-logins :paging {:limit 2})))))
      (testing "get"
        (let [logins @(list-logins)
              login-id (-> logins :data first :id)]
          (is (is-ok? @(get-login (or login-id 0))))))
      (testing "delete"
        (when @new-login-id
          (is (nil? @(delete-login @new-login-id))))))))

(defmacro with-ob-login
  [[symbol-name] & body]
  `(let [params# {:login_name (random-login-name "test")}
         ~symbol-name (-> @(create-login params#) :data :id)]
     (do ~@body)
     (when-not (nil? ~symbol-name)
       @(delete-login ~symbol-name))))

;; - onboarding:files ---------------------------------------------------------

(def ob-file-params
  {:content "abcd"
   :filename "test.txt"
   :mime_type "image/png"
   :metadata {:foo "keen"}})

(deftest onboarding-files-test
  (let [new-file-id (atom nil)]
    (testing "onboarding files"
      (testing "create"
        (let [new-file @(create-file ob-file-params)]
          (when new-file
            (reset! new-file-id (-> new-file :data :id)))
          (is (is-ok? new-file))))
      (testing "list"
        (testing  "default"
          (is (is-ok? @(list-files))))
        (testing  "paging"
          (is (is-ok? @(list-files :paging {:limit 2})))))
      (testing "get"
        (let [files @(list-files)
              file-id (-> files :data first :id)]
          (is (is-ok? @(get-file (or file-id 0))))))
      (testing "delete"
        (when @new-file-id
          (is (nil? @(delete-file @new-file-id))))))))

(defmacro with-ob-file
  [[symbol-name file-params] & body]
  `(let [~symbol-name (-> @(create-file ~file-params) :data :id)]
     (do ~@body)
     (when-not (nil? ~symbol-name)
       @(delete-file ~symbol-name))))

;; - onboarding:documents -----------------------------------------------------

(def ob-document-params
  {:document_type "ein"
   :file_id nil
   :metadata {:foo "keen"}
   :number "12-3456789"})

(deftest onboarding-documents-test
  (with-ob-file [file-id ob-file-params]
    (let [new-document-id (atom nil)]
      (testing "onboarding documents"
        (testing "create"
          (let [params (assoc ob-document-params :file_id file-id)
                new-document @(create-document params)]
            (when new-document
              (reset! new-document-id (-> new-document :data :id)))
            (is (is-ok? new-document))))
        (testing "list"
          (testing  "default"
            (is (is-ok? @(list-documents))))
          (testing  "paging"
            (is (is-ok? @(list-documents :paging {:limit 2})))))
        (testing "get"
          (let [documents @(list-documents)
                document-id (-> documents :data first :id)]
            (is (is-ok? @(get-document (or document-id 0))))))
        (testing "delete"
          (when @new-document-id
            (is (nil? @(delete-document @new-document-id)))))))))

(defmacro with-ob-document
  [[symbol-name document-params file-params] & body]
  `(with-ob-file [file-id# ~file-params]
     (let [params# (assoc ~document-params :file_id file-id#)
           ~symbol-name (-> @(create-document params#) :data :id)]
      (do ~@body)
      (when-not (nil? ~symbol-name)
        @(delete-document ~symbol-name)))))

;; - onboarding:gov-idents ----------------------------------------------------

(def ob-gov-ident-params
  {:country "GB"
   :document_type "passport"
   :expires_on "2026-01-31"
   :file_id nil
   :first_name "Simon"
   :last_name "Johnston"
   :middle_name "Biscuit"
   :number "GB00012345"})

(deftest onboarding-gov-idents-test
  (with-ob-file [file-id ob-file-params]
    (let [new-gov-ident-id (atom nil)]
      (testing "onboarding gov-idents"
        (testing "create"
          (let [params (assoc ob-gov-ident-params :file_id file-id)
                new-gov-ident @(create-gov-ident params)]
            (when new-gov-ident
              (reset! new-gov-ident-id (-> new-gov-ident :data :id)))
            (is (is-ok? new-gov-ident))))
        (testing "list"
          (testing  "default"
            (is (is-ok? @(list-gov-idents))))
          (testing  "paging"
            (is (is-ok? @(list-gov-idents :paging {:limit 2})))))
        (testing "get"
          (let [gov-idents @(list-gov-idents)
                gov-ident-id (-> gov-idents :data first :id)]
            (is (is-ok? @(get-gov-ident (or gov-ident-id 0))))))
        (testing "update"
          (when @new-gov-ident-id
            (is (is-ok? @(update-gov-ident
                           @new-gov-ident-id {:country "US"
                                              :document_type "passport"
                                              :expires_on "2026-10-31"
                                              :file_id file-id
                                              :first_name "Paul"
                                              :last_name "Joe"
                                              :middle_name "Bob"
                                              :number "452342341"})))))
        (testing "delete"
          (when @new-gov-ident-id
            (is (nil? @(delete-gov-ident @new-gov-ident-id)))))))))

(defmacro with-ob-gov-ident
  [[symbol-name gov-ident-params file-params] & body]
  `(with-ob-file [file-id# ~file-params]
     (let [params# (assoc ~gov-ident-params :file_id file-id#)
           ~symbol-name (-> @(create-gov-ident params#) :data :id)]
      (do ~@body)
      (when-not (nil? ~symbol-name)
        @(delete-gov-ident ~symbol-name)))))

;; - onboarding:persons -------------------------------------------------------

(def ob-person-params
  {:first_name "Dwayne"
   :middle_name "Carl"
   :last_name "Grantham"
   :title "Founder and CEO"
   :date_of_birth "1981-01-31"
   :email_address "lord.grantham@example.com"
   :phone_number "+44 123 4567"
   :address_id nil
   :gov_ident_ids []
   :login_id nil
   :percent_ownership 99
   :risk_commentary "An upright citizen."
   :roles ["bank_account_administrator"
           "beneficial_owner"
           "primary_representative"]
   :ssn "123456789"
   :metadata {:foo "keen"}})

(deftest onboarding-persons-test
  (with-ob-address [address-id ob-address-params]
    (with-ob-gov-ident [gov-ident-id ob-gov-ident-params ob-file-params]
      (with-ob-login [login-id ob-login-params]
        (let [new-person-id (atom nil)]
          (testing "onboarding persons"
            (testing "create"
              (let [params (merge ob-person-params {:address_id address-id
                                                    :gov_ident_ids [gov-ident-id]
                                                    :login_id login-id})
                    new-person @(create-person params)]
                (when new-person
                  (reset! new-person-id (-> new-person :data :id)))
                (is (is-ok? new-person))))
            (testing "list"
              (testing  "default"
                (is (is-ok? @(list-persons))))
              (testing  "paging"
                (is (is-ok? @(list-persons :paging {:limit 2})))))
            (testing "get"
              (let [persons @(list-persons)
                    person-id (-> persons :data first :id)]
                (is (is-ok? @(get-person (or person-id 0))))))
            (testing "update"
              (when @new-person-id
                (is (is-ok? @(update-person
                               @new-person-id {:first_name "Torben"
                                               :middle_name "Carl"
                                               :last_name "Friis"
                                               :date_of_birth "1982-01-31"
                                               :title "Producer"
                                               :percent_ownership 49
                                               :email_address "torben.friis@example.com"
                                               :phone_number "+45 123 4567"
                                               :address_id address-id
                                               :gov_ident_ids [gov-ident-id]
                                               :risk_commentary "A citizen."
                                               :metadata {:foo "keen" :id 8}})))))
            (testing "delete"
              (when @new-person-id
                (is (nil? @(delete-person @new-person-id)))))))))))

(defmacro with-ob-person
  [[symbol-name person-params address-params gov-ident-params file-params login-params] & body]
  `(with-ob-address [address-id# ~address-params]
     (with-ob-gov-ident [gov-ident-id# ~gov-ident-params ~file-params]
       (with-ob-login [login-id# ~login-params]
         (let [params# (merge ~person-params {:address_id address-id#
                                              :gov_ident_ids [gov-ident-id#]
                                              :login_id login-id#})
               ~symbol-name (-> @(create-person params#) :data :id)]
          (do ~@body)
          (when-not (nil? ~symbol-name)
            @(delete-person ~symbol-name)))))))

;; - onboarding:persons -------------------------------------------------------

(def ob-company-params
  {:description "Foo Co is the Food Company"
   :document_ids []
   :email_address "founders@foo.com"
   :incorporation_address_id nil
   :mailing_address_id nil
   :mcc 9211
   :metadata {:foo "keen"}
   :name "Foo Co"
   :options {}
   :parent_company_id nil
   :person_ids []
   :phone_number "415-123-4567"
   :physical_address_id nil
   :product_description "Foo Green is made out of people."
   :products ["dda_account" "fx" "web" "wires"]
   :referral_source "Hacker News"
   :risk_commentary "This company is very trustworthy."
   :source_of_funds "investor"
   :state "DE"
   :website_url "https://www.foo.com/"})

(deftest onboarding-companies-test
  (with-ob-address [address-id ob-address-params]
    (with-ob-document [document-id ob-document-params
                                   ob-file-params]
      (with-ob-person [person-id ob-person-params
                                 ob-address-params
                                 ob-gov-ident-params
                                 ob-file-params
                                 ob-login-params]
        (with-ob-parent-company [parent-company-id ob-parent-company-params
                                                   ob-address-params]
          (let [new-company-id (atom nil)]
            (testing "onboarding companies"
              (testing "create"
                (let [params (merge ob-company-params
                                    {:document_ids [document-id]
                                     :incorporation_address_id address-id
                                     :mailing_address_id address-id
                                     :parent_company_id parent-company-id
                                     :person_ids [person-id]
                                     :physical_address_id address-id})
                      new-company @(create-company params)]
                  (when new-company
                    (reset! new-company-id (-> new-company :data :id)))
                  (is (is-ok? new-company))))
              (testing "list"
                (testing  "default"
                  (is (is-ok? @(list-companies))))
                (testing  "paging"
                  (is (is-ok? @(list-companies :paging {:limit 2})))))
              (testing "get"
                (let [companies @(list-companies)
                      company-id (-> companies :data first :id)]
                  (is (is-ok? @(get-company (or company-id 0))))))
              (testing "update"
                (when @new-company-id
                  (is (is-ok? @(update-company
                                 @new-company-id
                                 {:description "A pivoting to photo sharing."
                                  :email_address "photos@widgetcorp.example.com"
                                  :incorporation_address_id address-id
                                  :mailing_address_id address-id
                                  :mcc 1337
                                  :metadata {:foo "keen" :id 2}
                                  :name "Widget Corp 2"
                                  :options {}
                                  :parent_company_id parent-company-id
                                  :person_ids [person-id]
                                  :phone_number "(415) 333-1234"
                                  :physical_address_id address-id
                                  :product_description "A photo widget."
                                  :products ["dda_account" "fx" "web" "wires"]
                                  :referral_source "Hacker News"
                                  :risk_commentary "This company is very trustworthy."
                                  :source_of_funds "inheritance"
                                  :state "NV"
                                  :website_url "https://www.example.com/photos"})))))
              (testing "delete"
                (when @new-company-id
                  (is (nil? @(delete-company @new-company-id))))))))))))
