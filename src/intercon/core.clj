(ns ^{:doc "DCI for Clojure"
      :author "Michiel Eggermont"}
      intercon.core
      (use :reload [intercon.rate :only [rate-fn]]))

;; To get a better understanding of what Cope means by DCI
;; I am trying to model the domain of monetary transactions
;; in the style of DCI. DCI results in an architecture that
;; separates the slowly evolving parts in your system from
;; the fast evolving parts. This is done by the introduction
;; of roles and contexts.
;; Domain models encapsulate the slowly evolving data. These
;; dumb models will be assigned a role within a context to be
;; able to execute contextual tasks. The roles therefore
;; encapsulate the ever changing business needs.

;; Of course even the dumb models should have some functionality
;; The following (dumb) function applies a function to the balance
;; of an account and returns the new account with the updated balance.
(defn modify-balance
  "Modifies the balance of the account by applying the function f to it
   and returns the updated account. If the balance was not found, then
   a new balance initialized with the amount is added to the account."
  [f account amount]
  (update-in
    account [:balance] (fnil #(f % amount) 0)))

;; Now let's define some roles that are frequently used in the domain
;; of monetary transactions. On such role is that of a money sink, in
;; which money can disappear. The other one is the money source that
;; can deliver money.
(defprotocol MoneySink
  (transfer-from
    [account amount]))
(defprotocol MoneySource
  (transfer-to
    [account amount]))
;; Then we need a dumb domain model of an account, which is not much
;; more than a container of account data and the mundane implementations
;; of account function roles of source and sink. These dumb functions are
;; NOT the complex, ever changing business functionality. These will come
;; later, when we let the dumb models play roles in a context.
(defrecord Account [id balance ccy]
  Object
  (toString
    [this]
    (str id ": " (:balance this) " " (.toUpperCase (name (:ccy this)))))
  MoneySink
  (transfer-from
    [account amount]
    (modify-balance + account amount))
  MoneySource
  (transfer-to
    [account amount]
    (modify-balance - account amount)))

;; As said, roles are played in a context. Here is a protocol that
;; can be used for the execution of a use case
(defprotocol
  Context
  (enact
    [use-case]))

;; Let's for example create a money tranfer context.
;; It can transfer amounts from one account to another using a function
;; for converting amounts to the ccy of the target.
;; For this Context we need the following roles:
;; - MoneySource: for the source of money
;; - MoneySink:   for the destination of money
;; - EchangeRate: for converting amounts between different currencies.
;; The first two were already defined. So here's the last one:
(defprotocol ExchangeRate
  (convert [source-ccy target-ccy amount]))

;; Now that we have identified the roles we can create a function
;; that will contain the business logic of transferring money
;; from one account to another one an performing conversion
;; along the way.
(defn transfer-money
  [rate-f source target s-amt]
  (reify
    ExchangeRate
    (convert
      [context target-ccy amount]
      (let [exchange-rate (rate-f (:ccy source) target-ccy)]
        (with-precision 4 (* amount exchange-rate))))
    Context
    (enact
      [this]
      (let [s-ccy (:ccy source)
            t-ccy (:ccy target)
            t-amt (convert this t-ccy s-amt)]
        {:source (transfer-to   source s-amt)
         :target (transfer-from target t-amt)}))))

;; Now we can see what happens.
;; Enact a use case in which we transfer 20 EUR to a NOK account.
(let [s-act (Account. "ACC-1" 1000.00M :eur)
      t-act (Account. "ACC-2" 1000.00M :nok)
      s-amt 203.34M
      mt-context (transfer-money rate-fn s-act t-act s-amt)
      {:keys [source target]} (enact mt-context)]
  (println
    (str "before:   " s-act ", " t-act))
  (println
    (str "transfer: " s-amt " " (:ccy s-act)))
  (println
    (str "after :   " source ", " target)))

