(ns intercon.rate)

(defn rate
  "Returns the rate from s-ccy to t-ccy.
   The rate-fn is used to find the rate from a base ccy to another ccy.
   The rate returned is the ratio between t-rate and s-rate."
  [p rate-fn s-ccy t-ccy]
  (let [s-rate (rate-fn s-ccy)
        t-rate (rate-fn t-ccy)]
   (with-precision p (/ t-rate s-rate))))

(defn ask-rate
   "Computes the ask exchange rate between source and target currnency"
   [p rates source target]
   (rate p #(:ask (% rates)) source target))
(defn bid-rate
   "Computes the bid exchange rate between source and target currnency"
   [p rates source target]
   (rate p #(:bid (% rates)) source target))
(defn mid-rate
   "Computes the mid exchange rate between source and target currnency"
   [p rates source target]
   (rate p #(reduce + (map (% rates) [:ask :bid])) source target))

;; An example of a list of exchange rates from usd to some
;; other currencies.
(def exchange-rates
   {:usd {:ask 1M      :bid 1M     }
    :eur {:ask 1.2003M :bid 1.2203M}
    :nok {:ask 3.1232M :bid 3.3032M}
    :mxd {:ask 2.4212M :bid 2.5444M}
    :cad {:ask 1.2212M :bid 1.2444M}
    :jpy {:ask 8.9183M :bid 9.0023M}})
(def ccys
  (keys exchange-rates))

;; Let's make a function that will be able to compute exchange rates
;; with a precision of 10 using mid-rates
(def rate-fn
   (partial mid-rate 10 exchange-rates))

;; Now print the mid exchange rates between all possible currency
;; combinations
(for [s ccys t ccys]
  (let [r (rate-fn s t)]
    (println (str "1 " s  " = " r " " t) )))

