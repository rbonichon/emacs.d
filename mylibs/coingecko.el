(require 'plz)

(defvar coingecko-api-base-uri "https://api.coingecko.com/api/v3")

(defun pp-list (l)
  (s-join "%2C" l))

(defun coingecko-make-uri (uri-postfix &rest args)
  "docstring"
  (eval
   (append
    '(format
      (concat
       coingecko-api-base-uri
       "/"
       uri-postfix))
    args)))

(defun coingecko-get-supported-vs-currencies ()
  "docstring"
  (plz 'get
      (coingecko-make-uri "simple/supported_vs_currencies")
    :as #'json-read))

(defun coingecko-get-simple-price (ids vs_currencies)
  "Get the price of cryptocurrencies IDS wrt to fiat currencies VS_CURRENCIES.

See coingecko-get-supported-vs-currencies for available VS_CURRENCIES. These are
essentially fiat currencies and the more established cryptocurrencies.
"
  (interactive)
  (plz
    'get
    (coingecko-make-uri
     "simple/price?ids=%s&vs_currencies=%s"
     (pp-list ids)
     (pp-list vs_currencies))
    :as #'json-read))

(defun coingecko-coins-list ()
  "Get list of all supported coin names from coingecko"
  (plz
    'get
    (coingecko-make-uri "coins/list")
    :as #'json-read))

(defun coingecko-get-price-usd (id)
  "Get USD price for cryptocurrency ID."
  (interactive)
  (cdadar
   (coingecko-get-simple-price
    (list id)
    (list "usd"))))

(defun coingecko-get-price-eur (id)
  "Get EUR price for cryptocurrency ID."
  (interactive)
  (cdadar
   (coingecko-get-simple-price
    (list id)
    (list "eur"))))

(defun coingecko-get-mina-price ()
  "Get the price of mina token from Coingecko"
  (coingecko-get-price-usd "mina-protocol"))


;; (get-mina-price)                        

(provide 'coingecko)
