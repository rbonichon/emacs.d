
(defvar invoice-templates-directory "~/loopless/invoicing")

(defvar invoice-emission-directory "~/loopless/invoices/")

(defvar invoice-pdf-format "invoice_%d.pdf")

(defun invoice--pdf-name (n)
  "docstring"
  (expand-file-name
   (format invoice-pdf-format n)
   invoice-emission-directory))


(defun invoice--count-invoices ()
  "Count the number of invoices in INVOICE-EMISSION-DIRECTORY."
  (interactive "P")
  (length
   (directory-files
    invoice-emission-directory
    nil
    directory-files-no-dot-files-regexp
    t)))

(defun invoice--genr-number ()
  "docstring"
  (interactive)
  (let ((file-count 
         (invoice--count-invoices)))
    (with-temp-file
        (expand-file-name
         "invoice_number.tex"
         invoice-templates-directory)
      (insert
       (format "%d" file-count)))))

(defun invoice--genr-date ()
  "docstring"
  (interactive)
  (let ((today (format-time-string "%F")))
    (with-temp-file
        (expand-file-name
         "invoice_date.tex"
         invoice-templates-directory)
      (insert today))))


(defun invoice-genr-pdf ()
  "docstring"
  (interactive)
  (let* ( (currency 'USD)
          (ecb-rate (money--get-ecb-rate currency)))
    (let ((default-directory invoice-templates-directory)
          (cmd (format
                "pdflatex invoice.tex && cp invoice.pdf %s"
                (invoice--pdf-name (invoice--count-invoices))
                )))
      ;; Create contents
      (invoice--genr-date)
      (invoice--genr-number)
      (invoice--pp-latex-table default-lines currency ecb-rate)
      ;; Compile contents
      (shell-command cmd)
      ;; Show directory where the new PDF has been created
      (dired
       invoice-emission-directory))))

(setq default-lines
      '((:descr "Monthly retainer fee" :unit-price 17500)
        (:descr "Wellness" :unit-price 150)
        (:descr "Internet stipend" :unit-price 30)))

(defun invoice--pp-latex-table (invoice-lines currency &rest ecb-rate)
  "docstring"
      (with-temp-file
        (expand-file-name "invoice_contents.tex" invoice-templates-directory)
      (let ((total 0))
        (insert
         (format
          "
\\begin{tabular}{p{.3\\textwidth}rrr}
\\toprule
\\textbf{Description} & \\textbf{Quantity} & \\textbf{Unit Price (%s)} & \\textbf{Amount (%s)}\\\\[5pt]
"
          currency
          currency))
        (cl-loop for elem in invoice-lines
                 do
                 (let* ((descr (plist-get elem :descr))
                        (unit-price (plist-get elem :unit-price))
                        (qty (or (plist-get elem :qty) 1))
                        (price (* qty unit-price)))
                   (insert
                    (format
                     "%s & %d & %.2f & %.2f \\\\"
                     descr
                     qty
                     unit-price
                     price))
                   (newline)
                   (setq total (+ total price)))
                 finally
                 (insert "\\midrule")
                 (newline)
                 (insert
                  (format
                   " & & \\textbf{%s} & %.2f \\\\"
                   currency
                   total)
                  "\n"
                  "\\\\\\\\"
                  ;; Only put ECB rate when the invoice is not already in EURO
                  (if ecb-rate  (format "\\multicolumn{4}{r}{\\small ECB rate (%s): 1 EUR = %s USD}"
                                        (format-time-string
                                         "%Y-%m-%d" (plist-get ecb-rate :date))
                                        (plist-get ecb-rate :rate))
                    "")
                  "\n"
                  "\\end{tabular}")
                 ;; TODO this should actually depend on currency
                 )))
      )



(defun invoice-send-last-invoice ()
  (interactive)
  (let* ((last-invoice-number (invoice--count-invoices))
         (invoice-file (expand-file-name (invoice--pdf-name last-invoice-number)))
         (dst-address "accounting@o1labs.org"))
    (message (format  "invoice: sending %s to %s" invoice-file dst-address))
    )
  )


;;;  Sample XML format for ECB data
;; <?xml version="1.0" encoding="UTF-8"?>
;; <gesmes:Envelope xmlns:gesmes="http://www.gesmes.org/xml/2002-08-01" xmlns="http://www.ecb.int/vocabulary/2002-08-01/eurofxref">
;; 	<gesmes:subject>Reference rates</gesmes:subject>
;; 	<gesmes:Sender>
;; 		<gesmes:name>European Central Bank</gesmes:name>
;; 	</gesmes:Sender>
;; 	<Cube>
;; 		<Cube time='2024-02-19'>
;; 			<Cube currency='USD' rate='1.0776'/>
;; 			<Cube currency='JPY' rate='161.59'/>
;; 			<Cube currency='BGN' rate='1.9558'/>
;; 			<Cube currency='CZK' rate='25.455'/>
;; 			<Cube currency='DKK' rate='7.4547'/>
;; 			<Cube currency='GBP' rate='0.85448'/>
;; 			<Cube currency='HUF' rate='388.40'/>
;; 			<Cube currency='PLN' rate='4.3265'/>
;; 			<Cube currency='RON' rate='4.9770'/>
;; 			<Cube currency='SEK' rate='11.2370'/>
;; 			<Cube currency='CHF' rate='0.9492'/>
;; 			<Cube currency='ISK' rate='148.70'/>
;; 			<Cube currency='NOK' rate='11.3045'/>
;; 			<Cube currency='TRY' rate='33.2640'/>
;; 			<Cube currency='AUD' rate='1.6479'/>
;; 			<Cube currency='BRL' rate='5.3433'/>
;; 			<Cube currency='CAD' rate='1.4522'/>
;; 			<Cube currency='CNY' rate='7.7570'/>
;; 			<Cube currency='HKD' rate='8.4273'/>
;; 			<Cube currency='IDR' rate='16860.24'/>
;; 			<Cube currency='ILS' rate='3.9074'/>
;; 			<Cube currency='INR' rate='89.4620'/>
;; 			<Cube currency='KRW' rate='1438.87'/>
;; 			<Cube currency='MXN' rate='18.3698'/>
;; 			<Cube currency='MYR' rate='5.1585'/>
;; 			<Cube currency='NZD' rate='1.7536'/>
;; 			<Cube currency='PHP' rate='60.410'/>
;; 			<Cube currency='SGD' rate='1.4503'/>
;; 			<Cube currency='THB' rate='38.826'/>
;; 			<Cube currency='ZAR' rate='20.4310'/>
;; 		</Cube>
;; 	</Cube>
;; </gesmes:Envelope>x

(defun money--get-ecb-conversion-rates ()
  "
Construct an ALIST from European Central Bank conversion rates for the current date.

XML format as of 2024-02-19 is described in comment above.
"
  (interactive)
  (require 'dom)
  (let ((buffer (plz
                  'get
                  "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml"
                  :as 'buffer)))
    (with-current-buffer
        buffer
      (let* ((data (libxml-parse-xml-region
                    (beginning-of-buffer)
                    (end-of-buffer)))
             (cubes (dom-by-tag data 'Cube))
             (relevant-data (cdr cubes))
             (date)
             (res))
        ;; (message (format "date %s" relevant-data))
        (cl-loop
         for
         e
         in
         relevant-data
         do
         (let* ((l (cadr e))
                (curr (cdr (assoc 'currency l)))
                (rate (cdr (assoc 'rate l)))
                (time (cdr (assoc 'time l)))
                ;; usually is nil except when this flags the date
                )
           (when (and curr rate)
             (push
              `(,curr . ,(string-to-number rate))
              res))
           (when time
             (setq date (date-to-time time)))
           ;; (message (format "%s" l))
           ))
        ;; (message
        ;;  (format "%s %s" date res))
        `(:date ,date :rates ,res)))))



(defun money--get-ecb-rate (currency)
  "Get current EUR/USD exchange rate from ECB.

CURRENCY is currency we are looking for, e.g., 'USD
"
  (interactive)
  (let* ((rates (money--get-ecb-conversion-rates))
         (rate
          (cdr (assoc (format "%s" currency)
                      (plist-get rates :rates))))
         (date (plist-get rates :date)))
    `(:rate ,rate :date ,date)))


(defun money--get-ecb-history ()
  "docstring"
  (interactive "P")
  (require 'dom)
  (let ((buffer (plz
                  'get
                  "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/usd.xml"
                  :as 'buffer)))
    (with-current-buffer
        buffer
      (let* ((data (libxml-parse-xml-region
                    (beginning-of-buffer)
                    (end-of-buffer)))
             (obss (dom-by-tag data 'Obs))
             (relevant-data (cdr obss)))
        (cl-loop
         for
         e
         in
         relevant-data
         collect
         (let* ((l (cadr e))
                (date (cdr (assoc 'TIME_PERIOD l)))
                (rate (cdr (assoc 'OBS_VALUE l))))
           `(:date ,date
                   :time ,(date-to-time date)
                   :rate ,(string-to-number rate))))))))

