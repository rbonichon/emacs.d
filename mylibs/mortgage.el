;; Mortgage Calculator Functions
;; Common utility functions and mortgage calculations

(defun mortgage-monthly-rate (annual-rate)
  "Convert annual interest rate to monthly rate.
ANNUAL-RATE should be a decimal (e.g., 0.05 for 5%)."
  (/ annual-rate 12.0))

(defun mortgage-monthly-payment (principal annual-rate months)
  "Calculate monthly payment for a mortgage.
PRINCIPAL is the loan amount.
ANNUAL-RATE is the annual interest rate as a decimal.
MONTHS is the loan duration in months."
  (let ((monthly-rate (mortgage-monthly-rate annual-rate)))
    (if (= monthly-rate 0)
        ;; If interest rate is 0, just divide principal by months
        (/ principal months)
      ;; Standard mortgage payment formula
      (let ((factor (expt (1+ monthly-rate) months)))
        (* principal
           (/ (* monthly-rate factor)
              (1- factor)))))))

(defun mortgage-remaining-balance (principal annual-rate months payment-number)
  "Calculate remaining balance after PAYMENT-NUMBER payments.
PRINCIPAL is the original loan amount.
ANNUAL-RATE is the annual interest rate as a decimal.
MONTHS is the total loan duration in months.
PAYMENT-NUMBER is the number of payments already made."
  (let ((monthly-rate (mortgage-monthly-rate annual-rate))
        (monthly-payment (mortgage-monthly-payment principal annual-rate months)))
    (if (= monthly-rate 0)
        ;; If interest rate is 0
        (* monthly-payment (- months payment-number))
      ;; Standard remaining balance formula
      (let ((factor (expt (1+ monthly-rate) months))
            (paid-factor (expt (1+ monthly-rate) payment-number)))
        (* principal
           (/ (- factor paid-factor)
              (1- factor)))))))

(defun mortgage-payment-breakdown (principal annual-rate months payment-number)
  "Calculate interest and principal portions of a specific payment.
Returns a list (interest-payment principal-payment).
PRINCIPAL is the original loan amount.
ANNUAL-RATE is the annual interest rate as a decimal.
MONTHS is the total loan duration in months.
PAYMENT-NUMBER is the payment number (1-indexed)."
  (let* ((monthly-rate (mortgage-monthly-rate annual-rate))
         (remaining-balance (if (= payment-number 1)
                               principal
                             (mortgage-remaining-balance principal annual-rate months (1- payment-number))))
         (interest-payment (* remaining-balance monthly-rate))
         (monthly-payment (mortgage-monthly-payment principal annual-rate months))
         (principal-payment (- monthly-payment interest-payment)))
    (list interest-payment principal-payment)))

(defun mortgage-payment-schedule (principal annual-rate months)
  "Generate complete payment schedule for a mortgage.
Returns a list of payment details for each month.
Each payment is a list: (month-number payment-amount interest-portion principal-portion remaining-balance)."
  (let ((schedule '())
        (monthly-payment (mortgage-monthly-payment principal annual-rate months)))
    (dotimes (month months)
      (let* ((payment-num (1+ month))
             (breakdown (mortgage-payment-breakdown principal annual-rate months payment-num))
             (interest-portion (car breakdown))
             (principal-portion (cadr breakdown))
             (remaining-balance (if (= payment-num months)
                                   0
                                 (mortgage-remaining-balance principal annual-rate months payment-num))))
        (push (list payment-num 
                    monthly-payment 
                    interest-portion 
                    principal-portion 
                    remaining-balance)
              schedule)))
    (reverse schedule)))

(defun mortgage-max-loan-amount (max-monthly-payment annual-rate months)
  "Calculate maximum loan amount given monthly payment constraint.
MAX-MONTHLY-PAYMENT is the maximum amount you can pay monthly.
ANNUAL-RATE is the annual interest rate as a decimal.
MONTHS is the desired loan duration in months."
  (let ((monthly-rate (mortgage-monthly-rate annual-rate)))
    (if (= monthly-rate 0)
        ;; If interest rate is 0
        (* max-monthly-payment months)
      ;; Solve for principal in the payment formula
      (let ((factor (expt (1+ monthly-rate) months)))
        (* max-monthly-payment
           (/ (1- factor)
              (* monthly-rate factor)))))))

(defun mortgage-display-schedule (schedule)
  "Display mortgage payment schedule in a formatted table.
SCHEDULE should be the output from mortgage-payment-schedule."
  (let ((buffer (get-buffer-create "*Mortgage Schedule*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "%-8s %-12s %-12s %-12s %-12s\n"
                      "Month" "Payment" "Interest" "Principal" "Balance"))
      (insert (make-string 60 ?-))
      (insert "\n")
      (dolist (payment schedule)
        (insert (format "%-8d €%-11.2f €%-11.2f €%-11.2f €%-11.2f\n"
                        (nth 0 payment)  ; month
                        (nth 1 payment)  ; payment
                        (nth 2 payment)  ; interest
                        (nth 3 payment)  ; principal
                        (nth 4 payment)  ; balance
                        )))
      (insert "\n")
      (let ((total-payments (length schedule))
            (total-paid (* (nth 1 (car schedule)) (length schedule)))
            (total-interest (apply '+ (mapcar (lambda (p) (nth 2 p)) schedule))))
        (insert (format "Summary:\n"))
        (insert (format "Total Payments: %d\n" total-payments))
        (insert (format "Total Amount Paid: €%.2f\n" total-paid))
        (insert (format "Total Interest Paid: €%.2f\n" total-interest))))
    (switch-to-buffer buffer)))

;; Example usage functions
(defun mortgage-example-schedule ()
  "Example: Generate payment schedule for a €200,000 loan at 4.5% for 30 years."
  (interactive)
  (let ((schedule (mortgage-payment-schedule 200000 0.045 360)))
    (mortgage-display-schedule schedule)))

(defun mortgage-example-max-loan ()
  "Example: Calculate max loan for €1,500/month payment at 4.5% for 30 years."
  (interactive)
  (let ((max-loan (mortgage-max-loan-amount 1500 0.045 360)))
    (message "Maximum loan amount: €%.2f" max-loan)))

;; Interactive functions for user input
(defun mortgage-calculate-schedule (principal annual-rate-percent months)
  "Interactive function to calculate mortgage payment schedule.
PRINCIPAL is the loan amount.
ANNUAL-RATE-PERCENT is the annual interest rate as a percentage.
MONTHS is the loan duration in months."
  (interactive "nLoan amount: \nnAnnual interest rate (%): \nnLoan duration (months): ")
  (let ((schedule (mortgage-payment-schedule principal (/ annual-rate-percent 100.0) months)))
    (mortgage-display-schedule schedule)))

(defun mortgage-calculate-max-loan (max-payment annual-rate-percent months)
  "Interactive function to calculate maximum loan amount.
MAX-PAYMENT is the maximum monthly payment.
ANNUAL-RATE-PERCENT is the annual interest rate as a percentage.
MONTHS is the desired loan duration in months."
  (interactive "nMaximum monthly payment: \nnAnnual interest rate (%): \nnLoan duration (months): ")
  (let ((max-loan (mortgage-max-loan-amount max-payment (/ annual-rate-percent 100.0) months)))
    (message "Maximum loan amount: €%.2f" max-loan)))

;; Comprehensive table generation functions
(defun mortgage-generate-payment-table (loan-amounts interest-rates durations-years)
  "Generate comprehensive payment tables for multiple scenarios in org-mode format.
LOAN-AMOUNTS is a list of loan amounts.
INTEREST-RATES is a list of interest rates (as decimals).
DURATIONS-YEARS is a list of loan durations in years."
  (let ((buffer (get-buffer-create "*Mortgage Payment Tables*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "#+TITLE: Mortgage Payment Tables\n")
      (insert "#+AUTHOR: Generated by Emacs Mortgage Calculator\n")
      (insert "#+DATE: ")
      (insert (format-time-string "%Y-%m-%d"))
      (insert "\n\n")
      
      ;; Generate a table for each duration
      (dolist (years durations-years)
        (let ((months (* years 12)))
          (insert (format "* Loan Duration: %d Years (%d Months)\n\n" years months))
          
          ;; Org-mode table header
          (insert "|")
          (insert (format " %-12s |" "Amount"))
          (dolist (rate interest-rates)
            (insert (format " %8.2f%% |" (* rate 100))))
          (insert "\n")
          
          ;; Org-mode table separator
          (insert "|")
          (insert (make-string 14 ?-))
          (insert "|")
          (dolist (rate interest-rates)
            (insert (make-string 10 ?-))
            (insert "|"))
          (insert "\n")
          
          ;; Generate rows for each loan amount
          (dolist (amount loan-amounts)
            (insert "|")
            (insert (format " €%-11.0f |" amount))
            (dolist (rate interest-rates)
              (let ((payment (mortgage-monthly-payment amount rate months)))
                (insert (format " €%7.2f |" payment))))
            (insert "\n"))
          
          (insert "\n")))
      
      ;; Add summary statistics
      (insert "* Summary Statistics")
      (newline 2)
      (mortgage-add-summary-stats loan-amounts interest-rates durations-years)
      
      ;; Activate org-mode
      (org-mode)
      (goto-char (point-min))
      ;; Align all generated tables
      (org-table-map-tables 'org-table-align)
      )
    (switch-to-buffer buffer)))

(defun mortgage-add-summary-stats (loan-amounts interest-rates durations-years)
  "Add summary statistics to the mortgage tables."
  (let ((min-amount (apply 'min loan-amounts))
        (max-amount (apply 'max loan-amounts))
        (min-rate (apply 'min interest-rates))
        (max-rate (apply 'max interest-rates))
        (min-years (apply 'min durations-years))
        (max-years (apply 'max durations-years)))
    
    (insert (format "- Loan Amount Range :: €%.0f - €%.0f\n" min-amount max-amount))
    (insert (format "- Interest Rate Range :: %.2f%% - %.2f%%\n" 
                    (* min-rate 100) (* max-rate 100)))
    (insert (format "- Duration Range :: %d - %d years" min-years max-years))
    (newline 2)
    
    ;; Calculate some interesting comparisons
    (insert "** Payment comparisons")
    (newline 2)
    
    ;; Show impact of interest rate on a €300k loan for 25 years
    (let ((base-amount 300000)
          (base-years 25)
          (base-months (* 25 12)))
      (insert (format "*** Monthly Payment for €%.0f loan over %d years" base-amount base-years))
      (newline 2)
      (insert "    | Rate | Monthly |")
      (org-table-insert-hline)
      (next-line 2)
      (dolist (rate interest-rates)
        (let ((payment (mortgage-monthly-payment base-amount rate base-months)))
          (insert (format "  | %.2f%% | %.2f |\n" (* rate 100) payment))))
      (insert "\n"))
    
    ;; Show impact of duration on a €300k loan at 4.25%
    (let ((base-amount 300000)
          (base-rate 0.0425))
      (insert (format "*** Monthly Payment for €%.0f loan at %.2f%%" base-amount (* base-rate 100)))
      (newline 2)
      (insert "   | Duration (y) | Monthly | Total | Interest |\n")
      (insert "   |--------------+---------+-------+----------|\n")
      (dolist (years durations-years)
        (let* ((months (* years 12))
               (payment (mortgage-monthly-payment base-amount base-rate months))
               (total-paid (* payment months))
               (total-interest (- total-paid base-amount)))
          (insert (format "  | %d   | %.2f | %.2f | %.2f |\n" 
                          years payment total-paid total-interest))))
      (insert "\n"))
    
    ;; Show total interest paid for different scenarios
    (insert "** Total interest examples")
    (newline 2)
    (insert "Total interest paid over loan lifetime")
    (newline)
    (let ((example-amount 250000))
      (dolist (years durations-years)
        (let ((months (* years 12)))
          (insert (format "\n**** %.0f loan for %d years" example-amount years))
          (newline 2)
          (dolist (rate interest-rates)
            (let* ((payment (mortgage-monthly-payment example-amount rate months))
                   (total-paid (* payment months))
                   (total-interest (- total-paid example-amount)))
              (insert (format "  | %.2f%% | €%.2f |\n" 
                              (* rate 100) total-interest)))))))))

(defun mortgage-generate-standard-tables ()
  "Generate standard mortgage payment tables for common scenarios.
Duration: 15, 20, 25 years
Amounts: €200k to €500k by €50k steps
Rates: 3.5% to 5% by 0.25% steps"
  (interactive)
  (let ((loan-amounts '(200000 250000 300000 350000 400000 450000 500000))
        (interest-rates '(0.035 0.0375 0.04 0.0425 0.045 0.0475 0.05))
        (durations-years '(15 20 25)))
    (mortgage-generate-payment-table loan-amounts interest-rates durations-years)))

(defun mortgage-generate-custom-tables (amounts-start amounts-end amounts-step 
                                       rates-start rates-end rates-step
                                       durations-list)
  "Generate custom mortgage payment tables with specified parameters.
AMOUNTS-START, AMOUNTS-END, AMOUNTS-STEP define the loan amount range.
RATES-START, RATES-END, RATES-STEP define the interest rate range (as percentages).
DURATIONS-LIST is a list of loan durations in years."
  (interactive "nLoan amount start: \nnLoan amount end: \nnLoan amount step: \nnInterest rate start (%): \nnInterest rate end (%): \nnInterest rate step (%): \nsLoan durations (years, comma-separated): ")
  (let ((loan-amounts '())
        (interest-rates '())
        (durations-years (mapcar 'string-to-number 
                                 (split-string durations-list "," t "[ \t]"))))

    (message "generate loan amounts")
    ;; Generate loan amounts
    (let ((amount amounts-start))
      (while (<= amount amounts-end)
        (push amount loan-amounts)
        (setq amount (+ amount amounts-step))))
    (setq loan-amounts (reverse loan-amounts))
    
    ;; Generate interest rates
    (let ((rate rates-start))
      (while (<= rate rates-end)
        (push (/ rate 100.0) interest-rates)
        (setq rate (+ rate rates-step))))
    (setq interest-rates (reverse interest-rates))
    
    (mortgage-generate-payment-table loan-amounts interest-rates durations-years)
    ))

(mortgage-generate-custom-tables 200000 500000 20000 2.9 4 0.1 "15,20,25")
