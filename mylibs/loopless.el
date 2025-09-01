(require 'f)

(defun loopless/admin-things-to-do ()
  "docstring"
  (interactive)
  (let ((receipts-to-deal-with (f-entries "~/loopless/justificatifs/a_faire/")))
    (when receipts-to-deal-with 
      (message (format "%d factures `a transmettre" (length receipts-to-deal-with)))
      )
    )
  )

(defun loopless/send-receipts ()
  "docstring"
  (interactive)
  (let ((dst_address "justif+loopless@tiime.fr")
        )

    (mail)
    (message-mode)
    (mail-to)
    (insert dst_address)
    (mail-subject)
    (insert "Justificatif(s)")
    (mail-text)
    (insert "Cf PJs")
    (newline 2)
    )
  )


(defun loopless/test-send-receipts ()
  (interactive)
  (let ((dst_address "richard.bonichon+loopless@gmail.com")
        (dir "~/loopless/justificatifs/a_faire/"))
    (cl-loop
     for file in (file-expand-wildcards (concat dir "*.pdf"))
     do
     (mail)
     (message-mode)
     (mail-to)
     (insert dst_address)
     (mail-subject)
     (insert "Justificatif(s)")
     (mail-text)
     (insert "Cf PJs")
     (newline 2)
     (mml-attach-file
      file
      "application/pdf"
      (file-name-base file) 
      "attachment")
     (message-send-and-exit)
     ;; Now move the file to the backup directory
     (let ((new-file
            (string-join
             (-replace-first "a_faire" "traites" (file-name-split file)) "/")
            ))
       (message (format "Moving to %s" new-file))
       (rename-file file new-file t)))))
