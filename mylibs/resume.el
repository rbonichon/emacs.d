(defun emit-work-experience (wexp)
  "docstring"
  )

(setq wexp-sections nil)

(defun wexp--read-entry ()
  "docstring"
  (let* ((e (org-element-context))
         (level (org-element-property :level e))
         )
    (pcase level
      (1 (push `(:section-name ,(org-element-property :raw-value e) :contents nil) wexp-sections))
      (2
       (let* ((sec (car wexp-sections))
              (cts (plist-get sec :contents))
              (name (org-element-property
                     :raw-value e))
              (date-from (org-element-property :FROM e))
              (date-to (org-element-property :TO e))
              (subtitle (org-element-property :DESCRIPTION e))
              (employer (org-element-property :EMPLOYER e)))
         (push
          `(:name ,name
                  :from ,date-from
                  :to ,date-to
                  :employer ,employer
                  :subtitle ,subtitle
                  :contents nil)
          cts)
         (plist-put sec :contents cts)))
      (3
       (let* ((sec (car wexp-sections))
              (cts (plist-get sec :contents))
              (elt (car cts))
              (elt_cts (plist-get elt :contents))
              (name (org-element-property :raw-value e))
              (tech_stack (when-let ((ts (org-element-property :TECHSTACK e))) (s-split " " ts)) 
                          )
              (descr (org-element-property :descr e))
              )
         (message (format "%s" (type-of tech_stack)))
         (push `(:name ,name :tech ,tech_stack :descr ,descr) elt_cts)
         (plist-put elt :contents elt_cts)
         )))))
;; ((:section-name Education :contents nil) (:section-name Work experience :contents ((:name Software Engineer :contents nil))) (:section-name Work experience :contents ((:name Software Engineer :contents nil))) (:section-name Work experience :contents ((:name Software Engineer :contents nil))))

(defun make-resume ()
  "Generate a resume from current org file"
  (interactive)
  (message "Make resume")
  (setq wexp-sections nil)
  (let ((n 0))
    (org-map-entries (lambda () (cl-incf n)) "+experience" 'file)
    (message (format "Found %d org entries" n))
    )
  (org-map-entries
   'wexp--read-entry
   "+experience"
   'file)
  (message "End resume")
  (message (format "%s" wexp-sections))
  (resume-pp-typst wexp-sections))


(defun resume-pp-typst (resume)
  "docstring"
  (let ((buf (get-buffer-create "resume.typst") ))
    (with-current-buffer buf
      (erase-buffer)
      (cl-loop
       for section in resume
       do
       (let ((secname (plist-get section :section-name)))
         (insert "== " secname)
         (newline 2)
         (cl-loop
          for item in (plist-get section :contents)
          do
          (insert (format "
#experience(
title: %S,
facility-description: [#link(%S)[%s]],
subtitle: %S,
)
"
                          (plist-get item :name)
                          (plist-get item :employer-url)
                          (plist-get item :employer)
                          (plist-get item :subtitle)
                          ))
          (newline 2)
          )
         )
       )))  )
