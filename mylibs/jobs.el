(require 'plz)
(require 'dom)

(defun tarides-careers ()
  "Get the list of available jobs from Tarides website"
  (interactive)
  (let* ((buffer (plz
                   'get
                   "https://tarides.com/careers/"
                   :as 'buffer)))
    (with-current-buffer
        buffer
      (beginning-of-buffer)
      (let* ((data (libxml-parse-html-region))
             (sections (dom-by-tag data 'section))
             ;; Jobs are in first section
             (relevant-section (car sections))
             (jobs (dom-by-tag
                    relevant-section
                    'a)))
        (cl-loop
         for
         job
         in
         jobs
         collect
         `(:link ,(dom-attr job 'href)
                 :name ,(dom-text job)))))))


;TODO: Doesn't work. The whole page is loaded through Js
(defun jobs-ashby (url)
  "Retrieve available jobs from a ashybhq generated page"
  (interactive)
  (let* ((buffer (plz
                   'get
                   url 
                   :as 'buffer)))
    (with-current-buffer
        buffer
      (beginning-of-buffer)
      (let* ((data (libxml-parse-html-region))
                                        ;TODO: Make the class a variable
             (postings (dom-by-class data "ashby-job-posting-brief-list"))
             ;; Jobs are in first section
             )

        (message (format "%s" postings))
        (cl-loop
         for posting in postings
         for job in posting 
         collect
         `(:link ,(dom-attr job 'href)
                 :name ,(dom-text job))))))

  )

(jobs-ashby "https://jobs.ashbyhq.com/irreducible")
(plz 'get "https://jobs.ashbyhq.com/irreducible")

(defun get-jobs ()
  "docstring"
  (interactive)
  (let ((buf (get-buffer-create "*job listing*"))
        (callbacks '(tarides-careers)))
    (with-current-buffer buf
      (erase-buffer)
      (beginning-of-buffer)
      (cl-loop
       for cback in callbacks
       do 
       (let ((links (funcall cback)))
         (cl-loop
          for link in links do
          (let ((name (plist-get link :name))
                (url (plist-get link :link)))
            (insert (format "%s: %s" name url))
            (newline)
            )
          )
         )
       )
      )
    )
  )

(provide 'jobs)
