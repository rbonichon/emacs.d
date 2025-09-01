(require 'transient)
(require 'vol)
(require 'light)
(require 'autorandr)

;; I am not sure all the needed libraries are actually correctly loaded.
;; They should be -- obviously.
(transient-define-prefix rb/utils ()
  "Transient for different utility targets"
  ["Volume"
   ("v+" "Increase"
    (lambda () (interactive) (vol-inc 10))
    )
   ("v-" "Increase"
    (lambda () (interactive) (vol-dec 10))
    )
   ]
  ["Light"
   ("l+" "Increase" (lambda () (interactive) (message "l")))
   ("l-" "Decrease" (lambda () (interactive) (message "l--")))
   ]
  ["Screens"
   ;; Update the configuration name as neeeded
   ("o" "Office" (lambda () (interactive) (autorandr "office")))
   ("s" "Laptop" (lambda () (interactive) (autorandr "laptop")))
   ]
  )


