;;; control-lock.el --- Like caps-lock, but for your control key.  Give your pinky a rest!

(defun ol (txt &optional txt2)
  (write-region (concat (prin1-to-string txt) (prin1-to-string txt2) "\n") nil "/tmp/eol.txt" t))

(defun control-lock-letter (l ch)
  "Called when keys are pressed.  If we deem control-lock to be enabled, it returns the control-version of the key.  Otherwise it just returns the key."
  (if (control-lock-enabled-p)
    ch l))

(defun control-lock-enabled-p ()
  "Returns whether control lock should be enabled at a given point"
  (and control-lock-mode-p
    ; If not disable once (turning off if set)
    (if control-lock-disable-once
      (progn
        (setq control-lock-disable-once nil)
        nil  ; Not enabled this time
        )
      t  ; It's enabled as far as we know here
      )

    ; not defined and true
    (not (and (boundp 'xiki-tree-filter-options) xiki-tree-filter-options))

    (not isearch-mode)
    (not (string-match "\\*Minibuf" (buffer-name)))))

; Make ctrl-lock be off by default
(setq control-lock-mode-p nil)

(setq control-lock-disable-once nil)

(defun control-lock-map-key (l ch fun &optional shift)
  "Makes function to handle one key, and maps it to that key"
  (eval (read
    (concat
      "(progn "
        "(defun control-lock-" fun " (p) "
           "(setq control-lock-shift " (if shift "t" "nil") ")"
           "(control-lock-letter (kbd \"" l "\") (kbd \"" ch "\"))"
        ") "
        "(define-key key-translation-map (kbd \"" l "\") 'control-lock-" fun ")"
      ")"
      ))))

; Map lowercase keys
(let ((c ?a) s)
  (while (<= c ?z)
    (setq s (char-to-string c))
    (control-lock-map-key s (concat "C-" s) s)
    (setq c (+ c 1))))

; Map uppercase keys to lowercase
(let ((c ?A) s)
  (while (<= c ?Z)
    (setq s (char-to-string c))
    (control-lock-map-key s (downcase s) s t)
    (setq c (+ c 1))))

; Map numbers
(let ((c ?0) s)
  (while (<= c ?9)
    (setq s (char-to-string c))
    (control-lock-map-key s (concat "C-" s) s)
    (setq c (+ c 1))))


; Map misc keys
(control-lock-map-key "'" "C-'" "apostrophe")
(control-lock-map-key "," "C-," "comma")
(control-lock-map-key "`" "C-`" "backtick")
(control-lock-map-key "\\\\" "C-\\\\" "backslash")


;(control-lock-map-key "C-i" "C-<tab>" "tab")

(control-lock-map-key "/" "C-/" "slash")
(control-lock-map-key "SPC" "C-@" "space")
(control-lock-map-key "[" "C-[" "lsqrbracket")
(control-lock-map-key "]" "C-]" "rsqrbracket")
(control-lock-map-key ";" "C-;" "semicolon")
(control-lock-map-key "." "C-." "period")
(control-lock-map-key "=" "C-=" "equals")
(control-lock-map-key "-" "C--" "dash")


(defun control-lock-keys ()
  "Sets default keys - C-z enables control lock."
  ; (global-set-key (kbd "C-z") 'control-lock-enable)
  (global-set-key (kbd "C-,") 'control-lock-enable))

(defun control-lock-apply-bar-color ()
  (if (boundp 'control-lock-color) (progn

    ; Remember old color unless it's already orange (sometimes we're called after a theme that didn't change the bg, and control lock might have been on)
    (let ((color (face-attribute 'mode-line :background)))
      (if (not (string= control-lock-color color))
        (set 'control-lock-color-old color)
      )
    )

    (set-face-attribute 'mode-line nil :background control-lock-color)
  ))
)

(defun control-lock-enable () (interactive)
  "Enable control lock.  This function should be mapped to the key the user uses to enable control-lock."
  (if control-lock-mode-p   ; If control lock was enabled, disable it
    (progn
      (setq control-lock-mode-p nil)
      (save-window-excursion   ; Unnecessary, but forces cursor to refresh immediately
        (customize-set-variable 'cursor-type '(bar . 3)))

      (if (and (boundp 'control-lock-color-old) control-lock-color-old)
        (set-face-attribute 'mode-line nil :background control-lock-color-old)
      )
    )
    (progn   ; Else, enable it
      (setq control-lock-mode-p t)
      (save-window-excursion   ; Unnecessary, but forces cursor to refresh immediately
        (customize-set-variable 'cursor-type '(hbar . 3)))

      (control-lock-apply-bar-color)
    )))

(provide 'control-lock)
