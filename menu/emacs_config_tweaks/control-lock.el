;;; control-lock.el --- Like caps-lock, but for your control key.  Give your pinky a rest!

;; Copyright (C) 2013 Craig Muth
;; Created 10 November 2007
;; Version 1.1.3
;; Version Keywords: control key lock caps-lock

;;;; Commentary
;; Quick Start / installation:
;; 1. Download this file and put it next to other files emacs includes
;; 2. Add this to your .emacs file and restart emacs:
;;      (require 'control-lock)
;;      (control-lock-keys)
;; 3. Type C-, and follow the below steps
;;
;; Use Case:
;;   - Type C-,
;;     - The cursor changes to a an underscore, so you know control-lock is on
;;   - Type n to go to the next line
;;   - Type v to scroll down
;;   - Type xf to open a file
;;   - Type any other chars, and emacs will behave as though control is held down
;;   - Type , to exit control-lock
;;     - The cursor changes back, so you know control-lock is off
;;
;; Input from commands:
;;   When in control lock and a command gets input from the minibuffer, control-lock
;;   doesn't interfere (i.e. chars are temporarily not converted to control chars).
;;
;; Inserting literal char:
;;   Pressing ' will temporarily turn control-lock off for the following key stroke.
;;   This is useful, for example, for typing 's to sort in dired mode.
;;   Holding down shift acts to temporarily disable control-lock

(defun ol (txt &optional txt2)
  (write-region (concat (prin1-to-string txt) (prin1-to-string txt2) "\n") nil "/tmp/eol.txt" t))

(defun control-lock-letter (l ch)
  "Called when keys are pressed.  If we deem control-lock to be enabled, it returns the control-version of the key.  Otherwise it just returns the key."
  ;(ol "control-lock-enabled-p: " (control-lock-enabled-p))
  ; (pp (control-lock-enabled-p))
  (if (control-lock-enabled-p)
    ch l))

(defun control-lock-enabled-p ()
  "Returns whether control lock should be enabled at a given point"
  ; (pp "control-lock-mode-p")
  ; (pp control-lock-mode-p)
  (and control-lock-mode-p
    ; If not disable once (turning off if set)
    (if control-lock-disable-once
      (progn
        (setq control-lock-disable-once nil)
        nil  ; Not enabled this time
        )
      t  ; It's enabled as far as we know here
      )
    (not isearch-mode)
    (not (string-match "\\*Minibuf" (buffer-name)))))

; Make ctrl-lock be off by default
(setq control-lock-mode-p nil)

(defun control-lock-quote (p)
  "Make ' disable ctrl-lock for next key"
  (if (control-lock-enabled-p)
    (progn
      ;(echo "xxxxxxx")
      (setq control-lock-disable-once t)
      "")
    "\\"))
;    p))

(setq control-lock-disable-once nil)
;(define-key key-translation-map "'" 'control-lock-quote)
(define-key key-translation-map "\\" 'control-lock-quote)

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


(control-lock-map-key "C-i" "C-<tab>" "tab")

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
  (global-set-key (kbd "C-,") 'control-lock-enable)
)

(defun control-lock-enable () (interactive)
  "Enable control lock.  This function should be mapped to the key the user uses to enable control-lock."
  (if control-lock-mode-p   ; If control lock was enabled, disable it
    (progn
      (setq control-lock-mode-p nil)
      (customize-set-variable 'cursor-type '(bar . 3))
      (setq isearch-mode nil)   ; Hack for control lock staying off
      )
    (progn   ; Else, enable it
      (setq control-lock-mode-p t)
      (customize-set-variable 'cursor-type '(hbar . 3)))))

(provide 'control-lock)
