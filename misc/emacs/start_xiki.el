(message "")   ; Helps a tiny bit to keep the flicker down

(set-face-attribute 'mode-line nil :foreground "unspecified")
(set-face-attribute 'mode-line nil :background "unspecified")

(menu-bar-mode 0)
(setq-default mode-line-format "")   ; Clear out modeline so it doesn't flash during startup
(sit-for 0)

(setenv "XIKI_DIR"
  (replace-regexp-in-string "/misc/emacs/start_xiki.\\w+$" "" load-file-name)
)

; Remove annoying messages during startup...

(setq message--old (symbol-function 'message))
(defun message (fmt &rest args)
  "Ignore useless messages."
  (cond
    ((string-equal "Mark set" fmt))
    ((string-equal "Mark activated" fmt))
    ((string-match "^Loading " fmt))   ; Fails :(

    ((and
      args
      (or
        (string-match "^Loading " (car args))   ; Fails :(
        (string-match "^When done with this frame, type " (car args))
      )
    ))

    ((apply message--old fmt args))
  )
)

(setq-default mode-line-format "")   ; Clear out modeline so it doesn't flash during startup
(set-face-attribute 'mode-line nil :background 'unspecified)

; Starts up el4r...

(load (concat (getenv "XIKI_DIR") "/misc/emacs/xsh.emacs"))


(message "")   ; Keep it from showing stuff on the bottom

; Call ruby method that processes command line args...

(defun populate-xsh-command-line-args ()
  (setq xsh-command-line-args '())
  (let ((i 1) val)
    (while
      (setq val (getenv (concat "XSH_COMMAND_LINE_ARG_" (int-to-string i))))
      (setq xsh-command-line-args (append xsh-command-line-args (list val)))
      (setq i (+ 1 i))
    )
  )
)

(populate-xsh-command-line-args)

(message "-----")
(pp xsh-command-line-args)

(if (not (and (boundp 'xiki-emacs-daemon) xiki-emacs-daemon))
  (el4r-ruby-eval "Xiki::Xsh.run :args_via_env=>1")
)
