(message "")

; This line makes > the top bar not flash white bg
; must be before > the sit-for line
(menu-bar-mode -1)

; This line makes > the bottom bar not flash gray bg
; must be before > the sit-for line
(set-face-attribute 'mode-line nil :background "unspecified")

; These 2 lines being in this order > is critical to help the flashing
; This line makes > the bottom bar not flash black text
; must be before > the sit-for line
(setq-default mode-line-format "")   ; Clear out modeline so it doesn't flash during startup

(sit-for 0)

; Load some libraries separately, because if not it loads during find-file and shows annoying message
(load "image")
(load "disp-table")
(load "cua-base")
(load "edmacro")
(load "xt-mouse")
(message "")


(setenv "XIKI_DIR"
  (replace-regexp-in-string "/misc/emacs/start_xiki.\\w+$" "" load-file-name)
)

; Remove annoying messages during startup > suppress...

(setq message--old (symbol-function 'message))
(defun message (fmt &rest args)
  "Ignore useless messages."
  (cond
    ((string-equal "Mark saved where search started" fmt))
    ((string-equal "Mark set" fmt))
    ((string-equal "Mark activated" fmt))
    ((string-equal "Mark cleared" fmt))
    ((string-match "^Loading places from " fmt))

    ((string-match "^Beginning of buffer" fmt))

    ((and
      args
      (or
        (string-match "^When done with this frame, type " (car args))
        (string-match "^Beginning of buffer" (car args))
      )
    ))

    ((apply message--old fmt args))
  )
)

; Suppress "Beginning of Buffer" message.

(defadvice previous-line (around silencer activate)
  (condition-case nil
    ad-do-it
    ((beginning-of-buffer))))
(defadvice next-line (around silencer activate)
  (condition-case nil
      ad-do-it
    ((end-of-buffer))))

(defadvice scroll-down (around silencer activate)
  (condition-case nil
    ad-do-it
    ((beginning-of-buffer))))
(defadvice scroll-up (around silencer activate)
  (condition-case nil
      ad-do-it
    ((end-of-buffer))))

(setq-default mode-line-format "")   ; Clear out modeline so it doesn't flash during startup
(set-face-attribute 'mode-line nil :background 'unspecified)

; Starts up el4r...

(load (concat (getenv "XIKI_DIR") "/misc/emacs/xsh.emacs"))
(message "")

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

(if (not (and (boundp 'xiki-emacs-daemon) xiki-emacs-daemon))
  (el4r-ruby-eval "Xiki::Xsh.run :args_via_env=>1")
)

