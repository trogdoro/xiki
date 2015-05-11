;; A slimmed-down and network-ified version of the el4r project.
;; El4r was created by rubikitch, and was incorporated into Xiki
;; with his permission.

; Define keys to reload and jump...
; This function is called from the Xiki config
(defun el4r-troubleshooting-keys ()

  ; Reload
  (global-set-key (kbd "M-r") 'el4r-kill-and-restart)
  (global-set-key [174] 'el4r-kill-and-restart)   ; For Terminal.app, in emacs24
  (global-set-key [2222] 'el4r-kill-and-restart)   ; For Terminal.app, in emacs22

  ; Jump to log and show error
  (global-set-key (kbd "M-l") 'el4r-jump-to-error)
  (global-set-key [172] 'el4r-jump-to-error)   ; For Terminal.app, in emacs24
  (global-set-key [2220] 'el4r-jump-to-error)   ; For Terminal.app, in emacs22

)

(defun ol (txt)
  (el4r-ruby-eval
    (if (stringp txt)
      (concat "Ol \"" txt "\"")   ; String, so no need to inspect
      (concat "Ol \"" (replace-regexp-in-string "\"" "\\\\\"" (pp-to-string txt)) "\"")
    )
  )
)

(or (>= emacs-major-version 21)
    (error "Sorry, el4r requires (X)Emacs21 or later, because it uses weak hashes."))

(put 'el4r-ruby-error
     'error-conditions
     '(error el4r-ruby-error))

(put 'el4r-ruby-error 'error-message "Error raised in Ruby.  Type Alt+R (or Option+R) to reload or Alt+L (or Option+L) to show the log with the error message.")

(defvar el4r-ruby-program
  "ruby"
  "The name of Ruby binary.")

(defvar el4r-instance-program (expand-file-name "~/src/el4r/bin/el4r-instance")
  "Full path of el4r-instance.")

(defvar el4r-instance-args nil)
(defvar el4r-debug-on-error nil)
(defvar el4r-coding-system nil)

(defvar el4r-process nil)
(defvar el4r-process-name "el4r")
(defvar el4r-process-bufname "*el4r:process*")
(defvar el4r-call-level 0)
(defvar el4r-last-error-desc nil)

(defvar el4r-ruby-object-ids nil)
(defvar el4r-ruby-object-weakhash nil)
(defvar el4r-defun-lambdas nil)
(defvar el4r-lisp-object-hash nil)
(defvar el4r-lisp-object-lastid 0)
(defvar el4r-lisp-object-gc-trigger-count 100)
(defvar el4r-lisp-object-gc-trigger-increment 100)

(defun call-process-to-string (program &rest args)
  (with-temp-buffer
    (apply 'call-process program nil t nil args)
    (buffer-string)))

(defun call-process-and-eval (program &rest args)
  (eval (read (apply 'call-process-to-string program args))))

(defun el4r-running-p ()
  (not (null el4r-process)))

(unless (fboundp 'process-send-signal)
  (defun process-send-signal (signal process-or-name)
    (signal-process (process-id (get-process process-or-name)) signal))
  )

(defun el4r-boot (&optional noinit)
  "Start el4r process, load ~/.el4r/init.rb, and prepare log buffer."
  (interactive "P")

  ;(if (not (get-buffer "*el4r:log*"))

  (with-current-buffer (get-buffer-create el4r-process-bufname) (erase-buffer))
  (with-current-buffer (get-buffer-create "*el4r:log*")
    (let ((buffer-read-only))
      (buffer-disable-undo)
      (erase-buffer)))

  ; Start ruby and make it listen...

  (el4r-init)
  (el4r-ruby-eval
    (if noinit "el4r_boot__noinit" "el4r_boot"))
)

(defun el4r-shutdown ()
  "Shutdown el4r."
  (interactive)
  (when (el4r-running-p)
    (el4r-ruby-eval "el4r_shutdown")
    (process-send-signal 'SIGTERM (process-name el4r-process))
    (setq el4r-process nil)
    ))

(defun el4r-restart ()
  "Shutdown then start el4r."
  (interactive)
  (el4r-shutdown)
  (el4r-boot))

(defun el4r-recover ()
  (interactive)
  (with-current-buffer el4r-process-bufname
    (erase-buffer)))

(defun el4r-override-variables ())

(defun el4r-ruby18-error ()
  (global-set-key "\C-q" 'kill-emacs)

  (run-with-idle-timer 0.2 nil (lambda ()
    (switch-to-buffer "ruby 1.8 error")
    (xterm-mouse-mode -1)
    (delete-region (point-min) (point-max))
    (insert "Your ruby version is 1.8, which is a really old version.

Ruby 1.9 or newer is required. Please install it, so that
when you run this, it shows ruby 1.9 or higher:

  $ ruby --version

1. Type Ctrl+Q to exit xsh.
2. Then upgrade ruby.
3. Then try running xsh again.

You may need to do something like:

  $ sudo apt-get install ruby1.9.3
  $ sudo ln -sf /usr/bin/ruby1.9.3 /usr/bin/ruby

")
  ))
  nil
)

(defun el4r-init ()
  ;; In many cases  (eq el4r-process (get-buffer-process el4r-process-bufname))
  ;; But this sexp is nil when el4r-instance is accidentally dead.
  (and (get-buffer-process el4r-process-bufname) (el4r-shutdown))

  (setq
    el4r-lisp-object-gc-trigger-count 100
    el4r-lisp-object-gc-trigger-increment 100
    el4r-ruby-gc-trigger-count 100
    el4r-ruby-gc-trigger-increment 100
    el4r-log-buffer "*el4r:log*"
    el4r-output-buffer "*el4r:output*"
    el4r-unittest-lisp-object-gc-trigger-count 5000
    el4r-unittest-lisp-object-gc-trigger-increment 5000
    el4r-unittest-ruby-gc-trigger-count 5000
    el4r-unittest-ruby-gc-trigger-increment 5000
    el4r-temp-file "/var/folders/66/3h81t2y913vf344z6fb2cks00000gn/T/el4r-tmp.tmp"
    el4r-instance-program "/Users/craig/Dropbox/xiki/misc/emacs/el4r/el4r-instance"
  )

  (el4r-override-variables)
  (setq el4r-lisp-object-hash (make-hash-table :test 'eq))
  (setq el4r-ruby-object-weakhash (make-hash-table :test 'eq :weakness 'value))
  (setq el4r-defun-lambdas nil)

  (let ((buffer el4r-process-bufname)
        (process-connection-type nil))  ; Use a pipe.
    (and (get-buffer buffer) (kill-buffer buffer))
    (get-buffer-create buffer)
    (with-current-buffer buffer
      (buffer-disable-undo)

      (setq el4r-process
        (if (and (boundp 'xiki-no-socket) xiki-no-socket)

          ; We're told not to use a socket, so start our own ruby process...

          (apply 'start-process el4r-process-name buffer
            el4r-ruby-program el4r-instance-program el4r-instance-args)

          ; Else, connect to separate ruby process, or start it...

          (condition-case err
            (make-network-process :remote (expand-file-name "~/.xikisock") :name el4r-process-name :buffer buffer)

            (file-error
              (let ((process-name (start-process "xiki-forker" (get-buffer-create "*xiki-forker*") el4r-ruby-program el4r-instance-program "forker")))
                (accept-process-output process-name)
              )

              ; If the script returned (lisp code) while forking, it's probably an error about ruby 1.8, so eval it
              (with-current-buffer "*xiki-forker*"
                (when (and (> (point-max) 1) (string= (buffer-substring 1 2) "("))
                  ;; (let ((txt (el4r-scan-expr-from-ruby-inner))) (eval (read txt)))
                  (eval (read (el4r-scan-expr-from-ruby-inner)))
                )
              )

              (message "el4r > tried 2nd time")
              (make-network-process :remote (expand-file-name "~/.xikisock") :name el4r-process-name :buffer buffer)
            )
            (error
              (message "el4r > unspecified error when connecting")
              (signal (car err) (cdr err))
            )
          )
        )

      )

      (and el4r-coding-system
           (set-process-coding-system el4r-process
                                      el4r-coding-system el4r-coding-system)))
    (setq xiki-child-pid (el4r-ruby-eval "Process.pid"))

    (message "")
  ))


(defun el4r-check-alive ()
  (or (eq (process-status el4r-process) 'run) (eq (process-status el4r-process) 'open)
      (error "el4r-instance is dead.")))

; Grabs elisp code from the *el4r:process* buffer
(defun el4r-scan-expr-from-ruby ()
  (with-current-buffer el4r-process-bufname
    (el4r-scan-expr-from-ruby-inner)
  )
)

; Pulls (ruby code) out of the current buffer, up until ^@.
(defun el4r-scan-expr-from-ruby-inner ()
  (goto-char (point-min))
  (save-match-data
    (let ((point-after-zero (search-forward "\0" nil t))
          expr)
      (if point-after-zero
        (progn
          (setq expr
                (buffer-substring (point-min) (- point-after-zero 1)))
          (delete-region (point-min) point-after-zero)
          expr
        )
      )
    )
  )
)


; Alternate method that loops and continually checks to see if el4r is
; still running. If we hit escape, it cancels out of it.
; We're using this all the time now, because it allows user to interrupt
; long-running operations by typing esc.

(if t

  ; Work-around
  (defun el4r-recv ()
    (let ((lock-time 20000) (expr) (deactivate-mark-orig deactivate-mark) (limit 0) (succeeded t))

      ; 1. Check frequently for a couple seconds, with no chance of interruption...

      (while (and (< limit lock-time) (eq nil (progn (setq expr (el4r-scan-expr-from-ruby)) expr)))
        (el4r-check-alive)
        (sleep-for 0.00001)
        (setq limit (1+ limit))
      )

      ; 2. Exceeded that, so check somewhat frequently, watching for an interruption...

      (when (>= limit lock-time)

        (message "Command taking a while. Press esc to cancel.")

        (setq succeeded (sit-for 0.1))

        (while (eq nil (progn (setq expr (el4r-scan-expr-from-ruby)) expr))
          ; Wait a bit
          (setq succeeded (sit-for 0.1))

          ; Original > check for escape and following events
          (if (and (not succeeded) (next-event-is-escape))
            (progn

              ; Move to the beginning of the command, to indicate it's cancelled
              (beginning-of-line)
              (skip-chars-forward " -$%")
              (setq quit-flag t)

            )
            ; Re-display message > it's going away for some reason
            (message "Command taking a while. Press esc to cancel.")
          )

          ; Else, keep on going

        )
        (message "")   ; To clear "Command taking a while..." message
      )

      (if (not deactivate-mark-orig)   ; If mark wasn't set to deactivate before, don't let the above elisp deactivate it
        (setq deactivate-mark nil)
      )
      expr
    ))

  ; Original method > disabled for now
  (defun el4r-recv ()
    (let ((expr) (deactivate-mark-orig deactivate-mark))
      (while (eq nil (progn (setq expr (el4r-scan-expr-from-ruby)) expr))
        (el4r-check-alive)
        (accept-process-output el4r-process))
      (if (not deactivate-mark-orig)   ; If mark wasn't set to deactivate before, don't let the above elisp deactivate it
        (setq deactivate-mark nil)
      )
      expr))

)

(defun el4r-send (rubyexpr)
  (el4r-check-alive)
  (process-send-string el4r-process rubyexpr)
  (process-send-string el4r-process "\0\n"))

(defvar el4r-error-lisp-expression nil)
(defun el4r-get ()
  (let ((result (el4r-recv)) expr)
    (while (eq (length result) 0)
      (el4r-wait-expr)
      (setq result (el4r-recv)))
    (condition-case err
        (eval (setq expr (read result)))
      (el4r-ruby-error (signal 'el4r-ruby-error nil))
      (error (setq el4r-error-lisp-expression expr)
             (signal (car err) (cdr err))))
    ))

(defun el4r-signal-last-error ()
  (signal (car el4r-last-error-desc) (cdr el4r-last-error-desc)))

(defun el4r-enter-call () (setq el4r-call-level (+ el4r-call-level 1)))
(defun el4r-leave-call () (setq el4r-call-level (- el4r-call-level 1)))
(defun el4r-callback-p () (not (eq el4r-call-level 0)))
(defun el4r-send-interrupt () (el4r-send ""))

(defun el4r-no-properties (str)
  (setq str (copy-sequence str))
  (set-text-properties 0 (length str) nil str)
  str)

(defvar el4r-treat-ctrl-codes nil)

(defvar el4r-temp-file nil)
(defvar obj nil)
(defsubst el4r-string-to-rubystr (str)
  (let ((file-read "File.read(conf.temp_file)"))
    (if (or (not el4r-treat-ctrl-codes)
            (string= str file-read))
        (concat "%q" (prin1-to-string (el4r-no-properties obj)))
      (cond ((eq el4r-treat-ctrl-codes 'use-file) ;experimental
             ;; !FIXME! coding-system @ XEmacs
             (with-temp-buffer
               (insert str)
               ;; suppress "wrote file-name" message
               ;; (find-efunctiondescr 'write-region "VISIT is neither")
               (write-region 1 (point-max) el4r-temp-file nil 0))
             file-read)
            (t
	      (concat "%Q"
                      (with-temp-buffer
                        (insert (prin1-to-string (el4r-no-properties str)))
                        (mapcar (lambda (x)
                                  (goto-char 1)
                                  (while (search-forward (car x) nil t)
                                    (replace-match (cdr x))))
                                '(("#" . "\\\\#")
                                  ("\003" . "\\\\cc")
                                  ("\004" . "\\\\cd")
                                  ("\021" . "\\\\cq")
                                  ("\023" . "\\\\cs")
                                  ("\026" . "\\\\cv")
                                  ("\027" . "\\\\cw")
                                  ("\031" . "\\\\cy")
                                  ("\032" . "\\\\cz")
                                  ))
                        (buffer-string)))

      )))))


(defun el4r-proper-list-p (expression)
  ;; Tell if a list is proper, id est, that it is `nil' or ends with `nil'.
  (cond ((not expression))
	((consp expression) (not (cdr (last expression))))))

(defun el4r-lisp2ruby (obj)
  (cond ((eq obj nil) "nil")
        ((eq obj t) "true")
        ((numberp obj) (number-to-string obj))
        ((stringp obj) (el4r-string-to-rubystr obj))
        ((el4r-rubyexpr-p obj) (el4r-rubyexpr-string obj))
        ((el4r-rubyobj-p obj)
         (format "el4r_rubyobj_stock.id2obj(%s)"
                 (el4r-rubyobj-id obj)))
        ((el4r-proper-list-p obj)
         (format "el4r_elobject_new(%d, ELListCell)"
                 (el4r-lisp-object-to-id obj)))
        ((and (consp obj) (atom (cdr obj)))
         (format "el4r_elobject_new(%d, ELConsCell)"
                 (el4r-lisp-object-to-id obj)))
        ((vectorp obj)
         (format "el4r_elobject_new(%d, ELVector)"
                 (el4r-lisp-object-to-id obj)))
        (t
         (format "el4r_elobject_new(%d)"
                 (el4r-lisp-object-to-id obj)))
        ))

(defun el4r-wait-expr ()
  (el4r-enter-call)
  (let (evaled rubyexpr)
    (condition-case err
        (progn (setq evaled (el4r-get))
               (setq rubyexpr (el4r-lisp2ruby evaled)))
      (el4r-ruby-error (setq rubyexpr "el4r_reraise_last_error"))
      (error (setq el4r-last-error-desc err)
             (setq rubyexpr "el4r_raise_lisp_error")))
    (el4r-send rubyexpr))
  (el4r-leave-call))

(defun el4r-ruby-eval (rubyexpr)
  (and (eq 0 (length rubyexpr)) (error "Empty expression is not evaluatable."))
  (and (el4r-callback-p) (el4r-send-interrupt))
  (el4r-enter-call)
  (el4r-send rubyexpr)
  (let ((result
      (let ((inhibit-quit t) (result2 nil) (succeeded nil))
        (with-local-quit
          (setq result2 (el4r-get))
          (setq succeeded t)   ; If it gets here, it succeeded
        )
        (if succeeded
          result2
          (progn
            ; Do stuff when C-g aborted half way through...

            (setq quit-flag nil)

            ; Send signal to raise exception  Kill process and start new?
            (signal-process xiki-child-pid 'SIGUSR1)

          )
        )
      )

    ))
    (el4r-leave-call)
    result
  )
)

(defun el4r-lisp-object-from-id (id)
  (or (gethash id el4r-lisp-object-hash)
      (error "No such object for given ID: %d" id)))

(defun el4r-lisp-object-to-id (obj)
  (el4r-gc-lisp-objects-if-required)
  (let ((id el4r-lisp-object-lastid))
    (setq el4r-lisp-object-lastid (+ id 1))
    (puthash id obj el4r-lisp-object-hash)
    id
    ))

(defun el4r-garbage-collect ()
  "Force garbage collection for el4r."
  (interactive)
  (el4r-gc-lisp-objects)
  (el4r-ruby-eval "el4r_rubyobj_stock.garbage_collect")
  (message "el4r garbage collected"))


(defun el4r-gc-lisp-objects ()
  (let ((ids (el4r-ruby-call nil 'el4r_get_garbage_lispobj_ids)))
    (while ids
      (remhash (car ids) el4r-lisp-object-hash)
      (setq ids (cdr ids))
    )))

(defun el4r-gc-lisp-objects-if-required ()
  (if (>= (hash-table-count el4r-lisp-object-hash)
          el4r-lisp-object-gc-trigger-count)
      (progn (el4r-gc-lisp-objects)
             (setq el4r-lisp-object-gc-trigger-count
                   (+ (hash-table-count el4r-lisp-object-hash)
                      el4r-lisp-object-gc-trigger-increment)))
    ))

(defun el4r-rubyobj-p (rubyobj)
  (and (listp rubyobj) (eq (car rubyobj) 'el4r-rubyobj)))
(defun el4r-rubyobj-id (rubyobj)
  (cdr rubyobj))
(defun el4r-rubyobj-create (id)
  (let ((rubyobj (cons 'el4r-rubyobj id)))
    (setq el4r-ruby-object-ids (cons id el4r-ruby-object-ids))
    (puthash id rubyobj el4r-ruby-object-weakhash)
    rubyobj))
(defun el4r-rubyobj-get-alive-ids ()
  (garbage-collect)

  ;; Introduce new variable `el4r-ruby-object-ids' and stop using
  ;; maphash to avoid fatal exception. I do not know why maphash
  ;; causes fatal. This idea is borrowed from pymacs.
  (let ((ids el4r-ruby-object-ids)
        used-ids)
    (while ids
      (let ((id (car ids)))
        (setq ids (cdr ids))
        (if (gethash id el4r-ruby-object-weakhash)
            (setq used-ids (cons id used-ids))
          )))
    (setq el4r-ruby-object-ids used-ids)
    used-ids))

(defun el4r-rubyexpr-p (rubyexpr)
  (and (listp rubyexpr) (eq (car rubyexpr) 'el4r-rubyexpr)))
(defun el4r-rubyexpr-string (rubyexpr)
  (cdr rubyexpr))
(defun el4r-rubyexpr-quote (string)
  (cons 'el4r-rubyexpr string))

;; disabled
' (defun el4r-list-to-rubyseq (list)
    (let (tokens)
      (while list
        (setq tokens
              (cons ", " (cons (el4r-lisp2ruby (car list)) tokens)))
        (setq list (cdr list)))
      (setq tokens (nreverse (cdr tokens)))
      (apply 'concat tokens)))
(defun el4r-list-to-rubyseq (list)
  (mapconcat (lambda (x)
               (el4r-lisp2ruby x))
             list ", "))

(defun el4r-list-to-rubyary (list)
  (el4r-rubyexpr-quote (format "[%s]" (el4r-list-to-rubyseq list))))
(defun el4r-cons-to-rubyary (cons)
  (el4r-list-to-rubyary (list (car cons) (cdr cons))))

(defalias 'el4r-vector-to-rubyseq 'el4r-list-to-rubyseq)
(defalias 'el4r-vector-to-rubyary 'el4r-list-to-rubyary)

(defun el4r-ruby-call (receiver name &rest args)
  "Invoke ruby's method. (RECEIVER can be nil.)"
  (setq name (cond ((symbolp name) (symbol-name name))
                   ((stringp name) name)
                   (t (error "Invalid value for method name: %s" name))))
  (setq receiver (cond ((eq receiver nil) "self")
                       (t (format "el4r_rubyobj_stock.id2obj(%s)"
                                  (el4r-rubyobj-id receiver)))))
  (el4r-ruby-eval (format "%s.%s(%s)"
                          receiver
                          name
                          (el4r-list-to-rubyseq args))))

(defun el4r-lambda-for-rubyproc (rubyproc-id &rest preform)
  (let ((lmd (eval (append '(lambda (&rest args))    ;; Lazy list play!
                           preform
                           (list (list 'el4r-ruby-call-proc-by-id
                                       rubyproc-id
                                       'args)))
                   )))
    (setq el4r-ruby-object-ids (cons rubyproc-id el4r-ruby-object-ids))
    (puthash rubyproc-id lmd el4r-ruby-object-weakhash)
    lmd))


(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    (and transient-mark-mode mark-active)))

(defun el4r-ruby-call-proc-by-id (rubyproc-id args)
  (el4r-ruby-eval (format "el4r_rubyobj_stock.id2obj(%s).call(%s)"
    rubyproc-id (el4r-list-to-rubyseq args)))
)


(defun el4r-ruby-call-proc (rubyproc &rest args)
  (el4r-ruby-call-proc-by-id (el4r-rubyobj-id rubyproc) args))

(defun el4r-ruby-eval-prompt (expr)
  "Read and execute Ruby code."
  (interactive "Eval Ruby: ")
  (message (prin1-to-string (el4r-ruby-eval expr)))
  )

(defun el4r-ruby-eval-region (point mark)
  "Execute the region as Ruby code."
  (interactive "r")
  (el4r-ruby-eval-prompt (buffer-substring point mark)))

(defun el4r-ruby-eval-buffer ()
  "Execute the buffer as Ruby code."
  (interactive)
  (el4r-ruby-eval-prompt (buffer-string)))

(defun el4r-debug-ruby-eval-report (expr)
  (interactive "Eval Ruby: ")
  (insert (format "%s\n  => %s\n"
                  expr
                  (prin1-to-string (el4r-ruby-eval expr)))))

(defun el4r-register-lambda (func)
  (setq el4r-defun-lambdas (cons func el4r-defun-lambdas)))

(defun el4r-define-function (name func)
  (fset name func)
  (el4r-register-lambda func)
  nil)


(defun el4r-kill-and-restart () (interactive)
  "Load .emacs (reloading EmacsRuby)"

  (let ((lock (control-lock-enabled)))

    ; Only kill if xiki running
    (when (or (eq (process-status el4r-process) 'run) (eq (process-status el4r-process) 'open))
      (el4r-ruby-eval "Xiki.kill")
      ; Give it time to die, so we don't error when trying to use ~/.xikisock again
      (sleep-for 0.1)
    )

    (when (get-buffer "*el4r:process*")
      (let (kill-buffer-query-functions)   ; So it doesn't prompt us to delete the buffer
        (kill-buffer "*el4r:process*")
      )
    )

    (load-file (concat (getenv "XIKI_DIR") "/misc/emacs/xsh.emacs"))

    ; Re-do client-specific init stuff with this new process clone
    (el4r-ruby-eval "Xiki.init_in_client")

    (message "reloaded")
  )
)

(defun control-lock-enabled ()
  (and (boundp control-lock-mode-p) control-lock-mode-p)
)

(defun el4r-jump-to-error ()
  (interactive)
  "Go to EmacsRuby error"
  (find-file el4r-log-path)
  (revert-buffer t t t)
  (setq truncate-lines t)
  (end-of-buffer)

  (re-search-backward "^  from ")
  (re-search-backward "^\\S-")
  (recenter 0)

)

(provide 'el4r)
