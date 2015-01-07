; Clear out scratch, otherwise it would flash first...

(with-current-buffer "*scratch*"
  (erase-buffer))

; It's our job to make this emacs ready for emacsclient sessions to join, so set up the hook, define variable, and load start_xiki.el in this dir...

(add-hook 'after-make-frame-functions (lambda (frame) (interactive)
  (set-buffer "*scratch*")
  (run-with-idle-timer 0 nil (lambda ()
    (el4r-ruby-eval "::Xiki::Xsh.run :args_via_daemon=>1")
  ))
))

(setq xiki-emacs-daemon t)

; Load start_xiki.el in this dir, which will start xiki...

(load (replace-regexp-in-string "_daemon.el$" ".el" load-file-name))
