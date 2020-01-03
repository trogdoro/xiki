(setq normal-escape-enabled t)    ; Makes sure we enable escape checking at the very beginning
(setq xiki-escape-override nil)   ; Gives other code a chance to control what's run when esc

; 1. define function for when esc is pressed:
(defun normal-escape () (interactive)

  ; Pull in all events that come in quickly
  (let ((events '()) next (re-add-escape t) (keep-going t))

    ; Always get first char after escape...

    (when (setq next (read-event nil nil 0.15)) (push next events))

    ; Keep reading subsequent chars until > nil or esc...

    ;; (let ((keep-going t))
    (while keep-going
      (setq next (read-event nil nil 0.05))
      (when next
        (push next events)
      )
      ; esc or nil, so quit
      (when (or (equal next nil) (equal next 27))
        (setq keep-going nil)
      )
    )

    (if (= (length events) 0)
      (progn
        (setq normal-escape-enabled t)   ; Makes sure we re-enable escape checking

        (cond

          ; Over-ride set, so just call it
          ; Todo > call callback if there (and wasn't esc)
          (xiki-escape-override
            (apply xiki-escape-override '())
            (setq xiki-escape-override nil)
          )

          ; In the minibuffer, so just quit
          ; Not sure if we get called when in the minibuffer?
          ((window-minibuffer-p)
            (keyboard-escape-quit)
          )
          (mark-active
            (goto-char (mark))
            (deactivate-mark)
            (message "")   ; So it doesn't show "Cleared Mark"
          )
          (t
            (el4r-ruby-eval "Xiki::ControlTab.go :from_escape=>1")
          )
        )

      )

      ; Recent event happened right after, so just put it back in effect...

      ; It wasn't esc, so delete the over-ride if there was one, because it was meant for only esc
      (setq xiki-escape-override nil)

      ; Todo > 1. > remove (99 59 53 57 59 48 62 91) from end

      ; Remove weird event from queue

      ; Xterm sends this > when you type too quickly
      (when (equal (last events 11) '(99 48 59 55 57 50 59 49 52 62 91))
        (setq events (butlast events 11))
        (setq re-add-escape nil)
      )

      ; Terminal.app sends this > when you type too quickly
      (when (equal (last events 6) '(99 50 59 49 63 91))
        (setq events (butlast events 6))
        (setq re-add-escape nil)
      )

      ; Iterm sends this > when you type too quickly
      (when (equal (last events 8) '(99 59 53 57 59 48 62 91))
        (setq events (butlast events 8))
        (setq re-add-escape nil)
      )

      ; Todo > 2. > only do this code if > any left

      (when (> (length events) 0)   ; Check again, since we removed a potential weird event

        (dolist (event events)
          (setq unread-command-events (cons event unread-command-events))
        )

        (when re-add-escape
          (setq unread-command-events (cons 27 unread-command-events))
        )

        (setq normal-escape-enabled nil)   ; Disable intercepting esc for a moment, so it'll go through as normal

      )


    )
  )
)

; 2. turn on before each command:
(defun normal-escape-pre-command-handler () (interactive)
  (setq normal-escape-enabled t)
)
(add-hook 'pre-command-hook 'normal-escape-pre-command-handler)

; 3. Define map:
; This is needed because > without a map, there wouldn't be a separate
; command for escape. The command would be meta-something.
(setq normal-escape-map
  '((normal-escape-enabled keymap
    (27 . normal-escape))
  )
)

; 4. Add to end:
(add-to-ordered-list 'emulation-mode-map-alists 'normal-escape-map 300)


; Function to read one event and know whether it's escape
; It throws the event away if it's not escope.
(defun next-event-is-escape ()

  (let ((escape nil) (first-char (read-event nil nil 0.05)))   ; Read any others that follow imediately

    ; If 1st char in sequence is esc or Ctrl+Q
    (when (or (eq first-char 27) (eq first-char 17))
      (setq escape t)
    )   ; Read 1st event

    ; Ignore any characters typed right after
    (while (read-event nil nil 0.05)
      (setq escape nil)
    )
    escape
  )
)

(provide 'normal-escape)
