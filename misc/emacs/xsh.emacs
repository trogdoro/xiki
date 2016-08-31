(message "")   ; This stops some flickering

; Where is this being over-written??
(set-frame-parameter nil 'background-mode 'dark)   ; Temp.......................

; C-k deletes remaining linebreak
(setq kill-whole-line t)

; Change this back to emacs 23 behavior, if emacs 24
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-line) ; jump+yet

(setq-default indent-tabs-mode nil)   ; Otherwise, it'll insert tabs?
(setq-default tab-width 2)
(setq ring-bell-function 'ignore)   ; Don't make noise when escape

(setq js-indent-level 2)   ; 2-space indent in javascript

; Add to end of path, so libs (like ruby-mode) won't super-cede ones already installed
(add-to-list 'load-path (concat (getenv "XIKI_DIR") "/misc/emacs/libs/") t)

; Control Lock
(require 'control-lock)
; Allow treating escape like cancel
(require 'normal-escape)

; Isearch > Move cursor to beginning of match after search
(add-hook 'isearch-mode-end-hook 'isearch-goto-beginning-after-finished)
(defun isearch-goto-beginning-after-finished ()
  (when (and isearch-forward isearch-success)
  (goto-char isearch-other-end)))

; Backup files > with tilda and hash in names

(setq backup-inhibited t)   ; Disable backup
(setq auto-save-default nil)   ; Disable auto save


; Try to enable mouse support, but doesn't matter if it fails
(condition-case nil
  (progn
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
  )
  (error nil)   ; Rescue, and do nothing when error happens
)

; Make clicking on a vertical bar hide other windows...

(global-set-key [vertical-line mouse-1] 'delete-other-windows)

; Right-clicking in mode-line do "expose links"

(global-set-key [mode-line mouse-3] (lambda nil (interactive) (el4r-ruby-eval "Xiki::View.link_views")))

; Make mouse scroll wheel work...

(global-set-key (kbd "<mouse-4>") (lambda () (interactive)
  ; Wake it scroll 1 line at a time
  ; - Store each time we scroll
  ; - Only scroll if more than N miliseconds has passed since last

  (let ((elapsed (if (boundp 'xiki-scroll-last-time) (- (float-time) xiki-scroll-last-time) 0)))
    ; Scroll if it's been more than .01s
    (when (> elapsed 0.005)
      (mouse-select-window last-command-event)
      (scroll-down 1)
    )
  )

  (setq xiki-scroll-last-time (float-time))
))

(global-set-key (kbd "<mouse-5>") (lambda () (interactive)

  (let ((elapsed (if (boundp 'xiki-scroll-last-time) (- (float-time) xiki-scroll-last-time) 0)))
    ; Scroll if it's been more than .01s
    (when (> elapsed 0.005)
      (mouse-select-window last-command-event)
      (scroll-up 1)
    )

  )

  (setq xiki-scroll-last-time (float-time))
))

(menu-bar-mode -1)

; Disable chars at end when wrapping or truncating
(set-display-table-slot standard-display-table 0 ?\ )
(set-display-table-slot standard-display-table 'wrap ?\ )
(set-display-table-slot standard-display-table 'vertical-border ?\ )

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(message "")
(cua-mode 1)
(message "")
(setq cua-auto-tabify-rectangles nil)   ; Seems to be working

; Make Shift+Rightarrow enable selection and move
(global-set-key (kbd "<S-right>") (lambda () (interactive)
  (if (not (region-active-p)) (cua-set-mark))
  (forward-char)
))
; Make Shift+Leftarrow enable selection and move
(global-set-key (kbd "<S-left>") (lambda () (interactive)
  (if (not (region-active-p)) (cua-set-mark))
  (backward-char)
))
; Make Shift+Downarrow enable selection and move
(global-set-key (kbd "<S-down>") (lambda () (interactive)
  (if (not (region-active-p)) (cua-set-mark))
  (next-line)
))
; Make Shift+Uparrow enable selection and move
(global-set-key (kbd "<S-up>") (lambda () (interactive)
  (if (not (region-active-p)) (cua-set-mark))
  (previous-line)
))

; Make M-h etc simulate vim key movement by default - HJKL
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-l") 'forward-char)

(global-set-key (kbd "M-a") 'move-beginning-of-line)
(global-set-key (kbd "M-e") 'move-end-of-line)

; "Clear" > kill-line alternative > since C-k and M-k don't do it
(global-set-key (kbd "M-c") 'kill-line)

(add-to-list 'load-path (concat (getenv "XIKI_DIR") "/misc/emacs/el4r/"))
(require 'el4r)
(el4r-recommended-keys)
(el4r-boot)

; Save cursor location when closing, and restore next time the file is visited
(require 'saveplace)
(setq-default save-place t)
(setq-default save-place-file "~/.xiki/misc/saved_places")

(custom-set-variables
  '(auto-revert-interval 1)
  '(echo-keystrokes 0.5)   ; Delay until showing the prefix keys at the bottom (when user types C-x or C-c)
  '(search-ring-max 30)   ; Remember a few more searches
  '(show-trailing-whitespace t)   ; Expose trailing whitespace

  ; Does this keep backups from being made?
  '(auto-save-interval 999999)   ; Don't do auto-saving
  '(auto-save-timeout 999999)    ; Don't do auto-saving

  '(cua-prefix-override-inhibit-delay 0.1)   ; Only small delay after Ctrl+C, for cua-mode

  '(search-whitespace-regexp nil) ; Otherwise it includes linebreaks
  '(electric-indent-mode nil)   ; Stop auto-indenting in ruby (etc) modes
  '(setq backup-inhibited t)
  '(setq auto-save-default nil)
)

; Lines from .emacs that should be uncommented?...

(setq ediff-split-window-function 'split-window-horizontally)   ; Make ediff horizontal

(setq ediff-diff-options "-w")

; This makes the file be colorzed as elisp
; Local Variables:
; mode: lisp
; End:

; Selection is active, so show "^C Copy" etc...

(setq xiki-selection-text-in-bar-text "Arrows keys to select    ^C Copy  ^X Cut  ^V Paste    (or esc)")

; Todo > Find better place to put this?
(defun xiki-selection-text-in-bar () (interactive)
  (if (and (region-active-p) (or (not (boundp 'xiki-bar-special-text)) (not xiki-bar-special-text)))
    (setq xiki-bar-special-text
      xiki-selection-text-in-bar-text
    )
    (if (and
        (or deactivate-mark (not mark-active))
        (boundp 'xiki-bar-special-text)


        (eq (type-of xiki-bar-special-text) 'string)


        (string= xiki-bar-special-text xiki-selection-text-in-bar-text)
      )

      (setq xiki-bar-special-text
        nil
      )
    )
  )
)
(add-hook 'post-command-hook 'xiki-selection-text-in-bar)

; Make C-d not interfere when in shell-mode. This delays running it until shell-mode runs (to save some startup time)
(add-hook 'shell-mode-hook
  (lambda ()
    (define-key shell-mode-map (kbd "C-d") nil)
  )
)

; Indent when pasting, if pasted on and indented line
(defadvice cua-paste (after around-fun activate)
  (el4r-ruby-eval "Xiki::Clipboard.cua_paste_advice rescue nil")
)

; Remove trailing spaces after enter+group
(defadvice cua--deactivate-rectangle (after around-fun activate)
  (el4r-ruby-eval "Xiki::Clipboard.cua_rectangle_advice rescue nil")
)

; Show "Jump to:" prompt instead of "I-Search:" when ^J
; Also handles variants

(defadvice isearch-message-prefix (after around-fun activate)
  (setq ad-return-value
    (replace-regexp-in-string "^I-Search: " "jump: "
      (replace-regexp-in-string "^I-Search backward: " "jump (reverse): "
        (replace-regexp-in-string "^Failing \\(.+\\): " "\\1: [not found] "
          (replace-regexp-in-string "^[Ww]rapped \\(.+\\): " "\\1: [wrapped] "
            (replace-regexp-in-string "^[Oo]verwrapped \\(.+\\): " "\\1: [overwrapped] "
              ad-return-value
  ) ) ) ) ) )
)


; Make ^Z deselect when there's a selection
(defun xiki-deselect () (interactive) (deactivate-mark))
(define-key cua--prefix-override-keymap (kbd "C-z") 'xiki-deselect)

; Only scroll down one line when the cursor is moved off-screen
(setq scroll-step 1)

; Now, trying to wrap by default
(setq-default truncate-lines 1)

; Suppress annoying "file locked by...(s, q, p, ?)" message (annoying because you have to type a weird key to make it go away)
(defun ask-user-about-lock (file opponent)
  "always grab lock"
   t)

; Disable sh mode > until we figure out "file mode specification error wrong-type-argument" error
(dotimes (i 6)
  (setq auto-mode-alist (remove (rassoc 'sh-mode auto-mode-alist) auto-mode-alist))
)
; Removes shebang mappings
(dotimes (i 10)
  (setq interpreter-mode-alist (remove (rassoc 'sh-mode interpreter-mode-alist) interpreter-mode-alist))
)
