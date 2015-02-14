(message "")   ; This stops some flickering

; Not proven to affect flickering
;; (set-face-attribute (make-face 'menu) nil :background "#666666")
;; (set-face-attribute (make-face 'menu) nil :foreground "#00ffff")
;; (setq inhibit-startup-echo-area-message "craig")

; Where is this being over-written??
(set-frame-parameter nil 'background-mode 'dark)   ; Temp.......................

; C-k deletes remaining linebreak
(setq kill-whole-line t)

; Change this back to emacs 23 behavior, if emacs 24
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-line)

(setq-default indent-tabs-mode nil)   ; Otherwise, it'll insert tabs?
(setq-default tab-width 2)
(setq ring-bell-function 'ignore)   ; Don't make noise when escape

(add-to-list 'load-path (concat (getenv "XIKI_DIR") "/misc/emacs/libs/"))

; Control Lock
(require 'control-lock)
; Allow treating escape like cancel
(require 'normal-escape)

; Ruby mode...

(add-to-list 'auto-mode-alist '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))

; Isearch > Move cursor to beginning of match after search
(add-hook 'isearch-mode-end-hook 'isearch-goto-beginning-after-finished)
(defun isearch-goto-beginning-after-finished ()
  (when (and isearch-forward isearch-success)
  (goto-char isearch-other-end)))

; Backup files > with tilda and hash in names

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

; Make mouse wheel work (though only works on current view)...

(global-set-key (kbd "<mouse-4>") (lambda () (interactive)
  (scroll-down 1)
))
(global-set-key (kbd "<mouse-5>") (lambda () (interactive)
  (scroll-up 1)
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
(setq cua-auto-tabify-rectangles nil)   ; Doesn't seem to work :/

(add-to-list 'load-path (concat (getenv "XIKI_DIR") "/misc/emacs/el4r/"))
(require 'el4r)
(el4r-troubleshooting-keys)
(el4r-boot)

; Save cursor location when closing, and restore next time the file is visited
(require 'saveplace)
(setq-default save-place t)
(setq-default save-place-file "~/xiki/misc/saved_places")

(custom-set-variables
  '(auto-revert-interval 1)
  '(echo-keystrokes 0.5)   ; Delay until showing the prefix keys at the bottom (when user types C-x or C-c)
  '(search-ring-max 30)   ; Remember a few more searches
  '(show-trailing-whitespace t)   ; Expose trailing whitespace
)

; Lines from .emacs that should be uncommented?...

(setq ediff-split-window-function 'split-window-horizontally)   ; Make ediff horizontal

(setq ediff-diff-options "-w")

; This makes the file be colorzed as elisp
; Local Variables:
; mode: lisp
; End:

; Set some keys for doing what lisp normally does

(global-set-key (kbd "M-s") 'eval-last-sexp)   ; Like emacs C-x C-e
(global-set-key (kbd "M-q") 'save-buffers-kill-emacs)   ; Like emacs C-x C-q

; Beginning of line, end, and kill
(global-set-key (kbd "M-a") 'move-beginning-of-line)
(global-set-key (kbd "M-e") 'move-end-of-line)
(global-set-key (kbd "M-k") 'kill-line)

; Emacs help
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h M-k") 'describe-key)
(global-set-key (kbd "M-h M-f") 'describe-function)
(global-set-key (kbd "M-h M-v") 'describe-variable)

; Enable and disable control lock (move these definitions to control-lock.el__?)
(global-set-key (kbd "M-C-L") 'control-lock-enable)
(global-set-key (kbd "M-L") 'control-lock-enable)

