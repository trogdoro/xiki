; Where is this being over-written??
(set-frame-parameter nil 'background-mode 'dark)   ; Temp.......................

; C-k deletes remaining linebreak
(setq kill-whole-line t)

; Change this back to emacs 23 behavior, if emacs 24
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-line)

(setq-default indent-tabs-mode nil)   ;tp Otherwise, it'll insert tabs?
(setq-default tab-width 2)

(add-to-list 'load-path (concat (getenv "XIKI_DIR") "/misc/emacs/libs/"))

; Control Lock...

(require 'control-lock)

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

(add-to-list 'load-path (concat (getenv "XIKI_DIR") "/misc/emacs/el4r/"))
(require 'el4r)
(el4r-troubleshooting-keys)
(el4r-boot)

(cua-mode 1)
(setq cua-auto-tabify-rectangles nil)   ; Doesn't seem to work :/
(transient-mark-mode 1)

; Save cursor location when closing, and restore next time the file is visited
(require 'saveplace)
(setq-default save-place t)
(setq-default save-place-file "~/xiki/misc/saved_places")

(custom-set-variables
  '(auto-revert-interval 1)
  '(echo-keystrokes 2)   ; Don't show partial key strokes, unless pause is more than 3 seconds
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

