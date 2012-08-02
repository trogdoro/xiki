class TroubleShooting
  def self.keys
    return if ! $el

    $el.el4r_lisp_eval '
      (progn
        (global-set-key (kbd "M-l") (lambda () (interactive)
          "Load .emacs (reloading EmacsRuby)"
          (switch-to-buffer "*el4r:process*")
          (kill-buffer "*el4r:process*")
          (load-file "~/.emacs")
        ))
        (global-set-key (kbd "M-e")  (lambda () (interactive)
        "Go to EmacsRuby error"
          (find-file el4r-log-path)
          (revert-buffer t t t)
          (setq truncate-lines nil)
          (end-of-buffer)
          (re-search-backward "^  from ")
          (re-search-backward "^[A-Z]")
          (recenter 0)
        ))
      )
      '
  end
end
TroubleShooting.keys
