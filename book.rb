class Book
  def self.apply_styles
    Styles.define :blocks,
      :size => '+5',
      :face => "Keyless"
    Styles.apply ".+", :blocks
  end

  def self.init
    $el.defun(:book_mode, :interactive => "", :docstring => "Apply book styles, etc") {
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"
      Book.apply_styles
    }

    $el.el4r_lisp_eval %q<
      (add-to-list 'auto-mode-alist '("\\\\.book\\\\'" . book-mode))
      >
  end
end
Book.init
