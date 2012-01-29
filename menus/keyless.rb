class Keyless
  def self.menu
    %`
    - keyless/this is an example
    - .apply/
      - .emacs view/
      - .web browser/
    - .browse/
    - docs/
      > Summary
      | Has to do with the "keyless
      |
      > Show keyless text inline
      - keyless/some text
      |
      > To show something in the browser
      | Use the "browse" menu.
    `
  end

  def self.emacs_view
    View.kill if View.name == "menu"

    Styles.apply ".+", :keyless
  end

  def self.web_browser
    Firefox.js "$('head').append('<style>body{font-family: keyless}</style>')"
  end

  def self.browse txt=nil
    return ".flash - Type some text" if txt.nil?
    "do"
  end

  def self.define_styles
    Styles.define :keyless, :size => '2', :face => "Keyless"
  end

  def self.apply_styles
    Styles.apply ".+", :keyless
  end

  def self.init
    $el.defun(:keyless_mode, :interactive => "", :docstring => "Apply keyless styles, etc") {
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"
      Keyless.apply_styles
    }

    self.define_styles

    $el.el4r_lisp_eval %q<
      (add-to-list 'auto-mode-alist '("\\\\.keyless\\\\'" . keyless-mode))
      >
  end
end
Keyless.init
