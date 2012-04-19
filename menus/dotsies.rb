class Dotsies
  def self.menu
    %`
    - dotsies/this is an example
    - .apply/
      - .emacs view/
      - .web browser/
    - .browse/
    - api/
      > HTML to include in web page
      | <link href='http://xiki.org/dotsies.css' rel='stylesheet' type='text/css'>
      | <style> .dotsies {font-family:Dotsies} </style>
    - docs/
      > Summary
      | Has to do with the "dotsies" font.
      |
      > Show dotsies text inline
      - dotsies/some text
      |
      > To show something in the browser
      | Use the "browse" menu.
    `
  end

  def self.emacs_view
    View.kill if View.name == "menu" || View.name =~ /^@/

    Styles.apply ".+", :dotsies
  end

  def self.web_browser
    Firefox.js "$('head').append('<style>body{font-family: dotsies}</style>')"
  end

  def self.browse txt=nil
    return ".flash - Type some text" if txt.nil?
    "do"
  end

  def self.define_styles
    Styles.define :dotsies, :size => '+2', :face => "Dotsies"
    Styles.define :dotsies_roman, :size => '+2', :face => "Dotsies Roman"
    Styles.define :dotsies_mono, :size => '+2', :face => "Dotsies Mono"
  end

  def self.apply_styles
    Styles.apply ".+", :dotsies
  end

  def self.init
    $el.defun(:dotsies_mode, :interactive => "", :docstring => "Apply dotsies styles, etc") {
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"
      Dotsies.apply_styles
    }

    self.define_styles

    $el.el4r_lisp_eval %q<
      (add-to-list 'auto-mode-alist '("\\\\.dotsies\\\\'" . dotsies-mode))
      >
  end
end
Dotsies.init
