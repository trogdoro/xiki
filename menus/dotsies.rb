class Dotsies
  def self.menu
    %`
    |~this is an example
    - .apply/
      - .one view/
      - .all views/
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
      |~Show dotsies text inline
      |
      > To show something in the browser
      | Use the "browse" menu.

    > See
    << dotsies tweets/
    `
  end

  def self.all_views

    View.kill if View.name == "menu" || View.name =~ /^@/

    faces = {
      :default=>"monaco",
      :ls_dir=>"verdana",
      :notes_exclamation=>"arial black",
      :notes_h1=>"arial",
    }

    if Styles.attribute(:default, :family) =~ /dotsies/
      faces.each{|k,v| Styles.define k, :face=>v}
    else
      faces.each{|k,v| Styles.define k, :face=>"dotsies"}
    end

    nil
  end

  def self.one_view
    View.kill if View.name == "menu" || View.name =~ /^@/

    $el.make_local_variable :dotsies_enabled
    dotsies_enabled = $el.boundp(:dotsies_enabled) && $el.elvar.dotsies_enabled

    font =
      if dotsies_enabled
        :default
      elsif Keys.prefix_u
        :dotsies_experimental
      else
        :dotsies
      end

    Styles.apply(".+", font)

    $el.elvar.dotsies_enabled = ! dotsies_enabled
    nil
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
    Styles.define :dotsies_white, :size => '+2', :face => "Dotsies", :fg=>"fff"
    Styles.define :dotsies_experimental, :size => '+2', :face => "Dotsies Experimental"
    Styles.define :dotsies_roman, :size => '+2', :face => "Dotsies Roman"
    Styles.define :dotsies_mono, :size => '+2', :face => "Dotsies Mono"
  end

  def self.apply_styles
    Styles.apply ".+", :dotsies
  end

  def self.init

    return if ! $el

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
