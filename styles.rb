class Styles
  extend ElMixin

  def self.menu

    '
    - .toggle_black_and_white/
    - docs/
      > Summary
      | How to use the Styles class.  You can change the color and font of text.
      | You define styles, then make them apply to the text that matches
      | regular expression.
      |
      > Define
      | Styles.define :red, :bg => "d77"
      |
      > Apply
      | Styles.apply "apply", :red
      |
      > Define more complex font
      | Styles.define :blueish,
      |   :fg => "99e",
      |   :face => "verdana",
      |   :size => 90,
      |   :bold => true
      |
    '
    #       > Apply multiple fonts and groups

  end

  def self.toggle_black_and_white
    "TODO implement"
  end

  def self.inverse_bg enable=true
    customize_set_variable(:background_mode_type, enable ? :dark : :light)
  end

  def self.inverse
    boundp(:background_mode_type) &&
      elvar.background_mode_type.to_s == 'dark'
  end

  def self.exand_colors options, keys
    keys.each do |key|
      next if ! options[key]
      options[key].sub! /^(.)(.)(.)$/, "\\1\\1\\2\\2\\3\\3"
    end
  end

  def self.define name, options

    self.exand_colors options, [:bg, :fg]

    code = "(set-face-attribute (make-face '#{name.to_s.gsub("_", "-")}) nil\n"
    code << "  :background \"##{options[:bg]}\"\n" if options[:bg]
    code << "  :foreground \"##{options[:fg]}\"\n" if options[:fg]
    code << "  :family \"#{options[:face]}\"\n" if options[:face]

    if options[:size]
      size = options[:size]
      # If a string, convert to relative size
      size = self.size(size.to_i) if size.is_a? String
      code << "  :height #{size}\n"
    end

    code << "  :strike-through t\n" if options[:strike]

    code += options[:underline] ? "  :underline t\n" : "  :underline nil\n" if options.has_key?(:underline)

    code += options[:bold] ? "  :weight 'bold\n" : "  :weight 'normal\n" if options.has_key?(:bold)
    if options[:border]
      border = options[:border]
      code <<
        if border.class == Symbol
          ":box nil\n"
        elsif border.class == String
          ":box '(:line-width 1 :color \"##{border}\")\n"
        elsif border.class == Array
          ":box '(:line-width #{border[1]} :color \"##{border[0]}\")\n"
        end
    end
    code << "  )"

    el4r_lisp_eval code
  end

  def self.apply(pattern, *styles)
    # Make back-slashes double
    pattern.gsub!("\\", '\\\\\\\\')
    code = "(font-lock-add-keywords nil '(( \"#{pattern}\"\n"
    styles.each_with_index do |f, i|
      code << "  (#{i} '#{f.to_s.gsub("_", "-")})\n" if f
    end
    code << "  )))"
    #el4r_lisp_eval "(setq font-lock-keywords-case-fold-search t)"
    el4r_lisp_eval code
    font_lock_mode 1
  end

  def self.init

    # Make 'arial black' work in Linux
    el4r_lisp_eval %q<
      (custom-set-variables
        '(face-font-family-alternatives '(
          ("arial black" "arial" "DejaVu Sans")
          ("arial" "DejaVu Sans")
          ("verdana" "DejaVu Sans")
          ;("verdana" "arial")
          ))
        '(global-font-lock-mode t nil (font-lock))
        ;'(font-lock-defaults '(nil t))  ; messes up Open List Faces
        '(show-trailing-whitespace t)
        '(font-lock-keywords-case-fold-search t)
        )
      >
      # '
  end

  # Don't format quotes (it can override other styles)
  def self.clear
    el4r_lisp_eval"(setq font-lock-defaults '(nil t))"
  end

  def self.size relative=0
    # Cache so subsequent calls don't have to look up from elisp
    @@size ||= el4r_lisp_eval "(face-attribute 'default :height)"
    @@size ||= 90
    @@size + relative * 5
  end

  def self.use_xiki_color_scheme
    if Styles.inverse   # Use black

      # Mode line (bar between windows)

      set_face_background :trailing_whitespace, "#555555"
      Styles.define :default, :bg=>'000000', :fg=>'ffffff'
      Styles.define :cursor, :bg=>'ffffff', :fg=>'000000'

      Styles.define :fringe, :bg=>'000000', :fg=>'666666'

      # Dark gray scheme
      Styles.define :mode_line, :bg=>'333', :border=>['666', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false, :fg=>'fff'
      Styles.define :mode_line_inactive, :bg=>'666', :border=>['888', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false, :fg=>'fff'

      #       # Aqua, blue bg, mac font
      #       Styles.define :mode_line, :fg=>'000000', :bg=>'2e7795', :border=>['387fa5', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false
      #       Styles.define :mode_line_inactive, :fg=>'363636', :bg=>'949494', :border=>['a2a2a2', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false

    else
      set_face_background :trailing_whitespace, "#cccccc"
      Styles.define :default, :bg=>'ffffff', :fg=>'000000'# , :height=>110
      Styles.define :cursor, :bg=>'333333', :fg=>'ffffff'
      Styles.define :fringe, :bg=>'ffffff', :fg=>'cccccc'   # eg border / status

      Styles.define :mode_line, :fg=>'222222', :bg=>'555', :border=>['333', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false
      Styles.define :mode_line_inactive, :fg=>'999999', :bg=>'888', :border=>['666', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false

    end

  end
end
Styles.init
