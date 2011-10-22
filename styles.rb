class Styles
  extend ElMixin

  CODE_SAMPLES = %q[
    # Define
    Styles.define :red, :bg => "d77"

    # Define more complex font
    Styles.define :ls_dir,
      :fg => "99e",
      :face => "verdana",
      :size => 90,
      :bold => true

    # Apply
    Styles.apply "apply", :red
  ]

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
    return self.use_xiki_color_scheme_black if Styles.inverse   # Use black

    set_face_background :trailing_whitespace, "#cccccc"
    Styles.define :default, :bg=>'ffffff', :fg=>'000000'# , :height=>110
    Styles.define :cursor, :bg=>'333333', :fg=>'ffffff'
    Styles.define :fringe, :bg=>'ffffff', :fg=>'cccccc'   # eg border / status
    Styles.define :mode_line, :fg=>'222222', :bg=>'666666', :border=>['666666', -1]
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'666666', :border=>['666666', -1]
    Styles.define :mode_line_inactive, :fg=>'999999', :bg=>'cccccc', :border=>['cccccc', -1]

    #     Styles.define :ls_quote,
    #       :size => "-1",
    #       :fg => "669"
    #       :fg => "88b"
    #       :fg => "aad"

  end

  def self.use_xiki_color_scheme_black
    set_face_background :trailing_whitespace, "#555555"
    #     Styles.define :default, :bg=>'2A211C', :fg=>'ccbbaa'
    #     Styles.define :default, :bg=>'1c1712', :fg=>'ccbbaa'
    Styles.define :default, :bg=>'000000', :fg=>'ffffff'
    #     Styles.define :default, :bg=>'000000', :fg=>'cccccc'
    Styles.define :cursor, :bg=>'ffffff', :fg=>'000000'


    #     Styles.define :fringe, :bg=>'666666', :fg=>'666666'
    Styles.define :fringe, :bg=>'000000', :fg=>'666666'
    #     Styles.define :fringe, :bg=>'2A211C', :fg=>'666666'




    # Mode line (bar between windows)

    # aqua, mac font, lighter light
    Styles.define :mode_line, :fg=>'000000', :bg=>'2e7795', :border=>['387fa5', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false
    Styles.define :mode_line_inactive, :fg=>'363636', :bg=>'949494', :border=>['a2a2a2', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false

    #     # aqua, mac font, less green
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'2e7095', :border=>['387fa5', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false
    #     Styles.define :mode_line_inactive, :fg=>'393939', :bg=>'949494', :border=>['a2a2a2', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false

    #     # aqua, mac font, lighter both
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'2775a5', :border=>['3880b5', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false
    #     Styles.define :mode_line_inactive, :fg=>'3c3c3c', :bg=>'949494', :border=>['a2a2a2', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false

    #     # aqua, mac font
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'2e7795', :border=>['387fa5', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false
    #     Styles.define :mode_line_inactive, :fg=>'363636', :bg=>'848484', :border=>['929292', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false

    #     # aqua, mac font - more color
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'2e7ea5', :border=>['3887b5', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false
    #     Styles.define :mode_line_inactive, :fg=>'363636', :bg=>'848484', :border=>['929292', -4], :face=>'Lucida Grande', :size=>'3', :bold=>false

    #     # aqua, bigger font
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'2e7795', :border=>['387fa5', -4], :face=>'verdana', :size=>'3', :bold=>false
    #     Styles.define :mode_line_inactive, :fg=>'333333', :bg=>'848484', :border=>['929292', -4], :face=>'verdana', :size=>'3', :bold=>false

    #     # big font, aqua - lighter gray
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'2e7795', :border=>['367da3', -4], :face=>'arial', :size=>'5'
    #     Styles.define :mode_line_inactive, :fg=>'333333', :bg=>'848484', :border=>['909090', -4], :face=>'arial', :size=>'5'

    #     # big font, aqua
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'2e7795', :border=>['367da3', -4], :face=>'arial', :size=>'5'
    #     Styles.define :mode_line_inactive, :fg=>'333333', :bg=>'747474', :border=>['808080', -4], :face=>'arial', :size=>'5'

    #     # big font, darker bg
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'309630', :border=>['36a336', -4], :face=>'arial', :size=>'5'
    #     Styles.define :mode_line_inactive, :fg=>'333333', :bg=>'747474', :border=>['808080', -4], :face=>'arial', :size=>'5'

    #     # big font, dark text
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'339933', :border=>['36a336', -4], :face=>'arial', :size=>'5'
    #     Styles.define :mode_line_inactive, :fg=>'333333', :bg=>'777777', :border=>['808080', -4], :face=>'arial', :size=>'5'

    #     # big font, black text
    #     Styles.define :mode_line, :fg=>'000000', :bg=>'339933', :border=>['36a336', -3], :face=>'arial', :size=>'5'
    #     Styles.define :mode_line_inactive, :fg=>'000000', :bg=>'777777', :border=>['808080', -3], :face=>'arial', :size=>'5'

    #     # big font with borders
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['36a336', -3], :face=>'arial', :size=>'5'
    #     Styles.define :mode_line_inactive, :fg=>'bbbbbb', :bg=>'777777', :border=>['7e7e7e', -3], :face=>'arial', :size=>'5'

    #     #     # arial
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'arial', :size=>'5'
    #     Styles.define :mode_line_inactive, :fg=>'bbbbbb', :bg=>'777777', :border=>['777777', -1], :face=>'arial', :size=>'5'

    #     # verdana
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'verdana', :size=>'3'
    #     Styles.define :mode_line_inactive, :fg=>'bbbbbb', :bg=>'777777', :border=>['777777', -1], :face=>'verdana', :size=>'3'

    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'hei', :size=>'3'
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'courier', :size=>'3'
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'monaco', :size=>'3'
    #     # arial
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'arial', :size=>'4'
    #     Styles.define :mode_line_inactive, :fg=>'dddddd', :bg=>'777777', :border=>['777777', -1], :face=>'arial', :size=>'4'

    # Flat green and brighter font
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'lucida', :size=>'3'
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1], :face=>'verdana', :size=>'3'
    #     Styles.define :mode_line_inactive, :fg=>'bbbbbb', :bg=>'777777', :border=>['777777', -1], :face=>'verdana', :size=>'3'

    #     # Flat green and brighter font
    #     Styles.define :mode_line, :fg=>'aaffaa', :bg=>'339933', :border=>['339933', -1]
    #     Styles.define :mode_line_inactive, :fg=>'bbbbbb', :bg=>'777777', :border=>['777777', -1]

    #     # Diff-style green
    #     Styles.define :mode_line, :fg=>'44dd33', :bg=>'113300', :border=>['113300', -2]
    #     Styles.define :mode_line_inactive, :fg=>'777777', :bg=>'333333', :border=>['333333', -2]


    #     # Diff-style green - inactive also green
    #     Styles.define :mode_line, :fg=>'44dd33', :bg=>'113300', :border=>['113300', -2]
    #     Styles.define :mode_line_inactive, :fg=>'227700', :bg=>'113300', :border=>['113300', -2]

    #     # Diff-style green - Hybrid
    #     Styles.define :mode_line, :fg=>'44dd33', :bg=>'113300', :border=>['113300', -2]
    #     Styles.define :mode_line_inactive, :fg=>'bbbbbb', :bg=>'777777', :border=>['777777', -2]

    #     # Diff-style green - and red
    #     Styles.define :mode_line, :fg=>'44dd33', :bg=>'113300', :border=>['113300', -2]
    #     Styles.define :mode_line_inactive, :fg=>'ee3333', :bg=>'440000', :border=>['440000', -2]

    #     # Diff-style green - and blue
    #     Styles.define :mode_line, :fg=>'44dd33', :bg=>'113300', :border=>['113300', -2]
    #     Styles.define :mode_line_inactive, :fg=>'5555ff', :bg=>'111155', :border=>['111155', -2]

    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'338833', :border=>['227722', -4]
    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'338833', :border=>['449944', -5]

    #     # Flat green and almost white
    #     Styles.define :mode_line, :fg=>'aaeeaa', :bg=>'338833', :border=>['338833', -2]
    #     Styles.define :mode_line_inactive, :fg=>'bbbbbb', :bg=>'777777', :border=>['777777', -2]


    #     # Flat green and white
    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'338833', :border=>['338833', -2]
    #     Styles.define :mode_line_inactive, :fg=>'ffffff', :bg=>'999999', :border=>['999999', -2]

    #     # Flat white and gray
    #     Styles.define :mode_line, :fg=>'999999', :bg=>'ffffff', :border=>['ffffff', -2]
    #     Styles.define :mode_line_inactive, :fg=>'ffffff', :bg=>'999999', :border=>['999999', -2]


    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'000000', :border=>['444444', -4], :bold=>false
    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'000000', :border=>['333333', -3]
    #     Styles.define :mode_line_inactive, :fg=>'ffffff', :bg=>'999999', :border=>['999999', -2]


    #     # Light bars
    #     Styles.define :mode_line, :fg=>'444444', :bg=>'eeeeee', :border=>['cccccc', -1], :bold=>false
    #     Styles.define :mode_line_inactive, :fg=>'999999', :bg=>'eeeeee', :border=>['cccccc', -1]


    #     # Black bars - gray border
    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'000000', :border=>['444444', -4], :bold=>false
    #     Styles.define :mode_line_inactive, :fg=>'999999', :bg=>'000000', :border=>['444444', -3]

    #     # Black bars
    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'000000', :border=>['000000', 1], :bold=>false, :underline=>false
    #     Styles.define :mode_line_inactive, :fg=>'999999', :bg=>'000000', :border=>['000000', -2]

    #     # Black bars - underline
    #     Styles.define :mode_line, :fg=>'ffffff', :bg=>'000000', :border=>['000000', 1], :bold=>false, :underline=>true
    #     Styles.define :mode_line_inactive, :fg=>'999999', :bg=>'000000', :border=>['000000', -2]













    #     Styles.define :mode_line_inactive, :fg=>'ffffff', :bg=>'999999', :border=>['bbbbbb', -2]

  end
end
Styles.init
