class Styles
  extend ElMixin

  def self.menu

    '
    - .font size/
    - .background color/
      - white/
      - black/
    - .list faces/
    - api/
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
      > See
      | For styling specific text (not just a pattern):
      <<< @overlay/
    - see/
      > List styles in current page in web browser
      <<< @css/list/
    '
    #       > Apply multiple fonts and groups

  end

  def self.background_color color

    $el.customize_set_variable(:background_mode_type, color == "black" ? :dark : :light)

    Notes.define_styles
    FileTree.define_styles
    Styles.use_xiki_color_scheme
    Styles.define_misc_face_colors
    Color.define_styles

    nil
  end

  def self.font_size size=nil

    # If nothing passed, show default sizes


    if ! size
      return "
        - original) #{Styles.height}
        - 110
        - 120
        - 135
        - 160
        - 200
        - 250
        "
    end

    Styles.define :default, :size=>size.to_i

    # To resize mode lines
    Themes.use "Shiny Blue"
    Notes.define_styles
    FileTree.define_styles

    nil
  end

  def self.zoom options={}

    increment = options[:out] ? -10 : 10
    increment *= 10 if Keys.prefix_u

    height = self.height
    height += increment
    self.font_size height
  end


  def self.toggle
    $el.elvar.font_lock_mode ?
      $el.font_lock_mode(0) :   # plain text
      $el.font_lock_mode(true)   # stylized text

    nil
  end

  def self.list_faces
    $el.with(:save_window_excursion) do
      $el.list_faces_display
    end
    View.to_buffer "*Faces*"

    nil
  end

  def self.inverse_bg enable=true
    $el.customize_set_variable(:background_mode_type, enable ? :dark : :light)
  end

  def self.inverse
    $el.boundp(:background_mode_type) &&
      $el.elvar.background_mode_type.to_s == 'dark'
  end

  def self.exand_colors options, keys
    keys.each do |key|
      next if ! options[key]
      options[key].sub! /^#/, ''
      options[key].sub! /^(.)(.)(.)$/, "\\1\\1\\2\\2\\3\\3"
    end
  end

  def self.method_missing *args
    self.define *args
  end

  def self.define name, options

    self.exand_colors options, [:bg, :fg]

    code = "(set-face-attribute (make-face '#{name.to_s.gsub("_", "-")}) nil\n"
    code << "  :background \"##{options[:bg]}\"\n" if options[:bg]
    code << "  :foreground \"##{options[:fg]}\"\n" if options[:fg]
    code << "  :family \"#{options[:face]}\"\n" if options[:face]

    if options[:size]
      size = options[:size]
      size = self.size(size.to_i) if size.is_a? String   # If a string, convert to relative size
      code << "  :height #{size}\n"
    end

    code += options[:strike] ? "  :strike-through t\n" : "  :strike-through nil\n" if options.has_key?(:strike)

    code += options[:underline] ? "  :underline t\n" : "  :underline nil\n" if options.has_key?(:underline)
    code += options[:overline] ? "  :overline t\n" : "  :overline nil\n" if options.has_key?(:overline)

    code += options[:bold] ? "  :weight 'bold\n" : "  :weight 'normal\n" if options.has_key?(:bold)
    if options[:border]
      border = options[:border]
      code <<
        if border.class == Symbol
          ":box nil\n"
        elsif border.class == String
          ":box '(:line-width 1 :color \"##{border}\")\n"
        elsif border.class == Array
          ":box '(:line-width #{border[1]} :color \"##{border[0]}\" :style #{border[2] || 'nil'})\n"
        end
    end
    code << "  )"

    $el.el4r_lisp_eval code
  end

  def self.define_misc_face_colors

    if Styles.inverse

      Styles.define :font_lock_comment_face, :fg=>'777'   # gray
      Styles.define :font_lock_function_name_face, :fg=>'f50'   # orange
      Styles.define :font_lock_type_face, :fg=>'0a1'   # green
      Styles.define :font_lock_variable_name_face, :fg=>'fd0'   # yellow
      Styles.define :font_lock_string_face, :fg=>'e10'   # red
      Styles.define :font_lock_keyword_face, :fg=>'999'

    else

      Styles.define :font_lock_comment_face, :fg=>'aaa'   # gray
      Styles.define :font_lock_function_name_face, :fg=>'f50'   # orange
      Styles.define :font_lock_type_face, :fg=>'090', :bold=>nil   # green
      Styles.define :font_lock_variable_name_face, :fg=>'00c', :bold=>1   # yellow
      Styles.define :font_lock_string_face, :fg=>'e10', :bold=>nil   # red
      Styles.define :font_lock_keyword_face, :fg=>'888', :bold=>1   # blue

    end


  end

  def self.apply(pattern, *styles)
    # Make back-slashes double
    pattern.gsub!("\\", '\\\\\\\\')
    code = "(font-lock-add-keywords nil '(( \"#{pattern}\"\n"
    styles.each_with_index do |f, i|
      code << "  (#{i} '#{f.to_s.gsub("_", "-")})\n" if f
    end
    code << "  )))"
    #$el.el4r_lisp_eval "(setq font-lock-keywords-case-fold-search t)"
    $el.el4r_lisp_eval code
    $el.font_lock_mode 1
    nil
  end

  def self.init

    # Make 'arial black' work in Linux
    $el.el4r_lisp_eval %`
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
      `
  end

  # Don't format quotes (it can override other styles)
  def self.clear
    $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"
  end

  def self.height
    $el.el4r_lisp_eval "(face-attribute 'default :height)"
  end

  def self.attribute style, attribute
    $el.el4r_lisp_eval "(face-attribute '#{style} :#{attribute})"
  end

  def self.size relative=0
    # Cache so subsequent calls don't have to look up from elisp
    size = $el.el4r_lisp_eval "(face-attribute 'default :height)"
    size ||= 90
    size + relative * 5
  end

  def self.use_xiki_color_scheme
    if Styles.inverse   # Use black
      # Mode line (bar between windows)

      $el.set_face_background :trailing_whitespace, "#333333"
      Styles.define :default, :bg=>'111111', :fg=>'ffffff'
      Styles.define :cursor, :bg=>'ffffff', :fg=>'000000'

      Styles.define :fringe, :bg=>'111111', :fg=>'666666'

      # Dark gray scheme
      Styles.define :mode_line, :bg=>'333', :border=>['666', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false, :fg=>'fff'
      Styles.define :mode_line_inactive, :bg=>'666', :border=>['888', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false, :fg=>'fff'

    else
      $el.set_face_background :trailing_whitespace, "#aaaaaa"
      Styles.define :default, :bg=>'ffffff', :fg=>'000000'# , :height=>110
      Styles.define :cursor, :bg=>'333333', :fg=>'ffffff'
      Styles.define :fringe, :bg=>'ffffff', :fg=>'cccccc'   # eg border / status

      Styles.define :mode_line, :fg=>'222222', :bg=>'555', :border=>['333', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false
      Styles.define :mode_line_inactive, :fg=>'999999', :bg=>'888', :border=>['666', -1], :face=>'Lucida Grande', :size=>'3', :bold=>false

    end

  end

end
Styles.init
