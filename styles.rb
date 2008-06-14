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

  def self.define name, options

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

    code << "  :underline t\n" if options[:underline]
    code << "  :strike-through t\n" if options[:strike]
    if options.has_key?(:bold)
      code += options[:bold] ?
        "  :weight 'bold\n" :
        "  :weight 'normal\n"
    end
#    code << ":box '(:line-width -1 :style released-button)\n" if options[:raised]
#    code << ":box '(:line-width -1 :style released-button)\n" if options[:raised]
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

    set_face_background :trailing_whitespace, "#eee"

    Styles.define :default,
      :bg => 'ffffff',
      :fg => '000000'
    Styles.define :cursor,
      :bg => '000000',
      :fg => 'ffffff'

    if self.inverse
      set_face_background :trailing_whitespace, "#333"
      Styles.define :default,
        :bg => '000000',
        :fg => 'ffffff'
      Styles.define :cursor,
        :bg => 'ffffff',
        :fg => '000000'
    end

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

end
Styles.init
