module Xiki
  class Styles


    def self.reload_styles
      Notes.define_styles
      FileTree.define_styles
      Color.define_styles
    end

    # Sets the font size
    def self.font_size size=nil

      # If nothing passed, show default sizes...

      if ! size
        return "
          - original) #{Styles.get_font_size}
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
      Notes.define_styles
      FileTree.define_styles

      nil
    end

    # Returns the font size
    def self.get_font_size
      $el.el4r_lisp_eval "(face-attribute 'default :height)"
    end

    def self.zoom options={}

      increment = options[:out] ? -10 : 10

      increment *= 5 if Keys.prefix_u || options[:up]

      height = self.get_font_size
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

    def self.dark_bg?
      # $el.frame_parameter(nil, :background_mode).to_s == "light"
      # Styles.attribute(:default, :background)[1..1][/[0-7]/]

      # Hard-coded to true for now > try with light background!
      true
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

      return if ! $el

      self.exand_colors options, [:bg, :fg]

      frame = "nil" # if options[:all]
      # Probably doesn't matter > only 1 frame in xsh
      frame = "(selected-frame)" if @@frame_only

      # How 'frame' param works:
      #   - nil > all frames and new frames
      #   - t > new frames

      code = "(set-face-attribute (make-face '#{name.to_s.gsub("_", "-")}) #{frame}"
      code << "  :background \"##{options[:bg]}\"" if options[:bg]
      code << "  :foreground \"##{options[:fg]}\"" if options[:fg]
      code << "  :family \"#{options[:face]}\"" if options[:face]

      if options[:size]
        size = options[:size]
        size = self.size(size.to_i) if size.is_a? String   # If a string, convert to relative size
        size = [size, 1].max
        code << "  :height #{size}"
      end

      code.<< options[:strike] ? "  :strike-through t" : "  :strike-through nil" if options.has_key?(:strike)

      code.<< options[:underline] ? "  :underline t" : "  :underline nil" if options.has_key?(:underline)
      code.<< options[:overline] ? "  :overline t" : "  :overline nil" if options.has_key?(:overline)

      code.<< options[:bold] ? "  :weight 'bold" : "  :weight 'normal" if options.has_key?(:bold)

      if options[:border]
        border = options[:border]
        code <<
          if border.class == Symbol
            " :box nil"
          elsif border.class == String
            " :box '(:line-width 1 :color \"##{border}\")"
          elsif border.class == Array
            " :box '(:line-width #{border[1]} :color \"##{border[0]}\" :style #{border[2] || 'nil'})"
          end
      end
      code << "  )"

      $el.el4r_lisp_eval code
    end

    def self.apply(pattern, *styles)
      # Make back-slashes double
      pattern.gsub!("\\", '\\\\\\\\')
      code = "(font-lock-add-keywords nil '(( \"#{pattern}\"\n"
      styles.each_with_index do |f, i|
        code << "  (#{i} '#{f.to_s.gsub("_", "-")})\n" if f
      end
      code << "  )))"
      $el.el4r_lisp_eval code
      $el.font_lock_mode 1
      nil
    end

    def self.init
      return if ! $el

      # Make 'arial black' work in old linuxes
      $el.el4r_lisp_eval %`
        (custom-set-variables
          '(face-font-family-alternatives '(
            ("arial black" "arial" "DejaVu Sans")
            ("arial" "DejaVu Sans")
            ("courier" "Monospace")
            ("monaco" "Monospace")

            ("xiki" "verdana")

            ("verdana" "DejaVu Sans")
            ;("verdana" "arial")
            ))
          '(global-font-lock-mode t nil (font-lock))
          ;'(font-lock-defaults '(nil t))  ; messes up Open List Faces
          '(show-trailing-whitespace t)
          '(font-lock-keywords-case-fold-search t)
          )
        `

      size = $el.boundp(:xiki_default_font_size) ? $el.elvar.xiki_default_font_size : nil
      size ||= self.get_font_size
      $el.elvar.xiki_default_font_size = size

      #     $el.elvar.xiki_default_font_size = self.get_font_size
    end

    # Don't format quotes (it can override other styles)
    def self.clear
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"
    end

    # Styles.attribute "mode-line", "background"
    # Styles.attribute "mode-line", "box"
    def self.attribute style, attribute
      $el.el4r_lisp_eval "(face-attribute '#{style} :#{attribute})"
    end

    def self.size relative=0
      # Cache so subsequent calls don't have to look up from elisp
      size = $el.el4r_lisp_eval "(face-attribute 'default :height)"
      size ||= 90
      size + relative * 5
    end

    def self.default_font_size value=nil
      # if value passed, set it
      if value
        self.font_size value
        $el.elvar.xiki_default_font_size = value
        return
      end

      # If no value, just return the default size

      $el.elvar.xiki_default_font_size
    end

    @@frame_only = nil
    def self.frame_only= val
      @@frame_only = val
    end

  end
  Styles.init
end
