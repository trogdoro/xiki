require 'xiki/core/styles'

module Xiki
  # Makes visual things happen
  class Effects

    MENU = "
      | Do cool-looking things to text.  Used by various menus.
      - api/
        | Try these out by double-clicking on them.
        - Glow/
          =! Effects.glow
          =! Effects.glow :times=>5
          =! Effects.glow :what=>:paragraph
          =! Effects.glow :what=>:block
          =! Effects.glow :what=>[1, 100]
          - Colors/
            =! Effects.glow :color=>:fire
            =! Effects.glow :color=>:water
            =! Effects.glow :color=>:forest
            =! Effects.glow :color=>:rainbow
            =! Effects.glow :color=>:fat_rainbow
          - Fade in and out/
            =! Effects.glow :fade_in=>1
            =! Effects.glow :fade_out=>1
        - Blink/
          | Makes line blink orange. Using a longer time since the blink happens
          | anyway.
          =! Effects.blink :time=>1
        - View methods/
          =! View.prompt
          =! View.flash
          =! View.flash 'Saved!'
        - Things menus return/
          
      - docs/
        > Keys
        | do+line+effects: make line blink
        | up+do+line+effects: make line blink rainbow color
      - see/
        <= themes/
      "

    #
    # Effects.glow :color=>:forest
    # Effects.glow :fade_in=>1
    #
    def self.glow options={}

      what = options[:what]
      if what.is_a? Array
        left, right = what
      elsif what == :block
        # TODO: change this to :section!
        ignore, left, right = View.block_positions "^[|>]"
      elsif what == :paragraph
        left, right = View.paragraph :bounds=>1
      else
        left, right = Line.left, Line.right
      end

      # Set :times to 1 if no args and fade out
      times = 1 if ! options[:times] && (options[:fade_out] || options[:fade_in])

      times ||= options[:times] || 3

      over = $el.make_overlay left, right

      faces =
        if options[:color] == :fire
          ['font-lock-doc-face', 'font-lock-function-name-face', 'html-helper-server-script-face', 'font-lock-variable-name-face', 'speedbar-tag-face', 'speedbar-tag-face', 'speedbar-tag-face']
        elsif options[:color] == :forest
          ['dired-mark', 'dired-mark', 'widget-documentation', 'widget-documentation', 'widget-documentation', 'bookmark-menu-heading', 'bookmark-menu-heading']
        elsif options[:color] == :water
          ['font-lock-constant-face', 'font-lock-constant-face', 'escape-glyph', 'escape-glyph', 'font-lock-builtin-face', 'font-lock-builtin-face', 'font-lock-builtin-face']
        elsif options[:color] == :rainbow
          ['blue', 'red', 'orange', 'orange', 'green', 'green', 'yellow']
        elsif options[:color] == :fat_rainbow
          ['notes-yellow', 'notes-yellow', 'notes-green', 'notes-green', 'notes-blue', 'notes-red', 'notes-red']
        else
          ['fade1', 'fade2', 'fade3', 'fade4', 'fade5', 'fade6', 'fade7']
        end

      up = [6, 5, 4, 3, 2]
      down = [2, 3, 4, 5, 6]

      sequence =
        if options[:fade_out]; [1] + (down + [7]) * times
        elsif options[:fade_in]; [7] + (up + [1]) * times
        elsif options[:reverse]; [7] + (up + [1] + down + [7]) * times
        else; [1] + (down + [7] + up + [1]) * times
        end

      delay = options[:delay]
      delay ||= Environment.gui_emacs ? 0.022 : 0.042

      sequence.each do |i|
        $el.overlay_put over, :face, (faces[i-1] || faces[0])
        $el.sit_for delay

        # Temp for video recording > wait for longer
        # $el.sit_for 0.05
      end

      $el.delete_overlay over
    end

    def self.blink options={}
      what = options[:what]
      what ||= :line
      case what
      when :all
        left, right = View.top, View.bottom
      when :region
        left, right = View.range
      when :line
        left, right = Line.bounds
        right += 1
      when :sexp
        left, right = bounds_of_thing_at_point(:sexp).to_a
        return unless left
      end

      left = options[:left] if options[:left]
      right = options[:right] if options[:right]

      time = options[:time] || 0.04
      over2 = $el.make_overlay(left, right)
      $el.overlay_put over2, :face, :color_rb_glow2
      $el.sit_for time
      $el.delete_overlay over2
    end

    def self.define_styles

      Code.cache(:effects_define_styles) do

        Styles.define :color_rb_glow1, :bg=>"fec"
        Styles.define :color_rb_glow2, :bg=>"f90"

        Styles.define :red, :fg=>"f00"
        Styles.define :orange, :fg=>"f80"
        Styles.define :yellow, :fg=>"ff0"
        Styles.define :green, :fg=>"0f0"
        Styles.define :blue, :fg=>"00f"
        Styles.define :indigo, :fg=>"408"
        Styles.define :violet, :fg=>"82e"

        Styles.define :purple, :fg=>"808"
        Styles.define :cyan, :fg=>"f0f"
      end

    end

  end
end
