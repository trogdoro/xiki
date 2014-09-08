require 'xiki/core/effects'
require 'xiki/core/styles'

module Xiki
  # Colors lines, and shows only colored lines
  class Color

    def self.menu
      %`
      - docs/
        - overview/
          | Temporarily marks lines as red or green etc. as a way to make
          | them visually stand out.  This menu is primarily accessed via this
          | key shortcut:
          |
          |   layout+mark
          |
          | (Control-l Control-m).  Try layout+mark+red on this line, for example.
          | If you type it quickly the menu won't appear.
        - keys/
          @memorize/
            | mark line light gray : layout+mark+light
            | mark line green : layout+mark+green
            | mark line red : layout+mark+red
            | remove next color (except light) : layout+mark+delete
            | remove all color (except light) : layout+mark+kill
            | go to next color : layout+mark+next
            | go to previous color : layout+mark+previous
            | iterate through marked lines : 8+Tab
        - color meanings/
          > Lose convention for color meanings
          | light : temporary mark (removed when other in file, and not shown in layout+mark+show)
          | red : causing a problem
          | orange : todo
          | yellow : needs fixing
          | green : found
          | blue : borrow from this
          | purple : misc
          | white : very important
      - api/
        > Make current line be red, yellow, or blue
        @Color.mark "red"
        @Color.mark "yellow"
        @Color.mark "green"
      - see/
        <@ themes/
        <@ styles/
        <@ effects/
      `
    end

    @@colors = {
      "r"=>:color_rb_red, "y"=>:color_rb_yellow,
      "t"=>:color_rb_orange,
      "e"=>:color_rb_green, "b"=>:color_rb_blue, "u"=>:color_rb_purple,
      "l"=>:color_rb_light,
      "w"=>:color_rb_white,
    }

    @@colors_by_name = {
      "light"=>:color_rb_light,
      "red"=>:color_rb_red,
      "orange"=>:color_rb_orange,
      "yellow"=>:color_rb_yellow,
      "green"=>:color_rb_green, "blue"=>:color_rb_blue, "purple"=>:color_rb_purple,
      "white"=>:color_rb_white,
    }

    def self.colors_by_name
      @@colors_by_name
    end

    def self.mark color

      # /mark/, so show options...

      View.kill :force_recent=>1 if View.name == "mark/"

      # Back in the original view...

      if color == "light"
        # We want there to be only one "light" line per file, so delete existing
        overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
        overlays.to_a.reverse.each do |o|   # Loop through and copy all
          if $el.overlay_get(o, :face).to_s == "color-rb-light"
            $el.delete_overlay(o)
          end
        end
      end


      left, right = Line.left, Line.right+1
      over = $el.make_overlay(left, right)
      $el.overlay_put over, :face, @@colors_by_name[color]

      # Save time it was added
      $el.overlay_put over, :created_at, Time.now.to_f.to_s

      nil
    end

    # Color.position_of_next
    def self.position_of_next
      $el.next_overlay_change(View.cursor)
    end

    def self.next options={}
      View.kill if View.name == "mark/"

      column = View.column
      pos = nil
      Keys.prefix_times do
        pos = $el.next_overlay_change(View.cursor)
        pos = $el.next_overlay_change(pos) unless $el.overlays_at(pos)
        View.to(pos)
      end
      View.column = options[:column] || column
      pos != View.bottom
    end

    def self.previous
      View.kill if View.name == "mark/"
      column = View.column
      Move.to_axis   # So we don't "find" the line we're already on
      Keys.prefix_times do
        pos = $el.previous_overlay_change(View.cursor)
        pos = $el.previous_overlay_change(pos-2) if $el.overlays_at(pos-2)
        View.to pos
      end
      View.column = column

    end

    def self.clear_light

      # We want there to be only one "light" line per file, so delete existing
      overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
      overlays.to_a.reverse.each do |o|   # Loop through and copy all
        if $el.overlay_get(o, :face).to_s == "color-rb-light"
          $el.delete_overlay(o)
        end
      end

    end

    #   def self.alternating
    #     orig = View.cursor
    #     # Get region to cover
    #     txt, left, right = View.txt_per_prefix
    #     View.cursor = left
    #     while(View.cursor < right)
    #       Color.colorize_line :color_rb_light
    #       Line.next 2
    #     end
    #     View.cursor = orig
    #   end

    def self.define_styles   # For Keys.layout_kolor_light, etc.

      return if ! $el

      if Styles.dark_bg?

        Styles.define :color_rb_red, :bg => "500"
        Styles.define :color_rb_orange, :bg => "630"
        Styles.define :color_rb_yellow, :bg => "550"
        Styles.define :color_rb_green, :bg => "141"
        Styles.define :color_rb_white, :fg=>'222', :bg=>'fff', :border=>['fff', -1]
        Styles.define :color_rb_light, :bg => "333"

        Styles.define :color_rb_blue, :bg => "135"
        Styles.define :color_rb_purple, :bg => "315"

      else

        Styles.define :color_rb_red, :bg => "ffd5d5"
        Styles.define :color_rb_orange, :bg => "ffe5bb"
        Styles.define :color_rb_yellow, :bg => "f9f9aa"
        Styles.define :color_rb_green, :bg => "e0ffcc"
        Styles.define :color_rb_white, :fg=>'222', :bg=>'666', :border=>['666', -1]
        Styles.define :color_rb_light, :bg => "ddd"

        Styles.define :color_rb_blue, :bg => "dde5ff"
        Styles.define :color_rb_purple, :bg => "f2ddff"

      end

      Styles.define :fade7, :fg => "333"
      Styles.define :fade6, :fg => "555"
      Styles.define :fade5, :fg => "777"
      Styles.define :fade4, :fg => "999"
      Styles.define :fade3, :fg => "bbb"
      Styles.define :fade2, :fg => "ddd"
      Styles.define :fade1, :fg => "fff"

    end

    # Returns list of colors at cursor
    # Color.at_cursor
    def self.at_cursor
      overlays = $el.overlays_in(Line.left, Line.right)
      overlays = overlays.to_a.reverse.inject([]) do |a, o|   # Loop through and copy all
        a.push $el.overlay_get(o, :face).to_s
      end
    end

    # Builds up hash of all marks, sorted by time.
    # Structure of hash returned:
    # - key: time
    # - value: [file, line, color]
    def self.all_marks_hash
      hash = {}
      orig = View.buffer

      # For each buffer, add marked lines to hash...

      Buffers.list.to_a.each do |b|  # Each buffer open
        $el.set_buffer b

        file = View.file

        next if ! file   # For now, don't try to handle buffers

        # TODO: Rework to add to hash
        overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
        overlays.to_a.each do |o|   # Loop through and copy all
          line = View.txt($el.overlay_start(o), $el.overlay_end(o))
          face = $el.overlay_get(o, :face)
          created_at = $el.overlay_get(o, :created_at)
          next unless created_at && line.any? && face.any?
          face = face.to_s
          next if face == "color-rb-light"
          hash[created_at] = [file, line, face]
        end
      end

      View.to_buffer orig
      hash
    end

    def self.run_highlight

      prefix = Keys.prefix :clear=>1

      # No prefix, so show menu of colors...

      return Launcher.open("mark/", :letter=>1, :bar_is_fine=>1) if ! prefix

      # N+, so jump to nth visible label...

      if prefix.is_a?(Fixnum)
        column = View.column
        View.to_relative
        prefix.times{ Xiki::Color.next }
        View.column = column
        return
      end

      # up+, so jump to light label...

      if prefix.is_a?(Fixnum)
        View.flash "TODO > make option > Color.next :light"
        return
      end

    end

  end
  Color.define_styles
end
