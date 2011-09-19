require 'effects'
require 'styles'

# Colors lines, and shows only colored lines
class Color
  extend ElMixin

  CODE_SAMPLES = %q<
    # Make current line be a color
    - red: Color.colorize :r
    - blue: Color.colorize :b
    - yellow: Color.colorize :y
  >

  @@colors = {
    "r"=>:color_rb_red, "y"=>:color_rb_yellow,
    "t"=>:color_rb_orange,
    "e"=>:color_rb_green, "b"=>:color_rb_blue, "u"=>:color_rb_purple,
    "l"=>:color_rb_light,
    "w"=>:color_rb_white,
  }

  def self.colorize char=nil
    char ||= Keys.input(:one_char => true, :prompt => 'Enter first letter of color: ')
    char = char.to_s

    # If h, just show all colors
    case char
    when "l"   # Mark as "map", so delete others in file first

      overlays = overlays_in(View.top, View.bottom)   # Get all overlays
      overlays.to_a.reverse.each do |o|   # Loop through and copy all
        if overlay_get(o, :face).to_s == "color-rb-light"
          delete_overlay(o)
        end
      end

    when "s"   # Search in all buffers for marked lines

      prefix = Keys.prefix

      if prefix.nil?
        light = overlays_in(View.top, View.bottom).to_a.find{|o| overlay_get(o, :face).to_s == "color-rb-light"}
        return View.to(overlay_start(light)) if light
        # Else, contine on and do Color.search
      end

      return CodeTree.display_menu("- Color.search/") if Keys.prefix == 8

      # Presumably :u
      CodeTree.display_menu("- Color.search 'light'/")
      return

    when "o"   # Outline of marked lines
      res = self.get_marked_lines
      res.gsub! /^/, "    | "

      file = View.file
      path = file ?
        "- #{File.expand_path(file).sub(/(.+)\//, "\\1/\n  - ")}\n" :
        "- buffer #{View.name}/\n"

      View.to_buffer("*outline of marked in #{path}")
      View.clear;  notes_mode
      View.insert path
      if res == "    | "
        return View.insert "    - Nothing was marked in this file!"
      end
      View.insert res
      Keys.clear_prefix
      View.to_line 3
      Move.to_line_text_beginning

      Tree.search :left=> Line.left, :number_means_enter=>true

      return

    when "c"   # Copy marked lines

      res = self.get_marked_lines

      Clipboard['0'] = res

    when "h"
      Hide.hide_unless_block { |l, bol, eol|
        # Whether current line contains an overlay of this color
        overlays_in(bol, eol).to_a.find{ |o|
          overlay_get(o, :face).to_s =~ /^color-rb-/
        }
      }
      recenter(-3)
      Hide.search
      return
    when "n"   # to next marker
      #       Keys.prefix_times do
      pos = next_overlay_change(View.cursor)
      #       end
      # If no overlay, may be at end, so continue on
      pos = next_overlay_change(pos) unless overlays_at(pos)
      return View.to(pos)
    when "p"   # to next marker
      pos = previous_overlay_change(View.cursor)
      pos = previous_overlay_change(pos-2) if overlays_at(pos-2)
      return View.to pos
    when "d"
      return delete_overlay( overlays_at(next_overlay_change(point_at_bol - 1))[0] )
    when "k"

      if Keys.prefix_u   # Don't delete map mark
        return remove_overlays
      end
      overlays = overlays_in(View.top, View.bottom)   # Get all overlays
      overlays.to_a.reverse.each do |o|   # Loop through and copy all
        if overlay_get(o, :face).to_s != "color-rb-light"
          delete_overlay(o)
        end
      end
      return

    when "a"
      return self.alternating
    end

    case Keys.prefix
    when :u   # If C-u, use region
      left, right = View.range
    when nil   # If nothing, do line
      left, right = Line.left, Line.right+1
    else   # Else, get N lines
      txt, left, right = View.txt_per_prefix
    end

    if ! @@colors[char]
      return View.message "No char color for '#{char}'"
    end

    over = make_overlay(left, right)
    overlay_put over, :face, @@colors[char]
  end

  def self.alternating
    orig = View.cursor
    # Get region to cover
    txt, left, right = View.txt_per_prefix
    View.cursor = left
    while(View.cursor < right)
      Color.colorize_line :color_rb_light
      Line.next 2
    end
    View.cursor = orig
  end

  def self.colorize_line face
    over = make_overlay(Line.left, Line.right+1)
    overlay_put over, :face, face
  end

  def self.define_styles   # For Keys.layout_kolor_light, etc.

    Styles.define :color_rb_glow1, :bg => "ffcc99"
    Styles.define :color_rb_glow2, :bg => "ff9933"

    if Styles.inverse
      Styles.define :color_rb_red, :bg => "550000"
      Styles.define :color_rb_orange, :bg => "442500"
      Styles.define :color_rb_yellow, :bg => "444400"
      Styles.define :color_rb_green, :bg => "113311"
      Styles.define :color_rb_white, :fg=>'222222', :bg=>'ffffff', :border=>['ffffff', -1]

      Styles.define :color_rb_light, :bg => "444444"

      Styles.define :color_rb_blue, :bg => "000055"
      Styles.define :color_rb_purple, :bg => "220033"

    else
      Styles.define :color_rb_red, :bg => "ffd5d5"
      Styles.define :color_rb_orange, :bg => "ffe5bb"
      Styles.define :color_rb_yellow, :bg => "f9f9aa"
      Styles.define :color_rb_green, :bg => "e0ffcc"
      Styles.define :color_rb_light, :bg => "dddddd"
      Styles.define :color_rb_white, :fg=>'222222', :bg=>'666666', :border=>['666666', -1]

      Styles.define :color_rb_blue, :bg => "dde5ff"
      Styles.define :color_rb_purple, :bg => "f2ddff"

    end

  end

  def self.get_marked_lines label=nil
    overlays = overlays_in(View.top, View.bottom)   # Get all overlays
    res = ""
    overlays.to_a.reverse.each do |o|   # Loop through and copy all
      if label
        next if overlay_get(o, :face).to_s != label
      end
      line = View.txt(overlay_start(o), overlay_end(o))
      line << "\n" unless line =~ /\n$/
      res << line
    end
    res
  end

  def self.search label=nil
    label = "color-rb-#{label}" if label

    orig = View.buffer
    txt = ""
    Buffers.list.to_a.each do |b|  # Each buffer open

      $el.set_buffer b
      res = self.get_marked_lines label
      next if res.empty?

      file = View.file
      next unless file
      path = file ?
        "- #{File.expand_path(file).sub(/(.+)\//, "\\1/\n  - ")}\n" :
        "- buffer #{View.name}/\n"

      txt << path
      txt << res.gsub!(/^/, "    | ")

    end
    View.to_buffer orig
    txt

  end

end
Color.define_styles
