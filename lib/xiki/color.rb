require 'xiki/effects'
require 'xiki/styles'

# Colors lines, and shows only colored lines
class Color

  def self.menu
    "
    > Make current line be red, yellow, or blue
    @Color.colorize :r
    @Color.colorize :y
    @Color.colorize :b

    > See
    << themes/
    << styles/
    "
  end


  @@colors = {
    "r"=>:color_rb_red, "y"=>:color_rb_yellow,
    "t"=>:color_rb_orange,
    "e"=>:color_rb_green, "b"=>:color_rb_blue, "u"=>:color_rb_purple,
    "l"=>:color_rb_light,
    "w"=>:color_rb_white,
  }

  def self.colorize char=nil
    char ||= Keys.input(:chars=>1, :prompt=>'Enter first letter of color: ')
    char = char.to_s
    # If h, just show all colors
    case char
    when "l"
      # We want there to be only one "light" line per file, so delete existing
      overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
      overlays.to_a.reverse.each do |o|   # Loop through and copy all
        if $el.overlay_get(o, :face).to_s == "color-rb-light"
          $el.delete_overlay(o)
        end
      end

    when "s"   # Search in all buffers for marked lines

      prefix = Keys.prefix

      if prefix.nil?
        light = $el.overlays_in(View.top, View.bottom).to_a.find{|o| $el.overlay_get(o, :face).to_s == "color-rb-light"}
        return View.to($el.overlay_start(light)) if light
        # Else, contine on and do Color.search
      end

      return Launcher.open("- Color.search/") if Keys.prefix == 8

      # Presumably :u
      Launcher.open("- Color.search 'light'/")
      return

    when "o"   # Outline of marked lines
      res = self.get_marked_lines
      res.gsub! /^/, "    | "

      file = View.file
      path = file ?
        "- #{File.expand_path(file).sub(/(.+)\//, "\\1/\n  - ")}\n" :
        "- buffer #{View.name}/\n"

      View.to_buffer("*outline of marked in #{path}")
      View.clear;  Notes.mode
      View.insert path
      if res == "    | "
        return View.insert "    - Nothing was marked in this file!"
      end
      View.insert res
      Keys.clear_prefix
      View.to_line 3
      Line.to_beginning

      Tree.search :left=> Line.left, :number_means_enter=>true

      return

    when "c"   # Copy marked lines

      res = self.get_marked_lines

      Clipboard['0'] = res

    when "h"   # Hilight
      prefix = Keys.prefix :clear=>1

      ignore, left, right = View.txt_per_prefix prefix, :just_positions=>1, :default_is_line=>1

      prefix == :u ?
        Effects.glow(:color=>:rainbow, :times=>4) :
        Effects.glow(:what=>[left, right], :times=>4)

      #     when "h"   # Hide
      #       Hide.hide_unless_block { |l, bol, eol|
      #         # Whether current line contains an overlay of this color
      #         $el.overlays_in(bol, eol).to_a.find{ |o|
      #           $el.overlay_get(o, :face).to_s =~ /^color-rb-/
      #         }
      #       }
      #       recenter(-3)
      #       Hide.search
      #       return
    when "n"   # to next marker
      #       Keys.prefix_times do
      pos = $el.next_overlay_change(View.cursor)
      #       end
      # If no overlay, may be at end, so continue on
      pos = $el.next_overlay_change(pos) unless $el.overlays_at(pos)
      return View.to(pos)
    when "p"   # to next marker
      pos = $el.previous_overlay_change(View.cursor)
      pos = $el.previous_overlay_change(pos-2) if $el.overlays_at(pos-2)
      return View.to pos
    when "d"
      overlays = $el.overlays_at($el.next_overlay_change($el.point_at_bol - 1))
      return View.beep "- No highlights after cursor!" if ! overlays
      return $el.delete_overlay(overlays[0])
    when "k"

      if Keys.prefix_u   # Don't delete map mark
        return $el.remove_overlays
      end
      overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
      overlays.to_a.reverse.each do |o|   # Loop through and copy all
        if $el.overlay_get(o, :face).to_s != "color-rb-light"
          $el.delete_overlay(o)
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

    over = $el.make_overlay(left, right)
    $el.overlay_put over, :face, @@colors[char]
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
    over = $el.make_overlay(Line.left, Line.right+1)
    $el.overlay_put over, :face, face
  end

  def self.define_styles   # For Keys.layout_kolor_light, etc.

    return if ! $el

    if Styles.dark_bg?

      Styles.define :color_rb_red, :bg => "500"
      Styles.define :color_rb_orange, :bg => "442500"
      Styles.define :color_rb_yellow, :bg => "440"
      Styles.define :color_rb_green, :bg => "131"
      Styles.define :color_rb_white, :fg=>'222', :bg=>'fff', :border=>['fff', -1]
      Styles.define :color_rb_light, :bg => "252525"

      Styles.define :color_rb_blue, :bg => "005"
      Styles.define :color_rb_purple, :bg => "203"

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

  def self.get_marked_lines label=nil
    overlays = $el.overlays_in(View.top, View.bottom)   # Get all overlays
    res = ""
    overlays.to_a.reverse.each do |o|   # Loop through and copy all
      if label
        next if $el.overlay_get(o, :face).to_s != label
      end
      line = View.txt($el.overlay_start(o), overlay_end(o))
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
        "@#{File.expand_path(file).sub(/(.+)\//, "\\1/\n  - ")}\n" :
        "- buffer #{View.name}/\n"

      txt << path
      txt << res.gsub!(/^/, "    | ")

    end
    View.to_buffer orig
    txt
  end


  # Returns list of colors at cursor
  # Color.at_cursor
  def self.at_cursor
    overlays = $el.overlays_in(Line.left, Line.right)
    overlays = overlays.to_a.reverse.inject([]) do |a, o|   # Loop through and copy all
      a.push $el.overlay_get(o, :face).to_s
    end
  end

end
Color.define_styles
