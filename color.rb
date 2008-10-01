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
    "r" => :color_rb_red, "o" => :color_rb_orange, "y" => :color_rb_yellow,
    "e" => :color_rb_green, "b" => :color_rb_blue, "p" => :color_rb_purple,
  }

  def self.colorize char=nil
    char ||= Keys.input(:one_char => true, :prompt => 'Enter first letter of color: ')
    char = char.to_s

    # If h, just show all colors
    case char
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
    when "d"
      return delete_overlay( overlays_at(next_overlay_change(point_at_bol - 1))[0] )
    when "k"
      return remove_overlays
    end

    if Keys.prefix_u?
      over = make_overlay(*View.range)
      overlay_put over, :face, @@colors[char]
      return
    end

    # Otherwise, just colorize line
    self.colorize_line @@colors[char]
  end

  def self.colorize_line face
    over = make_overlay(Line.left, Line.right+1)
    overlay_put over, :face, face
  end

  # Define font
  def self.define_styles
    Styles.define :color_rb_red, :bg => "ffd5d5"
    Styles.define :color_rb_orange, :bg => "ffe5bb"
    Styles.define :color_rb_yellow, :bg => "f9f9aa"
    Styles.define :color_rb_green, :bg => "e0ffcc"
    Styles.define :color_rb_blue, :bg => "dde5ff"
    Styles.define :color_rb_purple, :bg => "f2ddff"

    Styles.define :color_rb_glow1, :bg => "ffcc99"
    Styles.define :color_rb_glow2, :bg => "ff9933"

    if Styles.inverse

      Styles.define :color_rb_red, :bg => "550000"
      Styles.define :color_rb_orange, :bg => "552200"
      Styles.define :color_rb_yellow, :bg => "4a4a00"
      Styles.define :color_rb_green, :bg => "004400"
      Styles.define :color_rb_blue, :bg => "000077"
      Styles.define :color_rb_purple, :bg => "330044"

    end

  end

end
Color.define_styles

# comment
