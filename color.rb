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
    "e" => :color_rb_green, "b" => :color_rb_blue, "u" => :color_rb_purple,
    "l" => :color_rb_light,
  }

  def self.colorize char=nil
    char ||= Keys.input(:one_char => true, :prompt => 'Enter first letter of color: ')
    char = char.to_s

    # If h, just show all colors
    case char
    when "c"   # Copy marked lines

      overlays = overlays_in(View.top, View.bottom)   # Get all overlays
      res = ""
      overlays.to_a.reverse.each do |o|   # Loop through and copy all
        line = View.txt(overlay_start(o), overlay_end(o))
        line << "\n" unless line =~ /\n$/
        res << line
      end
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
      pos = next_overlay_change(View.cursor)
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
      return remove_overlays
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

  # Define font
  def self.define_styles
    Styles.define :color_rb_red, :bg => "ffd5d5"
    Styles.define :color_rb_orange, :bg => "ffe5bb"
    Styles.define :color_rb_yellow, :bg => "f9f9aa"
    Styles.define :color_rb_green, :bg => "e0ffcc"
    Styles.define :color_rb_light, :bg => "222222"
    Styles.define :color_rb_blue, :bg => "dde5ff"
    Styles.define :color_rb_purple, :bg => "f2ddff"

    Styles.define :color_rb_glow1, :bg => "ffcc99"
    Styles.define :color_rb_glow2, :bg => "ff9933"

    if Styles.inverse

      Styles.define :color_rb_red, :bg => "390000"
      Styles.define :color_rb_orange, :bg => "441500"
      Styles.define :color_rb_yellow, :bg => "333300"
      Styles.define :color_rb_green, :bg => "002200"
      Styles.define :color_rb_blue, :bg => "000055"
      Styles.define :color_rb_purple, :bg => "220033"

    end

  end

end
Color.define_styles

