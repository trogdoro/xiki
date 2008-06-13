require 'styles'

# Makes visual things happen
class Effects
  extend ElMixin

  CODE_SAMPLES = %q[
    # Blink
    - whole file: Effects.blink :what => :all
  ]

  # Sample usages:
  def self.blink options={}
    what = options[:what]
    what ||= :line
    case what
    when :all
      left, right = View.top, View.bottom
    when :line
      left, right = Line.bounds
      right += 1
    when :sexp
      left, right = bounds_of_thing_at_point(:sexp).to_a
      return unless left
    end

    left = options[:left] if options[:left]
    right = options[:right] if options[:right]

    time = 0.04
    over2 = make_overlay(left, right)
    overlay_put over2, :face, :color_rb_glow2
    sit_for time
    delete_overlay over2
#     over1 = make_overlay(left, right)
#     overlay_put over1, :face, :color_rb_glow1
#     sit_for time
#     delete_overlay over1

#     over = make_overlay(left, right)
#     overlay_put over, :face, :font_running
#     sit_for 0.02
#     delete_overlay over

  end

  # Define font
  def self.define_styles

    Styles.define :color_rb_glow1,
      :bg => "fec"
    Styles.define :color_rb_glow2,
      :bg => "f90"

  end

end
Effects.define_styles
