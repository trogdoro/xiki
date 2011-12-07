require 'styles'

# Makes visual things happen
class Effects
  extend ElMixin

  def self.menu
    "
    > Summary
    | Make text do things that look cool.
    | Try these out by double-clicking on them.
    |
    - Glow/
      @Effects.glow
      @Effects.glow :color=>:fire
      @Effects.glow :color=>:water
      @Effects.glow :color=>:forest
      @Effects.glow :color=>:rainbow
      @Effects.glow :color=>:fat_rainbow
    - Blink/
      | Makes line blink orange. Using a longer time since the blink happens
      | anyway.
      |
      @Effects.blink :time=>1
    - Some View methods that use effects/
      @View.prompt
      @View.flash
      @View.flash 'Saved!'
    "
  end

  def self.glow *args
    if args[0].is_a? Fixnum
      left, right = args.shift, args.shift
    else
      ignore, left, right = View.block_positions "^[|>]"
    end

    options = args[0] || {}

    times = options[:times] || 3

    over = $el.make_overlay left, right

    faces = if options[:color] == :fire
        ['change-log-function', 'change-log-function', 'change-log-file', 'change-log-file', 'change-log-date', 'change-log-date', 'change-log-date']
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

    sequence = options[:reverse] ?
      ([7] + (up + [1] + down + [7]) * times) :
      ([1] + (down + [7] + up + [1]) * times)
    sequence.each do |i|
      $el.overlay_put over, :face, (faces[i-1] || faces[0])
      $el.sit_for 0.02
    end

    $el.delete_overlay over
  end

  # Sample usages:
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
    over2 = make_overlay(left, right)
    overlay_put over2, :face, :color_rb_glow2
    sit_for time
    delete_overlay over2
  end

  # Define font
  def self.define_styles
    Styles.define :color_rb_glow1, :bg => "fec"
    Styles.define :color_rb_glow2, :bg => "f90"

    Styles.define :red, :fg => "f00"
    Styles.define :orange, :fg => "f80"
    Styles.define :yellow, :fg => "ff0"
    Styles.define :green, :fg => "0f0"
    Styles.define :blue, :fg => "00f"
    Styles.define :indigo, :fg => "408"
    Styles.define :violet, :fg => "82e"

    Styles.define :purple, :fg => "808"
    Styles.define :cyan, :fg => "f0f"
  end

end
Effects.define_styles
