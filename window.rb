class Window

  def self.menu
    "
    - .dimensions/
      - presets/
      - @dimensions config/
      - .adjust/
        - position/
          - up/
          - down/
          - left/
          - right/
        - size/
          - wider/
          - narrower/
          - taller/
          - shorter/
      - current/
    - .visibility/
      - full/
      - high/
      - medium/
      - low/
      - .colorized/
      - .scroll bars/
    - .scroll bars/
    "
  end

  # Show dimension options, and invoke corresponding proc
  def self.dimensions action=nil, preset=nil

    if action == "current"
      # width, height, left, top
      return "#{View.width}, #{View.height}, #{$el.frame_parameter(nil, :left)}, #{$el.frame_parameter(nil, :top)}"
    end

    txt = File.read(File.expand_path("~/menus/dimensions_config.menu")).split("\n")

    # If just presets/, list all presets

    if ! preset
      return txt.map { |o|
        o = o.match(/(\w.+?)\/(.+)/)
        next if ! o
        "#{o[1]}--#{o[2]}"
        "- #{o[1]}/\n"
      }.join("")
    end

    # If preset passed, apply it

    View.kill if View.name == "@window/dimensions/presets/"

    txt = txt.find { |o| o =~ /#{preset}\// }
    txt = txt[/\/(.+)/, 1]

    # If just numbers and commas
    if txt =~ /^[0-9, ]+$/
      txt = txt.split(/, */).map{|o| o.to_i}
      View.dimensions_set *txt
      return
    end

    # Otherwise, it's code so just eval

    eval txt
    nil


    #     # TODO: use View.input :options=>@@dimension_options
    #     if key.nil?
    #       message = @@dimension_options.map{|i|
    #         "[#{i.first[/./]}]#{i.first[/.(.+)/,1]}"}.
    #         join(', ')
    #       key = Keys.input(:chars=>1, :prompt=>"dimensions: #{message}")
    #     end
    #     option = @@dimension_options.find{|i| i.first =~ /^#{key}/}
    #     return View.message("Option not #{key} found") if option.nil?
    #     option[1].call
  end

  def self.adjust type, direction

    if type == "size"
      case direction
      when "wider"; View.width += 1
      when "narrower"; View.width -= 1
      when "taller"; View.height += 1
      when "shorter"; View.height -= 1
      end
      #       View.height = View.height - 1

    elsif type == "wider"

    end

    nil

  end


  def self.visibility choice=nil
    choices = {
      'full'=>"(100 85)",
      'high'=>"(85 70)",
      'medium'=>"(70 55)",
      'low'=>"(40 30)",
      }

    numbers = choices[choice]
    raise ".flash - '#{choice}' isn't a valid choice for View.visibility!" if numbers.nil?

    $el.el4r_lisp_eval "(set-frame-parameter nil 'alpha '#{numbers})"   # full visibility
    View.kill if View.name == "@window/visibility/"
  end

  def self.colorized
    View.kill if View.file.nil?
    Styles.toggle
    nil
  end

  def self.scroll_bars
    result = $el.scroll_bar_mode
    View.width += View.scroll_bars ? -3 : 3
    #     View.scroll_bars = false
    View.kill if View.name == "@window/visibility/"
    nil
  end

end
