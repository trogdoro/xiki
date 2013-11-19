module Xiki::Menu
  class Window

    MENU = "
      - .visible/
        - full/
        - high/
        - medium/
        - low/
        - .colorized/
        - .notes mode/
        - .scroll bars/
        - .dotsies/
        - @white/
        - @black/
      - .scroll bars/
      "

    # Show dimension options, and invoke corresponding proc
    def self.dimensions action=nil, preset=nil

      if action == "current"
        # width, height, left, top
        return "#{View.frame_width}, #{View.frame_height}, #{$el.frame_parameter(nil, :left)}, #{$el.frame_parameter(nil, :top)}"
      end


      txt = File.read(File.expand_path("~/menu/dimensions_config.menu")) rescue nil
      txt ||= File.read(File.expand_path("#{Xiki.dir}menu/dimensions_config.menu"))

      txt.sub! /\n\n.+/m, "\n"

      txt = txt.split("\n")

      # Any after blank line will be ignored


      # If just presets/, list all presets

      if ! preset
        txt = txt.map { |o|
          o = o.match(/(\w.+?)\/(.+)/)
          next if ! o
          "#{o[1]}--#{o[2]}"
          "- #{o[1]}/\n"
        }.join("")
        return "#{txt}@dimensions config/\n"
      end

      # If preset passed, apply it

      View.kill :force_recent=>1 if View.name == "@window/dimensions/presets/"
      txt = txt.find { |o| o =~ /^[ +-]*#{preset}\// }

      txt = txt[/\/(.+)/, 1]
      self.dimensions_set txt

      nil

    end

    def self.dimensions_set txt

      # If just numbers and commas
      if txt =~ /^[0-9, ]+$/
        txt = txt.split(/, */).map{|o| o.to_i}
        View.dimensions_set *txt
        return
      end

      # Otherwise, it's code so just eval

      eval txt
    end


    def self.adjust type, direction
      if type == "size"
        case direction
        when "wider"; View.frame_width += 1
        when "narrower"; View.frame_width -= 1
        when "taller"; View.frame_height += 1
        when "shorter"; View.frame_height -= 1
        end

      elsif type == "wider"

      end

      nil
    end


    def self.visible choice=nil
      Xiki::Window.visible choice
    end

    def self.colorized
      View.kill :force_recent=>1 if View.file.nil?
      Styles.toggle
      nil
    end

    def self.notes_mode
      View.kill :force_recent=>1 if View.file.nil?
      Xiki::Notes.mode
      nil
    end

    def self.scroll_bars
      visible = View.scroll_bars

      View.scroll_bars = ! visible
      View.frame_width += visible ? 3 : -3
      View.kill :force_recent=>1 if View.name == "@window/visible/"
      nil
    end

    def self.dotsies
      Styles.font_size 120
      Xiki["dotsies/apply/all views/"]
    end

end; end
