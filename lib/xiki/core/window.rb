# Deprecated after Unified.

module Xiki
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
        - .dotsies/
      - .scroll bars/
      "
    end

    # Move this into view.rb
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

  end
end
