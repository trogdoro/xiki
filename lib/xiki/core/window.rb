# Deprecated after Unified.

module Xiki
  class Window

    # Move this into view.rb
    def self.visible choice=nil
      choices = {
        'full'=>"(100 85)",
        #         'full'=>"(100 100)",   # For presentations
        'high'=>"(85 70)",
        'medium'=>"(70 55)",
        'low'=>"(40 30)",
        }

      numbers = choices[choice]
      raise ".flash - '#{choice}' isn't a valid choice for View.visible!" if numbers.nil?

      $el.el4r_lisp_eval "(set-frame-parameter nil 'alpha '#{numbers})"   # full visible
      View.kill if View.name == "@window/visible/"
    end

  end
end
