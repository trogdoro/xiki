require 'xiki/core/mode'

module Xiki
  # Makes text in .task files huge, and makes left and right arrow keys treat
  # headings as slides.
  class Task

    def self.init
      # Make task mode happen for .task files
      Mode.define(:task, ".task") do
        Notes.mode
      end
    end

  end
  Task.init   # Define mode
end
