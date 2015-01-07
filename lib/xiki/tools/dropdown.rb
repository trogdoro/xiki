require 'xiki/core/mode'

module Xiki
  # Makes text in .dropdown files huge, and makes left and right arrow keys treat
  # headings as slides.
  class Dropdown

    def self.init
      # Make dropdown mode happen for .dropdown files
      Mode.define(:dropdown, ".dropdown") do
        Notes.mode
      end
    end

  end
  Dropdown.init   # Define mode
end
