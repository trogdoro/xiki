module Xiki
  module Menu

    def self.init

      return if ! $el

      Mode.define(:menu, ".menu") do
        Xiki::Notes.mode
      end
      Mode.define(:xiki, ".xiki") do
        Xiki::Notes.mode
      end

    end

  end

  Menu.init   # Define mode
end
