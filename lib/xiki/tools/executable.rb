module Xiki
  class Executable
    def self.menu

      # If not nested, error
      return "
        | Put '@executable' under a menu then run it again.  It should be under
        | the menu you want to create the executable for ou want to create the"

      trunk = Tree.path

      "
      - cake/
        - chocolate/
      "
    end
  end
end
