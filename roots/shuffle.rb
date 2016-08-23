module Xiki
  class Shuffle
    def self.menu *args

      message = "| This menu shuffles the siblings that follow it."
      return message if args == []

      orig = Location.new

      left, ignore, ignore2, right = Tree.sibling_bounds

      return message if left == right

      txt = View.delete left, right

      Code.randomize_lines txt

      orig.go
      View.deselect

      nil
    end
  end
end
