class Shuffle
  def self.menu *args

    message = "| This menu shuffles the siblings that follow it."
    return message if args.any?

    orig = Location.new

    ignore, ignore2, left, right = Tree.sibling_bounds

    return message if left == right

    txt = View.txt left, right
    View.delete left, right

    Line.next

    Code.randomize_lines txt

    orig.go

    nil
  end
end
