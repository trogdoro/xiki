class Eval
  def self.menu *args

    # If any args, just eval them
    if args.any?
      return eval args.join("/")
    end

    left = Line.right + 1
    ignore, ignore, right = View.block_positions "^>"

    Code.run :right=>right, :left=>left

    nil
  end
end

