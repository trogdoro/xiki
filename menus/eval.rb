class Eval
  def self.menu *args
    left = Line.right + 1
    ignore, ignore, right = View.block_positions "^>"

    Code.run :right=>right, :left=>left

    nil
  end
end

