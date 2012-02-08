class Img
  def self.menu *args

    path = args.join('/')

    column = Line.indent.length
    Line.sub! /^([ +-]*).*/, "\\1"
    Move.to_end

    Image.<< path, "img/#{path}"
    Move.to_column column
    nil
  end
end
