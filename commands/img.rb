module Xiki
  class Img
    def self.menu *args
Ol "args", args

      path = args.join('/')

      column = Line.indent.length
      Line.sub! /^([ +-]*).*/, "\\1"
      Move.to_end

      at = column > 0 ? "=" : ""
Ol()
      Image.<< path, "#{at}img/#{path}"
Ol()
Ol "column", column
      Move.to_column column+1
# Move.to_column column
      nil
    end
  end
end
