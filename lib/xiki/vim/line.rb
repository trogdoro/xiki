module Xiki
  class Line
    def self.number
      $curbuf.line_number
    end
    def self.value n=nil
      n = self.number if ! n
      $curbuf[n]
    end
  end
end
