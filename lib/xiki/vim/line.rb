module Xiki
  class Line
    def self.number
      $curbuf.line_number
    end
    def self.value n=1
      n = self.number if ! n
      $curbuf[n]
    end
  end
end
