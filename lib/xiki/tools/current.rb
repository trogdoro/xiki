module Xiki
  class Current
    def self.menu *name
      Buffers.current *name
    end
  end
end
