module Xiki
  class Classes
    def self.menu *args
      Xiki["ruby/classes/#{args.join '/'}"]
    end
  end
end
