module Xiki
  class Classes
    def self.menu *args
      Menu["ruby/classes/#{args.join '/'}"]
    end
  end
end
