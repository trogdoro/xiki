module Xiki
  class Head
    def self.menu *args

      count = args.shift if args[0] =~ /^\d+$/

      dir = Tree.dir :file=>1
      Tree.quote `head #{count ? "-n #{count}" : ""} "#{dir}"`

    end
  end
end
