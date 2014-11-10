module Xiki
  class Say
    def self.menu *args
      txt = args.join('/').gsub('"', '\"')
      Applescript.run "say \"#{txt}\""
    end
  end
end
