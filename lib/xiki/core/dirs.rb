module Xiki
  class Dirs
    def self.commands
      @@commands ||= File.expand_path("~/xiki")
    end
  end
end

