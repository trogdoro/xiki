module Xiki
  class Command

    # Tells you whether a command exists.
    # path to the command but with no extension.
    #
    # Command.exists? "/tmp/git//"
    # Command.exists? "/tmp/git"
    def self.exists? path

      # Check if dir exists

      return true if File.exists? path
      return true if Dir["#{path}.*"].any?

      nil   # Doesn't exist
    end

  end
end
