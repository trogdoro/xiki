module Xiki
  class Conf

    # Use notes styles for .conf files
    def self.init
      Mode.define(:conf, ".conf") do
        Notes.mode
      end
    end

    # Saves by into ~/xiki/commands/conf/ by replacing
    # the line (copying the default conf over first, if
    # it's not there yet.
    # Keys.put "return warning", " 4"
    def self.put command, key, value

      # Read in file...

      user_conf = Bookmarks[":xh/commands/conf/#{command}.conf"]
      FileUtils.mkdir_p File.dirname user_conf   # In case it doesn't exist yet

      txt = File.read(user_conf) rescue nil

      # If not there, read from default...

      if ! txt
        txt = File.read(Bookmarks[":xiki/commands/#{command}/default.conf"]) rescue nil
      end

      # Update file accordingly

      result = txt.sub! /^(#{key}:).*/, "\\1 #{value}"

      # No replacement means it somehow wasn't in the file, so append to the end...

      txt << "\n#{key}: #{value}\n" if ! result

      # Write file...

      File.open(user_conf, "w") { |f| f << txt }

      nil
    end


    # Reads from ~/xiki/commands/conf/
    # Keys.get "xsh", "bottom bar"
    def self.get command, key
      txt = File.read Bookmarks[":xh/commands/conf/#{command}.conf"] rescue nil
      return nil if ! txt
      txt[/^#{key}: (.*)/, 1]   # => noob
    end


  end
  Conf.init   # Define mode
end
