module Xiki
  class Conf

    @@cache = {}

    # Use notes styles for .conf files
    def self.init
      Mode.define(:conf, ".conf") do
        Notes.mode
      end
    end

    # Saves by into ~/xiki/roots/conf/ by replacing
    # the line (copying the default conf over first, if
    # it's not there yet.
    # Keys.put "the command", "the key", "4"
    def self.set command, key, value

      cache_key = "#{command}/#{key}"
      @@cache[cache_key] = value

      # Read in file...

      user_conf = File.expand_path("~/.xiki/roots/conf/#{command}.conf")
      FileUtils.mkdir_p File.dirname user_conf   # In case it doesn't exist yet

      txt = File.read(user_conf) rescue nil

      # If not there, read from default...

      if ! txt
        txt = File.read(File.expand_path("~/.xiki/roots/#{command}/default.conf")) rescue nil
      end

      return if ! txt

      # Update file accordingly

      result = txt.sub! /^(#{key}:).*/, "\\1 #{value}"

      # No replacement means it somehow wasn't in the file, so append to the end...

      txt << "\n#{key}: #{value}\n" if ! result

      # Write file...

      File.open(user_conf, "w") { |f| f << txt }

      nil
    end


    # Reads from ~/xiki/roots/conf/
    # Keys.get "xsh", "bottom bar"
    def self.get command, key
      txt = File.read(File.expand_path("~/.xiki/roots/conf/#{command}.conf")) rescue nil
      return nil if ! txt
      txt[/^#{key}: (.*)/, 1]   # => noob
    end

    def self.get_cached command, key

      cache_key = "#{command}/#{key}"

      # Cached value exists, so just return it
      value = @@cache[cache_key]
      return value if value != nil

      # No cached value, so get from file and set cache

      value = self.get command, key

      @@cache[cache_key] = value

      value

    end

  end
  Conf.init   # Define mode
end
