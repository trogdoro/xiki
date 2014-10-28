module Xiki
  class Path

    # Removes |... from path and returns it
    # Only reliable when one quote.
    #
    # p Path.extract_quote "/projects/foo/bar.rb/| quoted"
    #   " quoted"
    def self.extract_quote path
      found = path =~ %r"/\|(.+)"   # Pipes followed by anything are quotes
      found ||= path =~ %r"/:([ +-].*)"   # Colons followed by only certain chars are quotes
      return if ! found

      path = path.slice!(found..-1).sub(/../, '')
      path.sub! %r".+/[|:]", ""   # Remove ancestor quotes if multiple

      path
    end

    def self.extract_line_number path
      found ||= path =~ %r"/:(\d+)\/?$"   # " Colons followed by only certain chars are quotes
      return if ! found

      path.slice!(found..-1).sub(/../, '').sub(/\/$/, '')
    end

    def self.escape! item
      item.gsub! ";", ";;"
      item.gsub! "/", ";/"
      item.sub! /^@/, ";@"   # At's only need escaping at the beginning
      item.gsub! "\n", ";0"
      item.gsub! /\A\| =/, "| ;="
      item   # Just in case they look at return val
    end

    def self.unescape! item
      result = ""

      # Go through each char, with flag for last_was_escape
      last_was_escape = false
      item.length.times do |i|
        c = item[i]
        if c == "0" && last_was_escape
          result.<< "\n"
        elsif c == ";"
          result.<< last_was_escape ? ";" : ""
        else
          result << c
        end

        last_was_escape = ! last_was_escape && c == ";"   # Only an escape if ; and the previous one wasn't
      end

      item.replace result
      result
    end

    # Path.escape "Hey\nyou"
    #   Hey;0you
    def self.escape item
      item = item.dup
      self.escape! item
      item
    end

    def self.unescape item
      item = item.dup
      self.unescape! item
      item
    end

    # Deprecated after Unified refactor.
    # Superceded by Path.split
    #
    # Menu.split("a/b").should == ["a", "b"]
    # Menu.split("a/b/@c/d/", :outer=>1).should == ["a/b/", "c/d/"]
    def self.split path, options={}

      return [] if path == ""

      result, last_was_escape = [""], false

      # If :outer, split based on "/="...

      if options[:outer]
        return self.split_outer path, options
      end

      # Else, split based on "/"...

      path.length.times do |i|
        c = path[i]

        # If slash and not escaped, make new item
        if c == "/" && ! last_was_escape
          is_last_char = i+1 == path.length
          result << "" if ! is_last_char || options[:trailing]
        elsif c == "0" && last_was_escape
          result[-1] << "\n"
        elsif c == ";"
          # This does escaping - should we do this only if an option??
            # It doesn't do escaping of \n etc
              # Maybe if :escape, delegate to self.escape, for less code duplication?
          result[-1].<< last_was_escape ? ";" : ""
        else
          result[-1] << c
        end
        last_was_escape = ! last_was_escape && c == ";"   # Only an escape if ; and the previous one wasn't
      end

      # Remove "| " from lines
      if options[:unquote]
        result.each{|o| o.sub!(/^\|./, '')}
      end

      if options[:return_path]
        return result[1..-1]
      end

      result

    end

    def self.split_outer path, options={}
      result, last_was_escape = [""], false

      i = 0
      while i < path.length
        c = path[i]
        cc = path[i, 2]

        # If /= and not escaped
        if cc =~ /\A\/=$/ && ! last_was_escape
          result[-1] << "/"
          result << ""
          i += 1
        else
          result[-1] << c
        end
        last_was_escape = ! last_was_escape && c == ";"   # Only an escape if ; and the previous one wasn't
        i += 1
      end

      # Remove = from beginning if there is one
      result[0].sub!(/\A= ?/, '') if result[0]

      result.each{|o| o.sub! /^ +/, ''}

      result
    end

    # Joins path array into a string, being sure to re-escape slashes
    def self.join array
      array.map{|o| Path.escape o}.join "/"
    end

  end
end
