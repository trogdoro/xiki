require 'xiki/core/view'
require 'xiki/core/effects'

module Xiki
  class Amazon

    def self.menu line=nil
      # If no arg, prompt to type something

      return Keys.prompt "Type something to search on amazon" if line.nil?

      # If arg, look it up

      line = CGI.escape line

      Firefox.url "http://www.amazon.com/s?field-keywords=#{line}"
      nil
    end
  end
end
