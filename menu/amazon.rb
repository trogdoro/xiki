require 'xiki/view'
require 'xiki/effects'

class Amazon

  def self.menu line=nil
    # If no arg, prompt to type something

    return Keys.prompt "Type something to search on amazon" if line.nil?

    # If arg, look it up

    Firefox.url "http://www.amazon.com/s?field-keywords=#{line}"
    nil
  end
end
