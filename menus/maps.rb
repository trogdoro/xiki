require 'view'
require 'effects'

class Maps

  def self.menu line=nil

    # If no arg, prompt to type something

    return View.prompt "Type something to search on google maps" if line.nil?

    # If arg, look it up

    Firefox.url "http://maps.google.com/maps?q=#{line}"
    nil

  end
end
