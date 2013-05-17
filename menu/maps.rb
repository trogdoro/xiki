require 'xiki/view'
require 'xiki/effects'

class Maps
  def self.menu txt=nil

    # If no arg, prompt to type something

    return View.prompt "Type something to search on google maps" if ! txt

    # If arg, look it up

    Firefox.url "http://maps.google.com/maps?q=#{txt.gsub "\n", ", "}"
    nil
  end
end
