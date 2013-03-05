require 'xiki/view'
require 'xiki/effects'

class Map
  def self.menu *args

    # If no arg, prompt to type something

    return View.prompt "Type something to search on google maps" if args.blank?

    # If arg, look it up

    Firefox.url "http://maps.google.com/maps?q=#{ENV['txt'].gsub "\n", ", "}"
    nil
  end
end
