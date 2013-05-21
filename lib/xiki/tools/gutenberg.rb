module Xiki
  class Gutenberg
    def self.menu *args
      arg = args.any? ? args.join("/") : nil

      return View.prompt('Type a search phrase') if arg.nil?

      arg.sub! /^\| ?/, ''
      return View.prompt "Type something to google gutenberg search for" if arg.nil?

      url = "http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=downloads&query=#{CGI.escape arg}"
      Browser.url url
    end
  end
end
