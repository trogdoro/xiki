require "cgi"

module Xiki::Menu
  class Google
    def self.menu *args

      if args.empty?   # If no path, pull from history
        return "@prompt/Type search string"
      end

      txt = args.join("/").strip
      txt.gsub! "\n", ' '
      txt = CGI.escape txt

      url = "http://www.google.com/search?q=#{txt}"

      Keys.prefix_u ?
        Browser.url(url):
        $el.browse_url(url)
      nil
    end

    def self.search txt
      txt = CGI.escape txt
      Browser.url "http://www.google.com/search?q=#{txt}"
      nil
    end

    def self.maps txt
      txt = CGI.escape txt
      Browser.url "http://maps.google.com/maps?q=#{txt}"
      nil
    end

  end
end
