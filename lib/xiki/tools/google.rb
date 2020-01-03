module Xiki
  class Google

    def self.search txt, options={}
      txt = CGI.escape txt
      url = "http://www.google.com/search?q=#{txt}"

      options[:via_os] ?
        $el.browse_url(url) :
        Browser.url(url)

      nil
    end

    def self.maps txt
      txt = CGI.escape txt
      Browser.url "http://maps.google.com/maps?q=#{txt}"
      nil
    end



  end
end
