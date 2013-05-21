module Xiki
  class French
    def self.menu txt=nil
      return "| Type something here to translate from french" if txt.nil?
      url = "http://translate.google.com/#fr|en|#{CGI.escape ENV['txt']}"
      Browser.url url
    end
  end
end
