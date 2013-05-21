module Xiki
  class Spanish
    def self.menu txt=nil
      return "| Type something here to translate from spanish" if txt.nil?

      Browser.url "http://translate.google.com/#es|en|#{URI.encode ENV['txt'].gsub(' ', '+')}"
    end
  end
end
