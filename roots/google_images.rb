module Xiki
  class GoogleImages
    def self.menu *txt
      return View.prompt "Type something to google image search for" if txt.blank?
      txt = txt.join('/')
      txt = CGI.escape txt
      url = "https://www.google.com/search?tbm=isch&q=#{txt}"

      # If up+..., just return url
      return "@ #{url}" if Keys.up?

      Firefox.url url
    end
  end
end
