module Xiki
  class GoogleImages
    def self.menu *txt
      return View.prompt "Type something to google image search for" if txt.blank?
      url = "https://www.google.com/search?tbm=isch&q=#{txt.join('/')}"

      # If up+..., just return url
      return "@ #{url}" if Keys.up?

      Firefox.url url
    end
  end
end
