module Xiki
  class GoogleImages
    def self.menu *args
      return View.prompt "Type something to google image search for" if args.blank?
      url = "https://www.google.com/search?tbm=isch&q=#{CGI.escape ENV['txt']}"

      # If up+..., just return url
      return "@ #{url}" if Keys.up?

      Firefox.url url
    end
  end
end
