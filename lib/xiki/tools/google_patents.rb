class GooglePatents
  def self.menu *args

    return View.prompt "Type something to google patent search for" if args.blank?
    txt = ENV['txt']

    url = "https://www.google.com/search?tbm=pts&q=#{CGI.escape txt}"
    Browser.url url
  end
end
