class Twitter
  def self.menu word=nil
    return View.prompt('Type something to search on twitter') if word.nil?

    word = word.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
    $el.browse_url "http://search.twitter.com/search?q=#{word}"
    nil
  end
end
