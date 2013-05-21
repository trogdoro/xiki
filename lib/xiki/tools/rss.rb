module Xiki
  # Gets mp3 url's from an rss feed
  class Rss
    def self.menu rss=nil
      # If no rss passed, tell them to provide one
      if rss.nil?
        return "- rss url - <put url here>"
      end

      # rss passed, so get result and pull out url's
      xml = Net::HTTP.get URI.parse(rss.sub(/.+?http/,'http'))
      urls = []
      xml.scan(/http:\/\/[\w.\/-]+\.mp3/) {|m| urls << m}
      urls.uniq.join("\n")
    end
  end
end
