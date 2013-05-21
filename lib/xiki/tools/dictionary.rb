module Xiki
  class Dictionary
    def self.menu word=nil
      return View.prompt('Type a word to look up') if word.nil?

      word = word.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      $el.browse_url "http://dictionary.reference.com/browse/#{word}"
      nil
    end
  end
end
