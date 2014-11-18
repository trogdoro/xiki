module Xiki
  class WebThesaurus
    def self.menu word=nil
      return View.prompt('Type a word to look up') if word.nil?

      word.strip!
      word.sub! /^: /, ''
      word.gsub! /\n/, ' '
      word = CGI.escape word

      $el.browse_url "http://thesaurus.reference.com/browse/#{word}"
      nil
    end
  end
end
