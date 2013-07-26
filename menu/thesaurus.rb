module Xiki
  class Thesaurus
    def self.menu word=nil
      return View.prompt('Type a word to look up') if word.nil?

      word.sub! /^\| /, ''

      word = CGI.escape word
      $el.browse_url "http://thesaurus.reference.com/browse/#{word}"
      nil
    end
  end
end
