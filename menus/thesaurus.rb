# - @http://thesaurus.com/browse/<here>

class Thesaurus
  # self.add :paren=>"th" do   # - (th): thesaurus.com
  def self.menu word=nil
    return View.prompt('Type a word to look up') if word.nil?

    word = word.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
    #     Browser.url "http://en.wiktionary.org/wiki/Wikisaurus:#{word}"
    $el.browse_url "http://thesaurus.reference.com/browse/#{word}"
    nil
  end
end
