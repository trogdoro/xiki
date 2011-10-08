class Contacts

  def self.menu heading=nil, body=nil

    if body
      return "- just experimenting :)!"
    end

    file = Bookmarks["$ko"]

    txt = File.read Bookmarks["$ko"]

    if heading
      txt.sub! /.*#{Regexp.escape heading}/m, ''
      txt.sub! /^\|.*/m, ''
      txt.strip!
      txt.gsub! /^/, '| '
      return txt
    end

    txt = txt.grep /^\|( |$)/

    txt.join('')#.gsub /^\|.*/, ''

  end
end
