module Xiki
  class Links
    def self.open_first
      url = View.txt[/http:\/\/.+/]
      $el.browse_url url
    end
  end
end
