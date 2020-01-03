module Xiki
  class Links
    def self.open_first
      url = View.txt[/http:\/\/.+/]
      $el.browse_url url
    end

    def self.to_nth_file n
      items = Xiki["files/", :go=>1].split("\n")
      item = items[n-1].sub(/^\+ /, '')
      Xiki["files", [item]]
    end

    def self.show_in_nav target_file, target_quote
      View.open "%links"
      View.to_top
      Search.forward $el.regexp_quote(target_file)
      if target_quote
        Search.forward $el.regexp_quote(target_quote), :beginning=>1
      end
    end
  end
end
