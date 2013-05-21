module Xiki
  class UrlTree
    def self.handles? list
      list.first =~ /^http:\/\// ? 1 : nil
    end

    def self.launch options={}
      url = options[:path].join("")
      Keys.prefix_u ? $el.browse_url(url) : Firefox.url(url)
    end
  end
end
