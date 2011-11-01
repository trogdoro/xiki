class UrlTree
  def self.handles? list
    list.first =~ /^http:\/\//
  end

  def self.launch options={}
    url = options[:path].join("")
    Keys.prefix_u ? $el.browse_url(url) : Firefox.url(url)
  end
end

Menu.urls do |path|
  url = path[/urls\/(.+)/, 1]

  if url.nil?
    next Launcher.last "urls", :exclude_path=>1
  end
  Keys.prefix == :u ? $el.browse_url(url) : Firefox.url(url)
  nil
end
