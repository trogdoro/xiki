class Readme
  def self.menu *args
    txt = File.read "#{Bookmarks["$x"]}README.markdown"
    Markdown.to_xiki_format txt
  end
end
