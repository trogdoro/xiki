class Readme
  def self.menu *args
    txt = File.read "#{Bookmarks["$x"]}README.markdown"
    Notes.from_markdown_format txt
  end
end
