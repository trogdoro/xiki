module Xiki
  class Readme
    def self.menu *args
      txt = File.read "#{Bookmarks["%xiki"]}README.md"
      Notes.from_markdown_format txt
    end
  end
end
