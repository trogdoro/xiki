require 'auto_menu'

class DataMapper
  extend AutoMenu

  def self.auto_menu
    puts "
      - .docs_links/
        - index: http://datamapper.org/docs/
        - finding: http://datamapper.org/docs/find.html
        - saving: http://datamapper.org/docs/create_and_destroy.html
        - api: http://datamapper.rubyforge.org/
      "
  end
end
