module Xiki
  class RandomMenu
    def self.menu *args
      dirs = ["~/menu/", "$xiki/menu/"]
      menus = []
      dirs.each do |dir|
        menus += Dir.new(Bookmarks[dir]).entries.grep(/\A[^.]/)
      end

      menu = menus.sort_by{ rand }[0]
      menu.gsub '_', ' '
      menu.sub! /\..*/, ''
      "@#{menu}/"
    end
  end
end
