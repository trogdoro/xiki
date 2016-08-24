module Xiki
  class Projects
    def self.before_menu
      nil
    end
    def self.after_menu
      nil
    end

    def self.menu
      "
      - This will never be called, because of projects.menu...
        - Think again of ways for .menu and .rb to work together creating menus
      - api/
      "
    end

    def self.default
      # If parent is dir, return it, else return first project
      dir = FileTree.handles?(Tree.path[-2]) ? "#{Dir.pwd}/" : self.default_project
    end

    def self.default_project
      txt = File.read(File.expand_path "~/.xiki/roots/projects.menu") rescue nil
      return nil if ! txt

      Line.without_label(:line=>txt[/.+/])
    end
  end
end
