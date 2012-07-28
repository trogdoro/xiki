class Projects
  def self.menu
    "
    - This will never be called, because of projects.menu...
      - Think again of ways for .menu and .rb to work together creating menus
    - api/
    "
  end

  def self.current
    # If parent is dir, return it, else return first project
    dir = FileTree.handles?(Xiki.trunk[-2]) ? "#{Dir.pwd}/" : self.default_project
  end

  def self.default_project
    txt = File.read(File.expand_path "~/menus/projects.menu") rescue nil
    return nil if ! txt

    Line.without_label(:line=>txt[/.+/])
  end
end
