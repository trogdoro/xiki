class MenuPath
  def self.menu *args
    dirs = Xiki.menu_path_dirs
    return dirs.map{|o| "+ @#{o.sub /\/*$/, '/'}"}.join("\n")

    menu_path = menu_path.split(":").map{|o| "+ @#{o.sub /\/*$/, '/'}"}.join("\n")
    menu_path
  end
end
