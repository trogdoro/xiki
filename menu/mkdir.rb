class Mkdir

  def self.menu
    trunk = Xiki.trunk
    return "
      > Usage
      | Nest @mkdir under a dir path to create the dir, like this:
      |
      | - /tmp/newdir/
      |   - @mkdir
      |
      " if trunk.length <= 1

    closest_file_path = Tree.closest_dir

    return View.flash("- Already exists: #{closest_file_path}") if File.exists?(closest_file_path)

    `mkdir -p "#{closest_file_path}"`

    return View.flash "- Created: #{closest_file_path}", :times=>3
  end

end
