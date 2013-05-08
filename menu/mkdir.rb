class Mkdir

  def self.menu

    dir = Tree.closest_dir yield[:ancestors]

    return "
      > Usage
      | Nest @mkdir under a dir path to create the dir, like this:
      |
      | - /tmp/newdir/
      |   - @mkdir
      |
      " if ! dir

    return View.flash("- Already exists: #{dir}") if File.exists?(dir)

    `mkdir -p "#{dir}"`

    return View.flash "- Created: #{dir}", :times=>3
  end

end
