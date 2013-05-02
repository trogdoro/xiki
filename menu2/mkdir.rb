class Mkdir

  def self.menu
    dir = yield[:ancestors][-1]

    return "
      > Usage
      | Nest @mkdir under a dir path to create the dir, like this:
      |
      | - /tmp/newdir/
      |   - @mkdir
      |
      " if ! dir

    closest_file_path = dir

    # TODO: update .closest_dir to use Tree.path_unified, and use it. (Try with @git under non-existent dir)
    #     closest_file_path = Tree.closest_dir

    return View.flash("- Already exists: #{closest_file_path}") if File.exists?(closest_file_path)

    `mkdir -p "#{closest_file_path}"`

    return View.flash "- Created: #{closest_file_path}", :times=>3
  end

end
