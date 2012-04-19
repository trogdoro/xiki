class Edited
  def self.menu option=nil

    paths = Files.edited_array[0..300]

    # If "tree" option, show in tree form
    if option == "tree"
      txt = FileTree.paths_to_tree paths
      txt.gsub! /^- \//, "- @/"
      return txt
    end

    # Else, show in array form

    paths.map!{|i| i.sub(/(.+\/)(.+)/, "- @\\1\n  - \\2")}
    paths.join("\n")
  end
end
