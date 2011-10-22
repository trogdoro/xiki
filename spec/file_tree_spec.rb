$:.unshift "spec/"
require 'el_mixin'
require 'file_tree'
require 'core_ext'

describe FileTree, "#clear_empty_dirs!" do
  it "removes one empty dir" do
    result = FileTree.clear_empty_dirs!("
      /projects/
        empty/
        full/
          database.rhtml
      ".unindent).join("\n")
    result.should =~ /full/
    result.should_not =~ /empty/
  end

  it "removes one empty dir when bullets" do
    result = FileTree.clear_empty_dirs!("
      - /projects/
        + empty/
        full/
          database.rhtml
      ".unindent).join("\n")

    result.should =~ /full/
    result.should_not =~ /empty/
  end

  it "doesn't remove quoted lines" do
    result = FileTree.clear_empty_dirs!("
      - /projects/trunk/app/views/assets/details/
        + hey/
        - _database.rhtml
          |Database Type
          |Database Name/
      ".unindent).join("\n")

    result.should =~ /Database Type/
    result.should =~ /Database Name/
  end

  it "removes files without quotes" do
    result = FileTree.clear_empty_dirs!("
      - /projects/trunk/
        - hey.html
        - you.html
          | Database Type
      ".unindent, :quotes=>true).join("\n")

    result.should =~ /trunk/
    result.should =~ /you/
    result.should_not =~ /hey/
  end

end
