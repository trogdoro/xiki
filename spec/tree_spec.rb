$:.unshift "spec/"
require 'el_mixin'
require 'tree'
require 'ol'
require 'core_ext'

describe Tree, "#traverse" do

  it "goes through each item" do
    paths = []
    tree = "
      - a/
        - b/
      - c/
      ".unindent

    Tree.traverse tree do |array|
      paths << array
    end
    paths.should == [["a/"], ["a/", "b/"], ["c/"]]
  end

  it "handles two-level dropoff and no dropoff" do
    paths = []
    tree = "
      - a/
        - aa/
        - aa2/
          - aaa/
      - c/
      ".unindent

    Tree.traverse tree do |array|
      paths << array
    end
    paths.should == [["a/"], ["a/", "aa/"], ["a/", "aa2/"], ["a/", "aa2/", "aaa/"], ["c/"]]
  end

  #   it "removes comments when :remove_comments" do
  #     paths = []
  #     tree = "
  #       - yack) a/
  #         - blab) b/
  #       - c/
  #       ".unindent

  #     Tree.traverse(tree, :remove_comments=>1) do |array|
  #       paths << array
  #     end
  #     paths.should == [["a/"], ["a/", "b/"], ["c/"]]
  #   end
end

describe Tree, "#routify" do
  it "returns children when no path" do
    tree = "
      - a/
        - aa/
        - aa2/
      - b/
      ".unindent

    Tree.routify!(tree, []).should == "- a/\n- b/\n"
  end

  it "returns children when one deep" do
    tree = "
      - a/
        - aa/
        - aa2/
      - b/
      ".unindent
    Tree.routify!(tree, ['a/']).should == "- aa/\n- aa2/\n"
    #     Tree.routify!(tree, "a/").should == "- aa/\n- aa2/\n"
  end

  it "adds dot to path when dot in tree" do
    tree = "
      - .a/
        - aa/
        - aa2/
      - b/
      ".unindent

    path = ["a"]
    Tree.routify!(tree, path).should == "- aa/\n- aa2/\n"
    path.should == [".a"]
  end

  it "doesn't include dot in output" do
    tree = "
      - a/
        - .aa/
        - .aa2/
      ".unindent

    list = ["a"]
    Tree.routify!(tree, list).should == "- aa/\n- aa2/\n"
    list.should == ["a"]
  end

  it "returns nil when no children" do
    tree = "
      - nice/
        - white
      - .delete/
      - .add/
        - sample/
      ".unindent

    path = ["delete"]
    Tree.routify!(tree, path).should == nil
    path.should == [".delete"]
  end

  it "passes param from .menu to action from .menu" do
    tree = "
      - .delete/
      - .add/
        - sample/
      ".unindent

    path = ["add", "sample"]
    Tree.routify!(tree, path).should == nil
    path.should == [".add", "sample"]
  end

  it "adds dot when path longer than tree" do
    tree = "
      - .keys/
      - .start/
      ".unindent

    path = ["keys", "a"]
    Tree.routify!(tree, path).should == nil
    path.should == [".keys", "a"]
  end

  it "returns items under star" do
    tree = "
      - .cold/
        - */
          - large
          - small
      - .hot/
      ".unindent

    target = ["cold", "lemonade"]
    Tree.routify!(tree, target).should == "- large\n- small\n"
    target.should == [".cold", "lemonade"]
  end

  it "finds item when comment" do
    tree = "
      - .js/
      - .roots/
        - @jsc/
        - examples) docs/
      ".unindent

    target = ["roots", "docs"]
    Tree.routify!(tree, target).should == nil
    target.should == [".roots", "docs"]
  end

end


describe Tree, "#route_match" do
  it "finds match of one" do
    Tree.route_match([".hot"], [".hot/"]).should == true
  end

  it "finds sublist match" do
    Tree.route_match([".hot", ".coffee"], [".hot/"]).should == true
  end

  it "finds match when multiple items" do
    Tree.route_match([".hot", ".coffee"], [".hot/", ".coffee/"]).should == true
  end

  it "doesn't find match when not all of target matched" do
    Tree.route_match([".hot"], [".hot/", ".coffee/"]).should == false
  end

  it "finds match when star" do
    Tree.route_match(["*"], [".hot/"]).should == true
  end

end


describe Tree, "#leaf" do
  it "returns last item" do
    Tree.leaf("aa/bb").should == "bb"
    Tree.leaf("bb").should == "bb"
    Tree.leaf("/aa/bb").should == "bb"
    Tree.leaf("/aa/bb/").should == "bb"
  end

  it "grabs siblings when pipe" do
    Line.should_receive(:value).and_return "| bb"
    Tree.should_receive(:siblings).any_number_of_times.and_return(["| aa", "| bb"])
    Tree.leaf("| bb").should == "aa\nbb\n"
  end

  it "grabs siblings when pipe" do
    Tree.leaf("aa/| bb", :dont_look=>1).should == "bb"
  end

  it "grabs siblings when slash after pipe" do
    Line.should_receive(:value).and_return "| b/b"
    Tree.should_receive(:siblings).any_number_of_times.and_return(["| aa", "| b/b"])
    Tree.leaf("| b/b").should == "aa\nb/b\n"
  end

  it "grabs siblings when slash pipe" do
    Line.should_receive(:value).and_return "| bb / hey"
    Tree.should_receive(:siblings).and_return ["| aa", "| bb / hey"]
    Tree.leaf("| bb / hey").should == "aa\nbb / hey\n"
  end

  it "uses line from path when slash pipe and not on the | line" do
    Line.should_receive(:value).and_return "- red herring"
    Tree.leaf("aa/|b/b").should == "b/b"
  end
end

describe Tree, "#quote" do
  it "leaves menus unquoted" do
    before = "
      > Using menus
      All menus can be used the same way.

      For more details, see:
      + @menu/docs/how_to_use/
      "
    after = "
      | > Using menus
      | All menus can be used the same way.
      |
      | For more details, see:
      + @menu/docs/how_to_use/
      ".unindent
    Tree.quote(before).should == after
  end
end

describe Tree, "#clear_empty_dirs!" do
  it "removes one empty dir" do
    result = Tree.clear_empty_dirs!("
      /projects/
        empty/
        full/
          database.rhtml
      ".unindent).join("\n")
    result.should =~ /full/
    result.should_not =~ /empty/
  end

  it "removes one empty dir when bullets" do
    result = Tree.clear_empty_dirs!("
      - /projects/
        + empty/
        full/
          database.rhtml
      ".unindent).join("\n")

    result.should =~ /full/
    result.should_not =~ /empty/
  end

  it "doesn't remove quoted lines" do
    result = Tree.clear_empty_dirs!("
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
    result = Tree.clear_empty_dirs!("
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
