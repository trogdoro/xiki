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
    #     paths.should == [["a"], ["a", "aa"], ["a", "aa2"], ["a", "aa2", "aaa"], ["c"]]
    paths.should == [["a/"], ["a/", "aa/"], ["a/", "aa2/"], ["a/", "aa2/", "aaa/"], ["c/"]]
  end

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

end


# describe Tree, "#check_route_match" do
#   it "finds exact match" do
#     target = ["a"]
#     Tree.check_route_match!(["a/"], target).should == true
#     target.should == ["a"]
#   end
# end

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

  it "doens't find match when not all of target matched" do
    Tree.route_match([".hot"], [".hot/", ".coffee/"]).should == false
  end

  it "finds match when star" do
    Tree.route_match(["*"], [".hot/"]).should == true
  end

end
