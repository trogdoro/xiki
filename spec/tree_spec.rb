$:.unshift "spec/"
require './spec/spec_helper'

%w"tree path view xik".each {|o| require "xiki/core/#{o}"}

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
    paths.should == [
      [["- a/"], "a/", 1],
      [["- a/", "- b/"], "a/b/", 2],
      [["- c/"], "c/", 3],
    ]
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

    paths.should ==[
      [["- a/"], "a/", 1],
      [["- a/", "- aa/"], "a/aa/", 2],
      [["- a/", "- aa2/"], "a/aa2/", 3],
      [["- a/", "- aa2/", "- aaa/"], "a/aa2/aaa/", 4],
      [["- c/"], "c/", 5],
    ]

  end

  it "leaves in comments" do
    paths = []
    tree = "
      - hey) a/
        - you) b/
      - c/
      ".unindent

    Tree.traverse tree do |array|
      paths << array
    end
    paths.should == [
      [["- hey) a/"], "hey) a/", 1],
      [["- hey) a/", "- you) b/"], "hey) a/you) b/", 2],
      [["- c/"], "c/", 3],
    ]
  end

  it "removes bullets" do
    paths = []
    tree = "
      - a/
        + b/
      ".unindent

    Tree.traverse tree, :no_bullets=>1 do |array|
      paths << array
    end
    paths.should == [
      [["a/"], "a/", 1],
      [["a/", "b/"], "a/b/", 2],
    ]
  end

  it "passes blanks as nil" do
    paths = []
    tree = "
      - a/

      - b/
      ".unindent

    Tree.traverse tree, :no_bullets=>1 do |array|
      paths << array
    end

    paths.should == [
      [["a/"], "a/", 1],
      [[nil], "", 2],
      [["b/"], "b/", 3],
    ]
  end

  it "passes blanks as nil one level down" do
    paths = []
    tree = "
      - a/
        - aa/

        - ab/
      ".unindent

    Tree.traverse tree, :no_bullets=>1 do |array|
      paths << array
    end
    paths.should == [
      [["a/"], "a/", 1],
      [["a/", "aa/"], "a/aa/", 2],
      [["a/", nil], "a/", 3],
      [["a/", "ab/"], "a/ab/", 4],
    ]
  end

  it "when blanks, use indent of next line" do
    paths = []
    tree = "
      a/
        aa/
          aaa/

          aab/

        ab/
          aba/

      b/
      ".unindent

    Tree.traverse tree, :no_bullets=>1 do |array|
      paths << array
    end
    paths.should == [
      [["a/"], "a/", 1],
      [["a/", "aa/"], "a/aa/", 2],
      [["a/", "aa/", "aaa/"], "a/aa/aaa/", 3],
      [["a/", "aa/", nil], "a/aa/", 4],
      [["a/", "aa/", "aab/"], "a/aa/aab/", 5],
      [["a/", nil], "a/", 6],
      [["a/", "ab/"], "a/ab/", 7],
      [["a/", "ab/", "aba/"], "a/ab/aba/", 8],
      [[nil], "", 9],
      [["b/"], "b/", 10],
    ]
  end


  it "error when blank line between parent and child" do

    # For simplicity, we're just erroring, as opposed to looking ahead
    # to see what the assumed indent would be.
    #
    # It could be possible to assume that the indent is one lower if
    # there's a child following.  But, since we're not allowing space
    # between a parent and child, this may not be necessary.

    paths = []
    tree = "
      - a/

        - aa/
      ".unindent

    lambda {
      Tree.traverse tree, :no_bullets=>1 do |array|
        paths << array
      end
    }.should raise_error(RuntimeError)

  end

  it "adds slashes when none" do
    paths = []
    tree = "
      a
        b
      c
      ".unindent

    Tree.traverse tree do |array|
      paths << array
    end
    paths.should == [
      [["a"], "a", 1],
      [["a", "b"], "a/b", 2],
      [["c"], "c", 3],
    ]
  end



end


# Old distructive way, before Unified refactor.
# TODO: port these tests over to describe #dotify below.
#   (first few are already done.)
describe Tree, "#dotify!" do
  it "adds dot to path when dot in tree" do
    tree = "
      - .a/
        - aa/
      - b/
      ".unindent

    path = ["a"]
    Tree.dotify! tree, path
    path.should == [".a"]
  end

  it "adds dot one level deep" do
    tree = "
      - sample/
        - .delete/
      ".unindent

    path = ["sample", "delete"]
    Tree.dotify!(tree, path)
    path.should == ["sample", ".delete"]
  end

  it "adds dots at multiple levels" do
    tree = "
      - .sample/
        - .delete/
      ".unindent

    path = ["sample", "delete"]
    Tree.dotify!(tree, path)
    path.should == [".sample", ".delete"]
  end

  it "adds dot when same name as other leaf" do
    tree = "
      - .delete/
        - sample/
      - .add/
        - sample/
      ".unindent

    path = ["add", "sample"]
    Tree.dotify!(tree, path)
    path.should == [".add", "sample"]
  end

  it "doesn't add dot when already there" do
    tree = "
      - .a/
      - b/
      ".unindent

    path = [".a"]
    Tree.dotify! tree, path
    path.should == [".a"]
  end

  it "adds dot when path longer than tree" do
    tree = "
      - .keys/
      - .start/
      ".unindent

    path = ["keys", "a"]
    Tree.dotify!(tree, path)
    path.should == [".keys", "a"]
  end

  it "shouldn't modify initial tree" do
    tree = "
      hi
      there
      ".unindent

    orig = tree.dup

    target = ["roots", "docs"]
    Tree.dotify!(tree, target)
    target.should == ["roots", "docs"]

    tree.should == orig
  end

  it "adds dot to path when arrow bullets" do
    tree = "
      <= .a/
        - aa/
      ".unindent

    path = ["a"]
    Tree.dotify! tree, path
    path.should == [".a"]
  end
end

describe Tree, "#dotify" do
  it "dot in tree" do
    tree = "
      - .a/
        - aa/
      - b/
      ".unindent

    path = ["a"]
    Tree.dotify(tree, path).should == [true]
    path.should == ["a"]
  end

  it "dot one level deep" do
    tree = "
      - sample/
        - .delete/
      ".unindent

    path = ["sample", "delete"]
    Tree.dotify(tree, path).should == [nil, true]
    path.should == ["sample", "delete"]
  end

  it "dots at multiple levels" do
    tree = "
      - .sample/
        - .delete/
      ".unindent

    path = ["sample", "delete"]
    Tree.dotify(tree, path).should == [true, true]
    path.should == ["sample", "delete"]
  end

  it "dot in 2nd item" do
    tree = "
      - sample/
      - .delete/
      ".unindent

    path = ["delete"]
    Tree.dotify(tree, path).should == [true]
  end

  it "works when a space" do
    tree = "
      - .delete it/
      ".unindent

    path = ["delete it"]
    Tree.dotify(tree, path).should == [true]
  end

  it "works when a underscore and space" do
    tree = "
      - .delete_it/
      ".unindent

    path = ["delete it"]
    Tree.dotify(tree, path).should == [true]
  end

  it "works when in reverse and a underscore and space" do
    tree = "
      - .delete it/
      ".unindent

    path = ["delete_it"]
    Tree.dotify(tree, path).should == [true]
  end

  it "works when a case is different" do
    tree = "
      - .Delete it/
      ".unindent

    path = ["delete_it"]
    Tree.dotify(tree, path).should == [true]
  end

  it "works when a hyphen and space" do
    tree = "
      - hey-you there/
      ".unindent

    path = ["hey-you-there"]
    Tree.dotify(tree, path).should == [true]
  end

  it "matches when star" do
    tree = "
      - */
        - .bb/
      ".unindent

    path = ["aa/bb/"]
    Tree.dotify(tree, path).should == [nil, true]
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
    mock(Line).value {"| bb"}
    stub(Tree).siblings {["| aa", "| bb"]}
    Tree.leaf("| bb").should == "aa\nbb\n-----"
  end

  it "grabs siblings when pipe" do
    Tree.leaf("aa/| bb", :dont_look=>1).should == "bb"
  end

  it "grabs siblings when slash after pipe" do
    mock(Line).value {"| b/b"}
    stub(Tree).siblings {["| aa", "| b/b"]}
    Tree.leaf("| b/b").should == "aa\nb/b\n"
  end

  it "grabs siblings when slash pipe" do
    mock(Line).value {"| bb / hey"}
    stub(Tree).siblings {["| aa", "| bb / hey"]}
    Tree.leaf("| bb / hey").should == "aa\nbb / hey\n"
  end

  it "uses line from path when slash pipe and not on the pipe line" do
    mock(Line).value {"- red herring"}
    Tree.leaf("aa/|b/b").should == "b/b"
  end
end

describe Tree, "#quote" do
  it "quotes normal lines" do
    Tree.quote("hey\nyou\n").should == ": hey\n: you\n".unindent
  end

  it "quotes blank lines" do
    Tree.quote("hey\n\nyou\n").should == ": hey\n:\n: you\n".unindent
  end

  it "quotes when indented" do
    Tree.quote("hey\n  you\n").should == ": hey\n:   you\n".unindent
  end

  #   it "leaves menus unquoted" do

  # TODO Where am I using Tree.quote?

  #     before = "
  #       > Using menus
  #       All menus can be used the same way.

  #       For more details, see:
  #       + @menu/docs/how_to_use/
  #       "
  #     Tree.quote(before).should == "
  #       > Using menus
  #       | All menus can be used the same way.
  #       |
  #       | For more details, see:
  #       + @menu/docs/how_to_use/
  #       ".unindent
  #   end

end


describe Tree, "#clear_empty_dirs!" do
  it "removes one empty dir when no bullets" do
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
          : Database Type
          : Database Name/
      ".unindent).join("\n")

    result.should =~ /Database Type/
    result.should =~ /Database Name/
  end

  it "removes files without quotes" do
    result = Tree.clear_empty_dirs!("
      - /projects/trunk/
        - hey.html
        - you.html
          : Database Type
      ".unindent, :quotes=>true).join("\n")

    result.should =~ /trunk/
    result.should =~ /you/
    result.should_not =~ /hey/
  end

  it "leaves paths starting with equals" do
    result = Tree.clear_empty_dirs!("
      nav history//xiki/search.rb/
        =/projects/xiki/lib/xiki/core/
        =/projects/xiki/lib/xiki/core/
          - search.rb
            :     def self.enter txt=nil
      ".unindent, :quotes=>true).join("\n")

  end
end


describe Tree, "#restore" do

  it "copies missing children when removed" do
    treea = "
      - m/
      - n/
      ".unindent
    treeb = "
      - m/
        - mm/
        - mm2/
      ".unindent

    Tree.restore(treea, treeb).should == "
      - m/
        - mm/
        - mm2/
      - n/
      ".unindent
  end
end


describe Tree, "#paths_to_tree" do

  # Uncomment if we mess with paths_to_tree again

  #   it "maintains trailing slashes" do
  #     paths = ["/projects/"]

  #     txt = Tree.paths_to_tree(paths)
  #     txt.should == "- /projects/\n"
  #   end

  #   it "builds a tree" do
  #     paths = %w[
  #       /projects/foo/a.txt
  #       /projects/foo/b.txt
  #       /other/c.txt
  #       ]

  #     tree = "
  #       - /other/
  #         + c.txt
  #       - /projects/
  #         - foo/
  #           + a.txt
  #           + b.txt
  #       ".unindent

  #     txt = Tree.paths_to_tree(paths)
  #     txt.should == tree
  #   end

  #   it "handles menus" do
  #     paths = %w[
  #       Tree/
  #       Tree/api/
  #       dotsies/apply/all views/
  #       dotsies/apply/one view/
  #       files/
  #       ]

  #     tree = "
  #       - Tree/
  #         + api
  #       - dotsies/
  #         - apply/
  #           + all views
  #           + one view
  #       - files/
  #       ".unindent

  #     txt = Tree.paths_to_tree(paths)
  #     txt.should == tree
  #   end

end




# TODO 2011-11-11: turn these into specs!



# describe Tree, "#acronym_regexp" do
#   it "makes regex's that work" do
# ...

#   def test_acronym_regexp
#     str = Tree.acronym_regexp("mr")
#     #puts str
#     re = Regexp.new(str)
#     assert "  mar_roon.txt" =~ re
#     assert "  mar.rb" =~ re
#     assert ! ("  mar,rb" =~ re)
#     assert ! ("  mar_xu_rb" =~ re)
#   end

#   def test_search_dir_names
#     tree =
#      "  - /docs/
#           emacs/
#             elisp.notes
#             todo/
#               files.notes
#         /projects/
#           app/
#             controllers/
#               pages.rb
#               pages2.rb
#             helpers/
#               pages_helper.rb
#       ".gsub(/^      /, '').split("\n")

#     after =
#       "  /docs/
#           emacs/
#             todo/
#               files.notes
#         /projects/
#           app/
#             controllers/
#             helpers/
#       ".gsub(/^      /, '').split("\n")

#     # TODO
#     #assert_equal after, Tree.search_dir_names(tree, /todo/)
#   end

#   def test_search_dir_names_no_indent
#     tree =
#       "/docs/
#         elisp.notes
#       ".gsub(/^      /, '').split("\n")

#     after =
#       "/docs/
#       ".gsub(/^      /, '').split("\n")

#     assert_equal after, Tree.search_dir_names(tree, /todo/)
#   end

#   def test_clean_path
#     assert_equal '/bla/', Tree.clean_path("- hey: /bla/")
#     assert_equal 'bla/', Tree.clean_path("- hey: bla/")
#     assert_equal '/bla/', Tree.clean_path("- /bla/")
#     assert_equal 'bla/', Tree.clean_path("+ bla/")
#   end

#   def test_is_root?
#     assert_equal true, Tree.is_root?("at left")
#     assert_equal true, Tree.is_root?("  - /hey")
#     assert_equal true, Tree.is_root?("  /hey")
#     assert_equal true, Tree.is_root?("  ./hey")
#     assert_equal false, Tree.is_root?("  .you")
#     assert_equal true, Tree.is_root?("  :tr/")
#     assert_equal true, Tree.is_root?("  :tr/##abc/")
#     #     assert_equal false, Tree.is_root?("  :tr")
#   end


describe Tree, "#children" do

  it "shows top items when blank path" do
    Tree.children("
      - a/
      - .b/
      ", "").should == "
      + a/
      + b/
      ".unindent
  end

  it "shows children of 1 deep" do
    Tree.children("
      - a/
        - aa/
        - ab/
      - b/
      ", "a").should == "
      + aa/
      + ab/
      ".unindent
  end

  it "converts dashes to pluses for items ending in slash" do
    Tree.children("
      - a/
        - aa/
        - ab
      - b/
      ", "a").should == "
      + aa/
      - ab
      ".unindent
  end

  it "returns nil when no match" do
    Tree.children("
      - a/
        - aa/
        - ab/
      - b/
      ", "x").should == nil
  end

  it "allows blank lines" do
    result = Tree.children "
      - a/
        aa

        ab
      ", "a"
    result.should == "
      aa

      ab
      ".unindent
  end

  it "includes blank lines in root" do
    result = Tree.children "
      Hey

      you
      ", ""
    result.should == "
      Hey

      you
      ".unindent
  end

  it "doesn't show dots" do
    Tree.children("
      - .a/
      - b/
      ", "").should == "
      + a/
      + b/
      ".unindent
  end

  it "doesn't show dots when arrow bullets" do
    Tree.children("
      <= .a/
      - b/
      ", "").should == "
      <= a/
      + b/
      ".unindent
  end

  it "leaves trailing spaces for quoted lines" do
    Tree.children("
      - a/
        | b 
        | c
      ", "a").should == "
      | b 
      | c
      ".unindent
  end

  it "returns nil if child is star" do
    Tree.children("
      - a/
        - */
      ", "a").should == nil
  end

  it "matches when star" do
    Tree.children("
      - a/
        - */
          - aaa/
      ", "a/z").should == "+ aaa/\n"
  end

  it "matches when root is star" do
    Tree.children("
      - */
        - bb/
      ", "a/").should == "+ bb/\n"
  end

  it "includes all sub-items of items under equals sign" do
    Tree.children("
      - =a/
        - .b/
          - c/
      ", "").should == "
      + =a/
        + b/
          + c/
      ".unindent
  end

  it "includes all sub-items of items under equals" do
    Tree.children("
      - =a/
        - .b/
          - c/
      ", "").should == "
      + =a/
        + b/
          + c/
      ".unindent
  end

  it "includes all sub-items when :include_subitems option" do
    Tree.children("
      - a/
        - b/
      ", "", :include_subitems=>1).should == "
      + a/
        + b/
      ".unindent
  end

  it "includes all items when no slashes and getting root" do
    Tree.children("
      - a
        - b

        - c
      ", "").should == "
      - a
        - b

        - c
      ".unindent
  end

  it "includes all sub-items when no slashes" do
    Tree.children("
      - a
        - b
          - c

          - d
      ", "a").should == "
      - b
        - c

        - d
      ".unindent
  end

  it "doesn't include sub-items of children with plus at beginning" do
    Tree.children("
      + a
        + b
          - c

          - d
      ", "a").should == "+ b".unindent
  end

  it "doesn't misinterpret blank lines as children" do
    result = Tree.children "
      - a/

      - b/
      ", "a"
    result.should == nil
  end

  it "isn't confused by blank lines a level deeper" do
    result = Tree.children "
      a/
        aa

        ab
      b/
      ", ""
    result.should == "
      a/
      b/
      ".unindent
  end

  it "associates blank lines at end with root" do
    result = Tree.children "
      a/
        aa

      b/
      ", ""
    result.should == "
      a/

      b/
      ".unindent
  end

  it "returns nil when nothing under item" do
    Tree.children("
      - a/
        - b/
      ", "a/b/").should == nil
  end

  it "returns nil when path too deep" do
    Tree.children("
      - a/
        - b/
      ", "a/b/c/").should == nil
  end

  it "returns exclamations" do
    Tree.children("
      - a/
        ! b
        ! c
      ", "a/").should == "! b\n! c\n"
  end

  it "returns path and exclamations when deeper path but exclamations" do
    options = {}
    Tree.children("
      - a/
        ! b
        ! c
      ", "a/arg", options).should == "! b\n! c\n"
    options[:exclamations_args].should == ["arg"]
  end

  it "returns empty args and exclamations when path to exclamations" do
    options = {}
    Tree.children("
      - a/
        ! b
        ! c
      ", "a", options).should == "! b\n! c\n"
    options[:exclamations_args].should == nil
  end

  it "returns only exclamations under one item" do
    options = {}
    Tree.children("
      - a/
        ! aa
      - b/
        ! bb
      ", "a", options).should == "! aa\n"
    options[:exclamations_args].should == nil
  end

  it "doesn't treat exclamations as special when :exclamations_normal" do
    options = {:exclamations_normal=>1}
    Tree.children("
      - a/
        ! b
        ! c
      ", "a/arg", options).should == nil
    options[:exclamations_args].should == nil
  end

  it "doesn't mess up when subpath and exclamations" do
    Tree.children("
      - right presentation/
        ! aa
      - right/
        ! bb
      ", ["right presentation"]).should == "! aa\n"
  end



  it "returns child of colon quoted items" do
    Tree.children("
        : a
          : aa
        : b
          : ba
        ", [": a"]).should == ": aa\n"
  end

  it "returns just one level of colon quoted items" do
    Tree.children("
        : a
          : aa
            : aaa
        : b
        ", [": a"]).should == ": aa\n"
  end

  it "finds child when hyphens" do
  - what about when > file paths?"
      Tree.children("
        - foo-bar bah/
          - ram/
        ", "foo-bar-bah").should == "+ ram/\n"
    end

end

describe Tree, "#children_old" do

  before(:all) do
    @tree = "
       - .aa
       - .bb/
         - .bb1/
           - bb11
         - .bb2
       - cc/
         - .cc1/
         - .cc2
       - dd/
         | > Heading
         | Underneath.
       - ee/
         - ee2/
           | > Heading
           | Underneath.
       - .ff
       ".unindent
  end

  # TODO put more thought into how to mock this
  # No idea how it worked when it was in AutoMenu

  before(:each) do
    Tree.should_receive(:child).and_return "foo"
    #     Line.should_receive(:indent).and_return "  "
    Line.should_receive(:value).with(2).and_return "  hey"
  end

  #   it "finds children of root" do
  #     Tree.children(@tree, nil).should == "- .aa\n- .bb/\n- cc/\n- dd/\n- ee/\n- .ff\n"
  #   end

  #   it "finds two children" do
  #     Tree.children(@tree, "/cc/").should == "- .cc1/\n- .cc2\n"
  #   end

  #   it "finds children 2 levels in" do
  #     Tree.children(@tree, "bb/bb1").should == "- bb11\n"
  #   end

  #   it "finds children that have children" do
  #     Tree.children(@tree, "bb").should == "- .bb1/\n- .bb2\n"
  #   end

  #   it "finds children when no period" do
  #     Tree.children(@tree, "cc").should == "- .cc1/\n- .cc2\n"
  #   end

  #   it "finds nothing when no children" do
  #     Tree.children(@tree, "aa").should == ""
  #   end

  #   it "finds nothing when no match" do
  #     Tree.children(@tree, "xx").should == ""
  #   end

  #   it "shows quoted children" do
  #     Tree.children(@tree, "/dd/").should == "| > Heading\n| Underneath.\n"
  #   end

  #   it "shows quoted children 2 levels in" do
  #     Tree.children(@tree, "/ee/ee2/").should == "| > Heading\n| Underneath.\n"
  #   end

end


describe Tree, "#target_match" do

  it "matches when blanks" do
    Tree.target_match("", "a").should == :longer
    Tree.target_match("a", "").should == :shorter
    Tree.target_match("", "").should == :same
  end

  it "matches when no dots" do
    Tree.target_match("a", "a").should == :same
    Tree.target_match("a/", "a/").should == :same
    Tree.target_match("aa/bb", "aa/bb").should == :same
  end

  it "matches when both dots" do
    Tree.target_match(".a", ".a").should == :same
    Tree.target_match(".a/", ".a/").should == :same
    Tree.target_match(".aa/.bb", ".aa/.bb").should == :same
  end

  it "matches when tree has dot" do
    Tree.target_match(".a", "a").should == :same
    Tree.target_match(".aa/bb", "aa/bb").should == :same
  end

  it "doesn't match when path has dot" do
    Tree.target_match("a", ".a").should == 0
    Tree.target_match("aa/bb", ".aa/bb").should == 0
  end

  it "doesn't match when ends with substring" do
    Tree.target_match("aa", "a").should == 0
  end

  it "matches when target is longer" do
    Tree.target_match("a/b", "a/b/c").should == :longer
  end

  it "recognizes longer when path has dot" do
    Tree.target_match(".sample/", "sample/delete").should == :longer
  end

  it "matches when target is shorter" do
    Tree.target_match("a/b", "a").should == :shorter
  end

  it "recognizes same when one has path" do
    Tree.target_match("a/", "a").should == :same
  end

  it "recognizes match when star" do
    Tree.target_match("a/*/aa/", "a/zzzz").should == :shorter
  end

  it "return the count of items that did match upon failure" do
    Tree.target_match("a/b/", "a/z/").should == 1
    Tree.target_match("a/b/c/", "a/b/z/").should == 2
  end

  it "doesn't confuse when partial match" do
    Tree.target_match("right/! bb", "right presentation").should == 0
  end

end


describe Tree, "#add_pluses_and_minuses" do

  it "adds bullets" do
    txt = "aa\n  bb\n"
    Tree.add_pluses_and_minuses(txt)
    txt.should == "+ aa\n  + bb\n"
  end

  it "doesn't add redundantly" do
    txt = "- aa\n  - bb\n"
    Tree.add_pluses_and_minuses(txt)
    txt.should == "- aa\n  - bb\n"
  end

end


describe Tree, "#construct_path" do
  before :each do
    $el = Object.new
    stub($el).point.times(1) {100}
  end

  it "handles one line" do
    # Simulates:
    # a/

    mock(Line).value.times(1) {"a/"}
    mock($el).goto_char(100)

    Tree.construct_path.should == "a/"
  end

  it "handles nesting one level" do
    # Simulates:
    # a/
    #   b/

    mock(Line).value.times(1) {"  b/"}
    mock(Line).value.times(1) {"a/"}
    mock($el).search_backward_regexp(anything)

    mock($el).goto_char(100)
    Tree.construct_path.should == "a/b/"
  end

  it "adds slash when last slash is escaped" do
    # Simulates:
    # a/
    #   b/

    mock(Line).value.times(1) {"  b/"}
    mock(Line).value.times(1) {": a/"}
    mock($el).search_backward_regexp(anything)

    mock($el).goto_char(100)
    Tree.construct_path.should == "a/b/"
  end


  it "stops when stop sign" do
    # Simulates:
    # a/
    #   =b/

    mock(Line).value.times(1) {"  =b/"}
    mock($el).goto_char(100)
    Tree.construct_path.should == "b/"
  end

  it "returns list" do
    # Simulates:
    # a/
    #   b/

    mock(Line).value.times(1) {"  b/"}
    mock(Line).value.times(1) {"a/"}
    mock($el).search_backward_regexp(anything)

    mock($el).goto_char(100)
    Tree.construct_path(:list=>1).should == ["a/", "b/"]
  end

  it "returns a list when stop" do
    # Simulates:
    # a/
    #   =b/

    mock(Line).value.times(1) {"  =b/"}
    mock($el).search_backward_regexp(anything)
    mock(Line).value.times(1) {"a/"}
    mock($el).goto_char(100)

    Tree.construct_path(:all=>1, :list=>1).should == ["a/", "=b/"]
  end

  it "handles when stop, slashes and list" do
    # Simulates:
    # a/
    #   =b/

    mock(Line).value.times(1) {"  =b/"}
    mock($el).search_backward_regexp(anything)
    mock(Line).value.times(1) {"a/"}
    mock($el).goto_char(100)

    Tree.construct_path(:all=>1, :slashes=>1).should == "a/=b/"
  end

  it "leaves double slashes" do
    # What should it return when this?...
    # a//
    #   =b/

    mock(Line).value.times(1) {"  =b/"}
    mock($el).search_backward_regexp(anything)
    mock(Line).value.times(1) {"a//"}
    mock($el).goto_char(100)

    Tree.construct_path(:all=>1, :slashes=>1).should == "a//=b/"

  end

  it "returns new raw format that menus can get via yield" do
    $el.point
    pending "Don't know what it should look like"


    # What should it return when this?...
    # a/
    #   b/@c/
    #     d/
  end








  it "appends hyphen to root when no slash" do
    # Simulates:
    # a

    mock(Line).value.times(1) {"a"}
    mock($el).goto_char(100)

    Tree.construct_path.should == "a-"
  end

  it "appends hyphen to multiple roots when no slash" do
    # Simulates:
    # a
    #   = b

    mock(Line).value.times(1) {"  =b"}
    mock($el).search_backward_regexp(anything)
    mock(Line).value.times(1) {"a"}
    mock($el).goto_char(100)

    Tree.construct_path(:all=>1, :list=>1).should == ["a-", "=b-"]
  end




end

describe Tree, "#join_to_subpaths" do

  it "leaves list boundaries only for equals signs" do
    Tree.join_to_subpaths(["a/", "=b/", "c/"]).should == ["a/", "b/c/"]
  end

  it "doesn't split escaped equals sign" do
    Tree.join_to_subpaths(["a/", "b/;=ip/"]).should == ["a/b/;=ip/"]
  end

  it "doesn't split equals sign after escaped slash" do
    Tree.join_to_subpaths(["a/", "b;/=ip/"]).should == ["a/b;/=ip/"]
  end

  it "doesn't split equals sign after escaped slash" do
    Tree.join_to_subpaths(["a/", "b;/=ip/"]).should == ["a/b;/=ip/"]
  end

  it "handles file path quotes" do
    Tree.join_to_subpaths(["/foo/", "path.rb", "| def bar"]).should == ["/foo/path.rb/| def bar"]
  end

  it "doesn't turn blank items into slashes" do
    Tree.join_to_subpaths(["", "ip/"]).should == ["ip/"]
  end

  it "merges single = without slashes" do
    Tree.join_to_subpaths(["=", "ip/"]).should == ["ip/"]
  end

  it "splits when shell prompt" do
    Tree.join_to_subpaths(["/dir/path/", "$ foo"]).should == ["/dir/path/", "$ foo"]
  end

  #   it "tests a bunch of other stuff, once we're comfortable with making this the official way of delimiting @'s and dealing with trailing slashes"

    # Paths we should handle:
    # ["a/", "b/"].should == ["a/b/"]
    # ["a/b/", "c/"].should == ["a/b/c/"]
    # ["a/@b/"].should == ["a/", "b/"]
    # ["/tmp/@b/"].should == ["/tmp/", "b/"]
    # ["/tmp@b/"].should == ["/tmp", "b/"]
    # ["a", "b", "@c/", "d/", "p Tree.path options={}"]

    # Shouldn't split these:
    # ["a@b/"]

  it "adds slash delimiter even when escaped slash at end" do
    Tree.join_to_subpaths(["a;/", "b"]).should == ["a;//b"]
  end

end

describe Tree, "#add_slashes_except_last" do
  it "doesn't add slash to last" do
    path = ["a", "b"]
    Tree.add_slashes_except_last path
    path.should == ["a/", "b"]
  end

  it "always adds slashes redundantly by default" do
    # After unified, should probably remove this behavior, and make :only_if_needed not required
    path = ["a/", "b"]
    Tree.add_slashes_except_last path
    path.should == ["a//", "b"]
  end

  it "doesn't add redundantly when :only_if_needed" do
    # After unified, should probably remove this behavior, and make :only_if_needed not required
    path = ["a/", "b"]
    Tree.add_slashes_except_last path, :only_if_needed=>1
    path.should == ["a/", "b"]
  end

  it "doesn't add slashes to blanks when :leave_blanks" do
    path = ["", ""]
    Tree.add_slashes_except_last path, :leave_blanks=>1
    path.should == ["", ""]
  end

  it "adds slash when last is escaped" do
    path = ["a;/", "b"]
    Tree.add_slashes_except_last path, :only_if_needed=>1
    path.should == ["a;//", "b"]
  end

end

describe Tree, "#update" do
  it "updates one item" do
    txt = "
      a/
        b
      c/
      ".unindent
    Tree.update(txt, ["a", "XX"]).should == "
      a/
        XX
      c/
      ".unindent
  end

  it "updates when bullets" do
    txt = "
      - a/
        - b
      - c/
      ".unindent
    Tree.update(txt, ["a", "XX"]).should == "
      - a/
        XX
      - c/
      ".unindent
  end

  it "updates multiple nested lines" do
    txt = "
      z/
        a/
          b
          c
      c/
      ".unindent
    Tree.update(txt, ["a", "XX\nYY"]).should == "
      z/
        a/
          XX
          YY
      c/
      ".unindent
  end

  it "returns when no match" do
    txt = "a/"
    Tree.update(txt, ["z", "XX"]).should == "a/"
  end

end


describe Tree, "#matching_siblings_start_index" do
  it "finds start" do
    txt = "
      * c/
      a/
        - b/
      * c/
        - b/
      ".unindent.gsub(/^/, "  ")
    Tree.matching_siblings_start_index(txt, "* ").should == 21
  end

  it "finds when no indent" do
    txt = "
      a/
        - b/
      * c/
      ".unindent
    Tree.matching_siblings_start_index(txt, "* ").should == 10
  end

  it "returns 0 when not all match" do
    txt = "
      * a/
        - b/
      * c/
      ".unindent
    Tree.matching_siblings_start_index(txt, "* ").should == 0
  end

  it "returns length of all when no match" do
    txt = "
      a/
        - b/
      c/
      ".unindent
    Tree.matching_siblings_start_index(txt, "* ").should == 13
  end

  it "returns length of all when no match" do
    txt = "  - hi\n  * bbb\n  - hi\n  * you\n  * you\n"
    Tree.matching_siblings_start_index(txt, "* ").should == 22
  end

end

describe Tree, "#matching_siblings_end_index" do
  it "finds nonmatching" do
    txt = "
      * a/
        - b/
      c/
      ".unindent.gsub(/^/, "  ")
    Tree.matching_siblings_end_index(txt, "* ").should == 16
  end

  it "finds when no indent" do
    txt = "
      * a/
        - b/
      c/
      ".unindent
    Tree.matching_siblings_end_index(txt, "* ").should == 12
  end

  it "returns nil when not all match" do
    txt = "
      * a/
        - b/
      * c/
      ".unindent
    Tree.matching_siblings_end_index(txt, "* ").should == nil
  end

  it "returns 0 when none match" do
    txt = "
      a/
        - b/
      c/
      ".unindent
    Tree.matching_siblings_end_index(txt, "* ").should == 0
  end
end
