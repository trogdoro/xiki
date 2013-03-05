$:.unshift "spec/"
require 'xiki/ol'
require 'xiki/tree'
require 'xiki/core_ext'

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
    paths.should == [[["- a/"], "a/"], [["- a/", "- b/"], "a/b/"], [["- c/"], "c/"]]
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
      [["- a/"], "a/"],
      [["- a/", "- aa/"], "a/aa/"],
      [["- a/", "- aa2/"], "a/aa2/"],
      [["- a/", "- aa2/", "- aaa/"], "a/aa2/aaa/"],
      [["- c/"], "c/"]
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
      [["- hey) a/"], "hey) a/"],
      [["- hey) a/", "- you) b/"], "hey) a/you) b/"],
      [["- c/"], "c/"]
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
      [["a/"], "a/"], [["a/", "b/"], "a/b/"]
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
      [["a/"], "a/"],
      [[nil], ""],
      [["b/"], "b/"],
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
      [["a/"], "a/"],
      [["a/", "aa/"], "a/aa/"],
      [["a/", nil], "a/"],
      [["a/", "ab/"], "a/ab/"],
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
      [["a/"], "a/"],
      [["a/", "aa/"], "a/aa/"],
      [["a/", "aa/", "aaa/"], "a/aa/aaa/"],
      [["a/", "aa/", nil], "a/aa/"],
      [["a/", "aa/", "aab/"], "a/aa/aab/"],
      [["a/", nil], "a/"],
      [["a/", "ab/"], "a/ab/"],
      [["a/", "ab/", "aba/"], "a/ab/aba/"],
      [[nil], ""],
      [["b/"], "b/"],
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
end


describe Tree, "#dotify" do

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

  it "uses line from path when slash pipe and not on the pipe line" do
    Line.should_receive(:value).and_return "- red herring"
    Tree.leaf("aa/|b/b").should == "b/b"
  end
end

describe Tree, "#quote" do
  it "quotes normal lines" do
    Tree.quote("hey\nyou\n").should == "| hey\n| you\n".unindent
  end

  it "quotes blank lines" do
    Tree.quote("hey\n\nyou\n").should == "| hey\n|\n| you\n".unindent
  end

  it "quotes when indented" do
    Tree.quote("hey\n  you\n").should == "| hey\n|   you\n".unindent
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
#     assert_equal true, Tree.is_root?("  $tr/")
#     assert_equal true, Tree.is_root?("  $tr/##abc/")
#     #     assert_equal false, Tree.is_root?("  $tr")
#   end


describe Tree, "#children" do

  it "shows 2nd-level items when blank path" do
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

  it "includes all sub-items of items under at sign" do
    Tree.children("
      - @a/
        - .b/
          - c/
      ", "").should == "
      + @a/
        + b/
          + c/
      ".unindent
    Tree.children("
      - a/
        - @b/
          - c/
            - d/
      ", "a").should == "
      + @b/
        + c/
          + d/
      ".unindent
  end

  #   it "doesn't include items under item with at sign" do
  #     Tree.children("docs/\n  @red/\n  - herr/\n", "docs").should == "@red/\n"
  #   end

  it "includes all sub-items when :include_subitems option" do
    Tree.children("
      - a/
        - b/
      ", "", :include_subitems=>1).should == "
      + a/
        + b/
      ".unindent
  end

  it "includes all sub-items when no slashes" do
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
    Tree.target_match("a", ".a").should == nil
    Tree.target_match("aa/bb", ".aa/bb").should == nil
  end

  it "doesn't match when ends with substring" do
    Tree.target_match("aa", "a").should == nil
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

describe Tree, "#to_html" do
  it "handles one tag" do
    Tree.to_html("
      p/
        hi
      ".unindent).should == "
      <p>
        hi
      </p>
      ".unindent
  end

  it "handles single with no contents" do
    Tree.to_html("
      p/
      ".unindent).should == "
      <p>
      </p>
      ".unindent
  end

  #   it "doesn't close certain tags" do
  #     Tree.to_html("
  #       hr/
  #       ".unindent).should == "
  #       <p>
  #       </p>
  #       ".unindent
  #   end

  it "doesn't confuse comments" do
    Tree.to_html("
      p/
        /* hey */
      ".unindent).should == "
      <p>
        /* hey */
      </p>
      ".unindent
  end

  it "adds closing tags to html" do
    Tree.to_html("
      <p>
        hi
      ".unindent).should == "
      <p>
        hi
      </p>
      ".unindent
  end

  it "doesn't close comment tags" do
    Tree.to_html("
      <!-- hey -->
      hi
      ".unindent).should == "
      <!-- hey -->
      hi
      ".unindent
  end

end
