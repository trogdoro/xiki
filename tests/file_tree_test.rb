require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require 'file_tree'

class FileTreeTest < Test::Unit::TestCase

  def call_clear_empty_dirs tree
    FileTree.clear_empty_dirs_string tree.gsub(/^ *\|/, '')
  end

  # Should remove one empty dir
  def test_clear_empty_dirs!
    tree = call_clear_empty_dirs(
      "|/projects/
       |  empty/
       |  full/
       |    database.rhtml
       |")
    assert_no_match( /empty\//, tree )
    assert_match( /full\//, tree )

    tree = call_clear_empty_dirs(
      "|- /projects/
       |  + empty/
       |  - full/
       |    + database.rhtml
       |")
    assert_no_match( /empty\//, tree )
    assert_match( /full\//, tree )

  end

  # Shouldn't remove quoted lines
  def test_clear_empty_dirs_with_quotes!
    tree = call_clear_empty_dirs(
      "|- /projects/trunk/app/views/assets/details/
       |  + hey/
       |  - _database.rhtml
       |    |Database Type
       |    |Database Name
       |")
    assert_match( /Database Type/, tree )
    assert_match( /Database Name/, tree )
  end

  # Shouldn't remove quoted lines
  def test_clear_empty_dirs_with_quotes_with_slashes!
    tree = call_clear_empty_dirs(
      "|- /projects/trunk/app/views/assets/details/
       |  + hey/
       |  - _database.rhtml
       |    |Database Type/
       |    |Database Name/
       |")
    assert_match( /Database Type/, tree )
    assert_match( /Database Name/, tree )
  end



  def test_paths_to_tree
    paths = %w[
      /projects/foo/a.txt
      /projects/foo/b.txt
      /other/c.txt
      ]

    tree =
      "|- /other/
       |  + c.txt
       |- /projects/
       |  - foo/
       |    + a.txt
       |    + b.txt
       |".gsub(/^ *\|/, '')

# TODO: uncomment
    #assert_equal tree, FileTree.paths_to_tree(paths)
  end

  def test_acronym_regexp
    str = FileTree.acronym_regexp("mr")
    #puts str
    re = Regexp.new(str)
    assert "  mar_roon.txt" =~ re
    assert "  mar.rb" =~ re
    assert ! ("  mar,rb" =~ re)
    assert ! ("  mar_xu_rb" =~ re)
  end

  def test_search_dir_names
    tree =
     "  - /docs/
          emacs/
            elisp.notes
            todo/
              files.notes
        /projects/
          app/
            controllers/
              pages.rb
              pages2.rb
            helpers/
              pages_helper.rb
      ".gsub(/^      /, '').split("\n")

    after =
      "  /docs/
          emacs/
            todo/
              files.notes
        /projects/
          app/
            controllers/
            helpers/
      ".gsub(/^      /, '').split("\n")

# TODO
    #assert_equal after, FileTree.search_dir_names(tree, /todo/)
  end

  def test_search_dir_names_no_indent
    tree =
      "/docs/
        elisp.notes
      ".gsub(/^      /, '').split("\n")

    after =
      "/docs/
      ".gsub(/^      /, '').split("\n")

    assert_equal after, FileTree.search_dir_names(tree, /todo/)
  end

  def test_clean_path
    assert_equal '/bla/', FileTree.clean_path("- hey: /bla/")
    assert_equal 'bla/', FileTree.clean_path("- hey: bla/")
    assert_equal '/bla/', FileTree.clean_path("- /bla/")
    assert_equal 'bla/', FileTree.clean_path("+ bla/")
  end

  def test_is_root?
    assert_equal true, FileTree.is_root?("at left")
    assert_equal true, FileTree.is_root?("  - /hey")
    assert_equal true, FileTree.is_root?("  /hey")
    assert_equal true, FileTree.is_root?("  ./hey")
    assert_equal false, FileTree.is_root?("  .you")
    assert_equal true, FileTree.is_root?("  $tr/")
    assert_equal false, FileTree.is_root?("  $tr")
  end

end
