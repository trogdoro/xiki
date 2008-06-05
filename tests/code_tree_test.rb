require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require '../code_tree'

class CodeTreeTest < Test::Unit::TestCase

  def test_extract_method
    assert_equal("you", CodeTree.extract_method("Hey.you"))
    assert_equal("you", CodeTree.extract_method(".you"))
    #assert_equal(nil, CodeTree.extract_method("hey.you"))
    assert_equal("menu('hey')", CodeTree.extract_method("Wiki.menu('hey')"))
    assert_equal("pages('new')", CodeTree.extract_method(".pages('new')"))
    assert_equal("menu('hey')", CodeTree.extract_method("Wiki1.menu('hey')"))
    assert_equal("merb_local", CodeTree.extract_method("- .merb_local"))
    assert_equal("dir 'f.com:21'", CodeTree.extract_method("- Remote.dir 'f.com:21'"))

  end

  def test_extract_class
    assert_equal("Wiki1", CodeTree.extract_class("Wiki1.menu('hey')"))
  end

  # Should return literal string for single path
  def test_determine_code_from_path
    code, data = CodeTree.determine_code_from_path(["- p Mem.menu"])
    assert_equal("p Mem.menu", code)  # Only one line, so runs it with no modification
  end

  # Should return literal string even if has parent
  def test_two_code_nodes
    code = CodeTree.determine_code_from_path([
      "CodeTree.menu",
      "- p HeyYou.menu"])
    assert_equal("p HeyYou.menu", code)
  end

  # Should return whole thing for single path
  def test_single_node
    code = CodeTree.determine_code_from_path([
      "- Wiki.menu('hey')"])
    assert_equal("Wiki.menu('hey')", code)
  end

  # Should get class from parent
  def test_class_from_parent
    code = CodeTree.determine_code_from_path([
      "- Wiki.menu('hey')",
      ".pages('new')"])
    assert_equal("Wiki.pages('new')", code)
  end

  # Should ignore parent when child is root
  def test_ignore_parent
    code = CodeTree.determine_code_from_path([
      "- Foo.menu('hey')",
      "- Wiki.pages('new')"])
    assert_equal("Wiki.pages('new')", code)
  end

  # Should pass data node to parent method
  def test_parent_data_node
    code = CodeTree.determine_code_from_path([
      "- Wiki.menu('hey')",
      "foo"])
    assert_equal("Wiki.menu('hey', \"foo\")", code)
  end

  # Should handle when no params in method
  def test_parent_no_params
    code = CodeTree.determine_code_from_path([
      "- Wiki.menu()",
      "foo"])
    assert_equal("Wiki.menu(\"foo\")", code)
  end

  # Should pass parent data node to method
  def test_child_data_node
    code = CodeTree.determine_code_from_path([
      "- Wiki.ignore()",
      "foo",
      ".menu()"])
    assert_equal("Wiki.menu(\"foo\")", code)
  end

  # Should pass 2 descendant nodes as params
  def test_two_descendant_nodes
    code = CodeTree.determine_code_from_path([
      "- Wiki.menu()",
      "foo",
      "bar"])
    assert_equal('Wiki.menu("foo", "bar")', code)
  end

  # Should append 2 descendant nodes as params
  def test_append_two_descendant_nodes
    code = CodeTree.determine_code_from_path([
      '- Wiki.menu("first")',
      "foo",
      "bar"])
    assert_equal('Wiki.menu("first", "foo", "bar")', code)
  end

  # Should pass 2 ancestor nodes as params
  def test_two_ancestor_nodes
    code = CodeTree.determine_code_from_path([
      "- Red.herring",
      "- Wiki.ignore",
      "foo",
      "bar",
      ".menu()"])
    assert_equal('Wiki.menu("foo", "bar")', code)
  end

  # Should handle commas in a data node
  def test_commas
    code = CodeTree.determine_code_from_path([
      "- Wiki.menu()",
      "foo, bar"])
    assert_equal('Wiki.menu("foo", "bar")', code)
  end

  # Should handle commas in ancestors
  def test_ancestors_commas
    code = CodeTree.determine_code_from_path([
      "- Wiki.no",
      "foo, bar",
      ".menu()"])
    assert_equal('Wiki.menu("foo", "bar")', code)
  end

  # Should handle: data, method, data
  def test_data_method_data
    code = CodeTree.determine_code_from_path([
      "- Wiki.no",
      "foo",
      ".menu()",
      "bar"])
    assert_equal('Wiki.menu("foo", "bar")', code)
  end

  # Should handle: data, method(param), data
  def test_data_methodwithparam_data
    code = CodeTree.determine_code_from_path([
      "- Wiki.no",
      "foo",
      '.menu("fir")',
      "bar"])
    assert_equal('Wiki.menu("fir", "foo", "bar")', code)
  end

  # Should handle: data, method(param), data
  def test_labels
    code = CodeTree.determine_code_from_path([
      "- Wiki.no",
      "foo",
      '.menu("fir")'])
    assert_equal('Wiki.menu("fir", "foo")', code)
  end

  # Should handle commas in ancestors
  def test_period_in_data
    code = CodeTree.determine_code_from_path([
      "- Wiki.yes",
      "foo.no"])
    assert_equal('Wiki.yes("foo.no")', code)
  end

  # Should pass 2 ancestor nodes as params
  def test_data_node_that_looks_like_root
    code = CodeTree.determine_code_from_path([
      "- Wiki.bla",
      "Red.herring"])
    assert_equal('Wiki.bla("Red.herring")', code)
  end

  # Should pass 2 ancestor nodes as params
  def test_two_classes_with_bullets
    code = CodeTree.determine_code_from_path([
      "- CodeTree.menu",
      "- Moo.menu",
      "- .merb_local"])
    assert_equal('Moo.merb_local()', code)
  end

  # Should pass 2 ancestor nodes as params
  def test_class_with_param
    code = CodeTree.determine_code_from_path([
      "- try:",
      "- CodeTree.menu",
      "- Moo.menu",
      "- .php",
      "- Remote.dir 'cmuth@Moo.com:21/tmp'",
      "approved/"])
    assert_equal('Remote.dir( \'cmuth@Moo.com:21/tmp\', "approved/")', code)
  end

  # Should pass 2 ancestor nodes as params
  def test_class_with_param
    code = CodeTree.determine_code_from_path([
      "- try:",
      "- CodeTree.menu",
      "- Moo.menu",
      "- .php",
      "- Remote.dir 'cmuth@Moo.com:21/tmp'",
      "approved/",
      "Spanish.txt"])
    assert_equal('Remote.dir( \'cmuth@Moo.com:21/tmp\', "approved/", "Spanish.txt")', code)
  end

#   # Should return literal string even if has parent
#   def test_astrixes
#     code = CodeTree.determine_code_from_path([
#       "CodeTree.menu",
#       "- *hi"])
#     assert_equal('CodeTree.menu("*hi")', code)

#     code = CodeTree.determine_code_from_path([
#       "CodeTree.menu",
#       "- **hi"])
#     assert_equal('CodeTree.menu("**hi")', code)
#   end

end
