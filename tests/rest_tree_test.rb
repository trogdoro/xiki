require 'test/unit'
$:.unshift File.expand_path("..")
require 'rest_tree'
require 'ol'

class RestTreeTest < Test::Unit::TestCase

  def test_handles?
    assert RestTree.handles?(['GET http://foo.com', 'hey'])
    assert ! RestTree.handles?(['HEY http://foo.com', 'hey'])
  end

  def test_extract_root_verb
    list = ['POST http://foo.com', 'PUT hey']
    assert_equal 'POST', RestTree.extract_root_verb(list)
    assert_equal ['http://foo.com', 'PUT hey'], list
  end

  def test_extract_verb
    line = 'POST hey'
    assert_equal 'POST', RestTree.extract_verb(line)
    assert_equal 'hey', line

    line = 'DELETE'
    assert_equal 'DELETE', RestTree.extract_verb(line)
    assert line.empty?   # Should be nothing left

    assert_equal nil, RestTree.extract_verb('hiya')
  end

  def test_launch_inner
    # Single line
    verb, path, body = RestTree.launch_inner ["PUT http://localhost:5984/foo/"], nil
    assert_equal ['PUT', 'http://localhost:5984/foo/', nil], [verb, path, body]

    # More path
    verb, path, body = RestTree.launch_inner ["GET http://localhost:5984/foo/", "bar/"], nil
    assert_equal ['GET', 'http://localhost:5984/foo/bar/', nil], [verb, path, body]

    # More path and verb
    verb, path, body = RestTree.launch_inner ['GET http://localhost:5984/foo/', 'bar/', 'POST'], nil
    assert_equal ['POST', 'http://localhost:5984/foo/bar/', nil], [verb, path, body]

    # More path, verb, and body
    verb, path, body = RestTree.launch_inner ['GET http://localhost:5984/foo/', 'bar/', 'POST ["txt"]'], nil
    assert_equal ['POST', 'http://localhost:5984/foo/bar/', '["txt"]'], [verb, path, body]
  end

  def test_launch_inner_with_children
    # TODO Just verb with children
    verb, path, body = RestTree.launch_inner ['GET http://localhost:5984/foo/', 'POST'], ['a', 'b']
    assert_equal ['POST', 'http://localhost:5984/foo/', "a\nb\n"], [verb, path, body]

    # TODO Quoted children (should be ignored)
    verb, path, body = RestTree.launch_inner ['GET http://localhost:5984/foo/', 'POST'], ['|a', '|b']
    assert_equal ['POST', 'http://localhost:5984/foo/', nil], [verb, path, body]


  end

  def test_remove_before_root
    # No root anywhere
    assert_equal [], RestTree.remove_before_root(["CodeTree.menu/"])
    assert_equal ["GET http://foo.com"], RestTree.remove_before_root(['GET http://foo.com'])
    assert_equal ["GET http://foo.com"], RestTree.remove_before_root(['crap', 'GET http://foo.com'])
    assert_equal ["GET http://localhost:5984/mem/"], RestTree.remove_before_root(["CodeTree.menu/", "CouchDb.menu/", ".rest/", "mem/", "GET http://localhost:5984/mem/"])

  end

end
