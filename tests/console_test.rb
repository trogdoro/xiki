require 'test/unit'
$:.unshift File.expand_path("..")
require 'console'
require 'ol'
class CodeTreeTest < Test::Unit::TestCase

  def test_extract_method
    assert_equal("ssh foo@foo.org", Console.ssh_line("/foo@foo.org/"))
    assert_equal("ssh -p 1234 foo@foo.org", Console.ssh_line("foo@foo.org:1234"))
  end
end
