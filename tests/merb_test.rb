require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require '../merb'
require '../core_ext'

class MerbTest < Test::Unit::TestCase

  def test_name_from_dir
    assert_equal "you", Merb.name_from_dir("/hey/you")
  end
end
