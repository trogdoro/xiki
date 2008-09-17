require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require 'line'
require 'core_ext'

class LineTest < Test::Unit::TestCase

  def test_without_label
    assert_equal "you", Line.without_label("- hey: you")
    assert_equal "", Line.without_label("- hey: ")
  end
end
