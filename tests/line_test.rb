require 'test/unit'
require 'el_mixin'
$:.unshift "../"
require 'line'
require 'core_ext'

class LineTest < Test::Unit::TestCase

  def test_normal
    assert_equal "you", Line.without_label(:line=>"- hey: you")
  end

  def test_just_bullet
    assert_equal "you", Line.without_label(:line=>"- you")
  end

  def test_blank
    assert_equal "", Line.without_label(:line=>"- hey: ")
  end

  def test_with_indent
    assert_equal "you", Line.without_label(:line=>"  - hey: you")
    assert_equal "", Line.without_label(:line=>"    - hey: ")
    assert_equal "you", Line.without_label(:line=>"    - you")
  end

  def test_leave_indent
    assert_equal "  you", Line.without_label(:line=>"  - hey: you", :leave_indent=>true)
    assert_equal "    ", Line.without_label(:line=>"    - hey: ", :leave_indent=>true)
  end

  # Make sure it doesn't mess up the string
  def test_side_effects
    before = "  - label: hey1"
    Line.without_label(:line=>before)
    assert_equal "  - label: hey1", before
  end

end
