$:.unshift "spec/"
require 'el_mixin'
require 'line'
require 'core_ext'

describe Line, "#without_label" do
  it "removes bullets" do
    Line.without_label(:line=>"- you").should == "you"
  end

  it "should remove new parethesis labels" do
    Line.without_label(:line=>"- hey) you").should == "you"
  end

  it "should keep labels if opening paren" do
    Line.without_label(:line=>"- (hey) you").should == "(hey) you"
  end

  it "should respect collapse bullets" do
    Line.without_label(:line=>"< hey) you").should == "you"
  end

  it "should respect multiple collapse bullets" do
    Line.without_label(:line=>"<< hey) you").should == "you"
  end

  it "should respect multiple collapses and only label" do
    Line.without_label(:line=>"<< hey)").should == ""
  end

  it "should leave old style labels" do
    Line.without_label(:line=>"- hey: you").should == "hey: you"
  end

end

# class LineTest < Test::Unit::TestCase

#   def test_normal
#     assert_equal "you", Line.without_label(:line=>"- hey: you")
#   end

#   def test_just_bullet
#     assert_equal "you", Line.without_label(:line=>"- you")
#   end

#   def test_blank
#     assert_equal "", Line.without_label(:line=>"- hey: ")
#   end

#   def test_with_indent
#     assert_equal "you", Line.without_label(:line=>"  - hey: you")
#     assert_equal "", Line.without_label(:line=>"    - hey: ")
#     assert_equal "you", Line.without_label(:line=>"    - you")
#   end

#   def test_leave_indent
#     assert_equal "  you", Line.without_label(:line=>"  - hey: you", :leave_indent=>true)
#     assert_equal "    ", Line.without_label(:line=>"    - hey: ", :leave_indent=>true)
#   end

#   # Make sure it doesn't mess up the string
#   def test_side_effects
#     before = "  - label: hey1"
#     Line.without_label(:line=>before)
#     assert_equal "  - label: hey1", before
#   end

# end
