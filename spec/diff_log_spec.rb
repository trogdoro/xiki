$:.unshift "spec/"
require 'el_mixin'
require 'line'
require 'ol'
require 'diff_log'
# require 'core_ext'

describe Line, "#parse_tree_diffs" do
  it "parses a rename" do
    DiffLog.parse_tree_diffs("d5 1\na5 1\n    - cccc\n").should == [[[5, "    - cccc"]], [5]]
  end

  it "parses a move" do
    DiffLog.parse_tree_diffs("d4 1\na7 1\n    - bb\n").should == [[[7, "    - bb"]], [4]]
  end

  it "parses a delete" do
    DiffLog.parse_tree_diffs("d3 2\nd6 1\n").should == [[], [3, 4, 6]]
  end

  it "parses an add" do
    DiffLog.parse_tree_diffs("a24 1\n    - nn\n").should == [[[24, "    - nn"]], []]
  end

  it "parses 2 adds and 2 deletes" do
    DiffLog.parse_tree_diffs("d12 2\na15 2\n    - cc\n    - dd\n").should == [[[15, "    - cc"], [16, "    - dd"]], [12, 13]]
  end
end
