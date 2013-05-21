$:.unshift "spec/"
require 'xiki/core/core_ext'
require 'xiki/core/tree_cursor'
require 'xiki/core/ol'

def a_aa_tree
  "
  - a/
    - aa/
  ".unindent
end

describe TreeCursor, "#each" do
  it "loops through each line" do
    tc = TreeCursor.new "- a/\n  - aa/", 1

    result, indexes = "", []
    tc.each do
      result << tc.line
      indexes << tc.i
    end

    result.should == "- a/  - aa/"
    indexes.should == [0, 1]
  end
end

describe TreeCursor, "#at_leaf?" do
  it "recognizes leaf if no children" do
    TreeCursor.new("
      - a/
      - aa/".unindent).at_leaf?.should == true
  end

  it "returns false if children" do
    TreeCursor.new(a_aa_tree).at_leaf?.should == false
  end

  it "recognizes leaf if at end" do
    TreeCursor.new("
      - a/
        - aa/".unindent, 1).at_leaf?.should == true
  end
end

describe TreeCursor, "#select" do
  it "finds matching line" do
    tc = TreeCursor.new(a_aa_tree)

    tc.select "  - aa/"
    tc.i.should == 1
  end

  it "stays at 0 when no matching line" do
    tc = TreeCursor.new(a_aa_tree)

    tc.select "  - aaa/"
    tc.i.should == 0
  end

  it "finds matching when bullet changed" do
    tc = TreeCursor.new(a_aa_tree)
    tc.select "  + aa/"
    tc.i.should == 1
  end
end

describe TreeCursor, "#under" do
  it "returns lines under" do
    tc = TreeCursor.new(a_aa_tree+"  - bb/")
    tc.under.should == "  - aa/\n  - bb/\n"
  end

  it "returns none if at end" do
    tc = TreeCursor.new(a_aa_tree)
    tc.i = 1
    tc.under.should == ""
  end
end

describe TreeCursor, "#<<" do
  it "inserts in tree" do
    tc = TreeCursor.new(a_aa_tree)
    tc << "  - zz/\n"
    tc.txt.should == "
      - a/
        - zz/
        - aa/
      ".unindent
  end
end
