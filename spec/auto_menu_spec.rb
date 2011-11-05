$:.unshift "spec/"
require 'el_mixin'
require 'ol'
require 'tree'
require 'core_ext'
require 'auto_menu'

describe AutoMenu, "#child_bullets" do

  before(:all) do
    @tree = "
       - .aa
       - .bb/
         - .bb1/
           - bb11
         - .bb2
       - cc/
         - .cc1/
         - .cc2
       - dd/
         | > Heading
         | Underneath.
       - ee/
         - ee2/
           | > Heading
           | Underneath.
       - .ff
       ".unindent
  end

  it "finds children of root" do
    AutoMenu.child_bullets(@tree, nil).should == "- .aa\n- .bb/\n- cc/\n- dd/\n- ee/\n- .ff\n"
  end

  it "finds two children" do
    AutoMenu.child_bullets(@tree, "/cc/").should == "- .cc1/\n- .cc2\n"
  end

  it "finds children 2 levels in" do
    AutoMenu.child_bullets(@tree, "bb/bb1").should == "- bb11\n"
  end

  it "finds children that have children" do
    AutoMenu.child_bullets(@tree, "bb").should == "- .bb1/\n- .bb2\n"
  end

  it "finds children when no period" do
    AutoMenu.child_bullets(@tree, "cc").should == "- .cc1/\n- .cc2\n"
  end

  it "finds nothing when no children" do
    AutoMenu.child_bullets(@tree, "aa").should == ""
  end

  it "finds nothing when no match" do
    AutoMenu.child_bullets(@tree, "xx").should == ""
  end

  it "shows quoted children" do
    AutoMenu.child_bullets(@tree, "/dd/").should == "| > Heading\n| Underneath.\n"
  end

  it "shows quoted children 2 levels in" do
    AutoMenu.child_bullets(@tree, "/ee/ee2/").should == "| > Heading\n| Underneath.\n"
  end

end
