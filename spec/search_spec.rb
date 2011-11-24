$:.unshift "spec/"
require 'el_mixin'
require 'search'
require 'core_ext'

describe Search, "#case_options" do
  it "should initialize" do
    options = Search.case_options
    options.class.should == Array
    options[0][0].should == 'upper'
  end

  it "should convert case correctly" do
    options = Search.case_options
    options[0][1].call("hey").should == 'HEY'
    options[1][1].call("HEY").should == 'hey'
    options[2][1].call("hey_you").should == 'HeyYou'
    options[3][1].call("HeyYou").should == 'hey_you'
  end

end

describe Search, "#deep_outline" do

  before(:all) do
    @small_tree = "
      daf jam
        stuff
      include ink
      closs Clam
        daf aa
        daf tt
      ".unindent
  end

  it "grabs lines above" do
    Search.deep_outline(@small_tree, 6)[0].should == "
      daf jam
      closs Clam
        daf tt
      ".unindent
  end

  it "remembers line we're on" do
    Search.deep_outline(@small_tree, 5)[1].should == 3
  end

  it "grabs only lines above with children" do
    before = "
      daf jam
        stuff
      include ink
      closs Clam
        daf zz
        daf aa
          stuff
        daf zzz
        daf tt
      ".unindent

    result = Search.deep_outline(before, 9)

    result[0].should == "
      daf jam
      closs Clam
        daf aa
        daf tt
      ".unindent
    result[1].should == 4
  end

  it "grabs lines below" do
    Search.deep_outline(@small_tree, 1)[0].should == "
      daf jam
      closs Clam
      ".unindent
  end

  it "includes lines above and below" do
    before ="
      include ink
      daf a
        ee
          bb
      closs Clam
        daf aa
          stuff
        daf zz
        daf bb
          aaa
            stuff
          zzz
          ttt
          ccc
            stuff
        daf zz
        daf cc
          stuff
        daf zzz
      end
      ".unindent

    result = Search.deep_outline(before, 13)
    result[0].should == "
      daf a
      closs Clam
        daf aa
        daf bb
          aaa
          ttt
          ccc
        daf cc
      ".unindent

    result[1].should == 6

  end

  #   it "should convert case correctly" do
  #     options = Search.case_options
  #     options[0][1].call("hey").should == 'HEY'
  #     options[1][1].call("HEY").should == 'hey'
  #     options[2][1].call("hey_you").should == 'HeyYou'
  #     options[3][1].call("HeyYou").should == 'hey_you'
  #   end

  it "ignores Ol lines" do
    tree = "
      daf jam
        aa
          aaa
      Ol.line
        bb
      Ol.line
        cc
          ccc
      ".unindent

    Search.deep_outline(tree, 5)[0].should == "
      daf jam
        aa
        bb
        cc
      ".unindent
  end

end


# describe Search, "# function that prompts" do
#   # TODO: move this into View.input :choices=>case_options
#   it "should get input from user" do

#   end
# end

# describe Search, "#change_case" do
#   it "should change case" do
#     1.should == 1
#     #     Line.without_label(:line=>"- hey: you").should == "you"
#   end
# end

