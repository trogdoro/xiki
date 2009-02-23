$:.unshift "spec/"
require 'el_mixin'
require 'search'
# require 'core_ext'

describe Search, "#case_options" do
  it "should initialize" do
    options = Search.case_options
    options.class.should == Array
    options[0][0].should == 'upper'
  end

  it "should be convert case correctly" do
    options = Search.case_options
    options[0][1].call("hey").should == 'HEY'
    options[1][1].call("HEY").should == 'hey'
    options[2][1].call("hey_you").should == 'HeyYou'
    options[3][1].call("HeyYou").should == 'hey_you'
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

