# $:.unshift "spec/"
# require 'keys'
# require 'ol'

# describe Keys, "#input" do
#   before(:each) do

#     View = mock 'View'
#     #     Cursor = mock 'Cursor', :remember=>1, :green=>1, :hollow=>1
#     #     Cursor = mock 'Cursor', :null_object=>true
#     #     Cursor = mock  # :null_object=>true
#     #     Cursor = mock :null_object=>true
#     #     Cursor = mock 'Cursor', :null_object=>true
#     $el = mock "el", :read_char=>98   # doesn't matter what it returns
#     @choices = [['aaa', 'Aye'], ['bbb', 'Bye']]
#   end

#   it "with :choices param, gives choice according to char typed" do
#     $el.stub!(:char_to_string).and_return "a"
#     Keys.input(:prompt=>'letters: ', :choices=>@choices).should == 'Aye'
#     $el.stub!(:char_to_string).and_return "b"
#     Keys.input(:prompt=>'letters: ', :choices=>@choices).should == 'Bye'
#   end
# end
