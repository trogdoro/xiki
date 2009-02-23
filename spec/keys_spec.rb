$:.unshift "spec/"
require 'el_mixin'
require 'keys'
require 'ol'

describe Keys, "#input" do
  # TODO: move this into Keys.input :choices=>case_options
  it "should handle :choices param" do
    View = mock 'View'
    Cursor = mock 'Cursor', :null_object=>true
    #     View.should_receive(:).with(:one_char=>true, :prompt=>"xx").and_return('b')
    #     Keys.should_receive(:input).with(:one_char=>true, :prompt=>"xx").and_return('b')
Ol.line
    choice = Keys.input(:prompt=>'letters: ', :choices=>[['aaa', 'Aye'], ['bbb', 'Bye']])
    choice.should == 'Bye'
  end
end
