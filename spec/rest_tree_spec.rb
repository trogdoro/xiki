$:.unshift "spec/"
require './spec/spec_helper'

require 'xiki/core/tree'
require 'xiki/core/rest_tree'

describe RestTree, "#comment_embedded_menu" do

  it "returns unaltered when no comment" do
    RestTree.comment_embedded_menu("foo", "foo").should == nil
  end

  it "uses a menu" do
    txt = "
      Hey
      <!--
        - foo/
        - bar/
        -->
      "
    RestTree.comment_embedded_menu(txt, "").should == "+ foo/\n+ bar/\n"
  end

  it "uses menu when path" do
    txt = "
      Hey
      <!--
        - foo/
          - XX
        - bar/
        -->
      "
    RestTree.comment_embedded_menu(txt, "xiki://localhost/foo").should == "- XX\n"
  end

end
