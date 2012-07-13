$:.unshift "spec/"
require 'xiki'
require 'code_tree'
# require 'core_ext'

describe CodeTree, "#extract_class" do

  it "should extract method" do
    CodeTree.extract_method("- .you/").should == "you"
    CodeTree.extract_method("- Hey.you").should == "you"
    CodeTree.extract_method("- .you").should == "you"
    CodeTree.extract_method("- Wiki.menu('hey')").should == "menu('hey')"
    CodeTree.extract_method("- .pages('new')").should == "pages('new')"
    CodeTree.extract_method("- Wiki1.menu('hey')").should == "menu('hey')"
    CodeTree.extract_method("- .merb_local").should == "merb_local"
    CodeTree.extract_method("- Remote.dir 'f.com:21'").should == "dir 'f.com:21'"

  end

  it "should extract method with junk at beginning" do
    CodeTree.extract_method("you there/").should == nil
    CodeTree.extract_method(".you 'Bla.there'/").should == "you 'Bla.there'"
  end

  it "should extract class" do
    CodeTree.extract_class("wiki1.menu('hey')").should == nil
    CodeTree.extract_class("Wiki1.menu('hey')").should == "Wiki1"
    CodeTree.extract_class("blaa Wiki1.menu('hey')").should == nil
  end

  it "should get class from parent" do
    code = CodeTree.determine_code_from_path([
      "Wiki.menu('hey')",
      ".pages"])
    code.should == "Wiki.pages()"
  #     assert_equal("Wiki.pages()", code)
  end

  it "should ignore parent when child is root" do
    code = CodeTree.determine_code_from_path([
      "Red.herring/", "Foo.menu/", ".php/"])
    code.should == "Foo.php()"
  end

  it "should add curlies when hash" do
    code = CodeTree.determine_code_from_path(["CodeTree.menu/",
      "ProtoNight.menu/", ".data/", ".local/",
      ".send_invite_emails :date=>'2010-02-17'", "- Hey\n- You"])
    code.should == "ProtoNight.send_invite_emails( {:date=>'2010-02-17'}, \"Hey\n- You\")"
  end

  #   # Should get class from parent
  #   def test_with_params
  #     code = CodeTree.determine_code_from_path(["Foo.menu/", ".php(10)/", "mt"])
  #     assert_equal("Foo.php(10, 'mt')", code)
  #   end

end

