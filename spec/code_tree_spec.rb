$:.unshift "spec/"
require 'xiki'
require 'xiki/code_tree'

describe CodeTree, "#extract_class" do
  context "extracing methods" do
    it "should extract starting with . and ending with /" do
      CodeTree.extract_method("- .you/").should == "you"
    end

    it "should extract when beginning with class" do
      CodeTree.extract_method("- Hey.you").should == "you"
    end

    it "should extract starting with . with clean ending" do
      CodeTree.extract_method("- .you").should == "you"
    end

    it "should extract with class and params" do
      CodeTree.extract_method("- Wiki.menu('hey')").should == "menu('hey')"
    end

    it "should extract without class and having params" do
      CodeTree.extract_method("- .pages('new')").should == "pages('new')"
    end

    it "should extract with class and params, having numbers into class" do
      CodeTree.extract_method("- Wiki1.menu('hey')").should == "menu('hey')"
    end

    it "should extract method having underscore" do
      CodeTree.extract_method("- .merb_local").should == "merb_local"
    end

    it "should extract method with params (without parentesis)" do
      CodeTree.extract_method("- Remote.dir 'f.com:21'").should == "dir 'f.com:21'"
    end

    context "with junk at beginning" do
      it "should extract rejecting the junk on beginning" do
        CodeTree.extract_method("you there/").should == nil
      end

      it "should extract rejecting the beginning junk (even it looking like a method)" do
        CodeTree.extract_method(".you 'Bla.there'/").should == "you 'Bla.there'"
      end
    end
  end

  context "extract class" do
    it "should extract class name all lowercased" do
      CodeTree.extract_class("wiki1.menu('hey')").should == nil
    end

    it "should extract class with first letter uppercase" do
      CodeTree.extract_class("Wiki1.menu('hey')").should == "Wiki1"
    end

    it "should extract class with junk at beginning" do
      CodeTree.extract_class("blaa Wiki1.menu('hey')").should == nil
    end
  end

  it "should get class from parent" do
    code = CodeTree.determine_code_from_path([
      "Wiki.menu('hey')",
      ".pages"])
    code.should == "Wiki.pages()"
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
end
