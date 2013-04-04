$:.unshift "spec/"
require 'xiki/ol'
require 'xiki/tree'
require 'xiki/core_ext'
require 'xiki/menu'

describe :Menu, "#split" do
  it "splits normal strings" do
    Menu.split("bb").should == ["bb"]
    Menu.split("aa/bb").should == ["aa", "bb"]
    Menu.split("aa/|bb").should == ["aa", "|bb"]
  end

  it "handles :return_path option" do
    Menu.split("bb", :return_path=>1).should == []
  end

  it "handles trailing slashes" do
    Menu.split("dom/body/").should == ["dom", "body"]
    Menu.split("dom/body/", :return_path=>1).should == ["body"]
  end

  it "handles pipes" do
    Menu.split("aa/|b/b").should == ["aa", "|b/b"]
    Menu.split("aa/|b/b/|c/c").should == ["aa", "|b/b", "|c/c"]
  end

  it "handles :return_path with pipes" do
    Menu.split("dom/| </div>", :return_path=>1).should == ["| </div>"]
  end
end

describe Menu, "#menu_to_hash" do
  it "handles simple menu" do
    input = "
      | dotsies.loc : /projects/dotsies.org/www/
      | dotsies.org : /xiki@xiki.org/var/www/dotsies.org/
      ".unindent

    Menu.menu_to_hash(input).should == {
      "dotsies.loc"=>"/projects/dotsies.org/www/",
      "dotsies.org"=>"/xiki@xiki.org/var/www/dotsies.org/",
    }
  end
end
