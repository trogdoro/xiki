$:.unshift "spec/"
require 'el_mixin'
require 'ol'
require 'tree'
require 'core_ext'
require 'mode'
class Mode
  def self.define *args; end
end
require 'menu'

describe Menu, "#split" do
  it "splits normal strings" do
    Menu.split("bb").should == ["bb"]
    Menu.split("aa/bb").should == ["aa", "bb"]
    Menu.split("aa/|bb").should == ["aa", "|bb"]
  end

  it "handles :rootless option" do
    Menu.split("bb", :rootless=>1).should == []
    Menu.split("aa/bb", :rootless=>1).should == ["bb"]
  end

  it "handles trailing slashes" do
    Menu.split("dom/body/").should == ["dom", "body"]
    Menu.split("dom/body/", :rootless=>1).should == ["body"]
  end

  it "handles pipes" do
    Menu.split("aa/|b/b").should == ["aa", "|b/b"]
    Menu.split("aa/|b/b/|c/c").should == ["aa", "|b/b", "|c/c"]
  end

  it "handles :rootless with pipes" do
    Menu.split("dom/| </div>", :rootless=>1).should == ["| </div>"]
  end
end
