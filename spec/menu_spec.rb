$:.unshift "spec/"

%w"core_ext ol tree core_ext menu launcher expander".each {|o| require "xiki/core/#{o}"}

require './spec/spec_helper'

describe :Menu, "#split" do

  it "splits normal strings" do
    Menu.split("aa/bb").should == ["aa", "bb"]
  end

  it "doesn't split when no slashes" do
    Menu.split("bb").should == ["bb"]
  end

  it "doesn't split escaped slashes" do
    Menu.split("aa/|bb").should == ["aa", "|bb"]
  end

# TODO uncomment?

#   it "handles :return_path option" do
#     Menu.split("bb", :return_path=>1).should == []
#   end

#   it "handles trailing slashes" do
#     Menu.split("dom/body/").should == ["dom", "body"]
#     Menu.split("dom/body/", :return_path=>1).should == ["body"]
#   end

#   it "handles pipes" do
#     Menu.split("aa/|b/b").should == ["aa", "|b/b"]
#     Menu.split("aa/|b/b/|c/c").should == ["aa", "|b/b", "|c/c"]
#   end

#   it "handles :return_path with pipes" do
#     Menu.split("dom/| </div>", :return_path=>1).should == ["| </div>"]
#   end
# end

# describe Menu, "#menu_to_hash" do
#   it "handles simple menu" do
#     input = "
#       | dotsies.loc : /projects/dotsies.org/www/
#       | dotsies.org : /xiki@xiki.org/var/www/dotsies.org/
#       ".unindent

#     Menu.menu_to_hash(input).should == {
#       "dotsies.loc"=>"/projects/dotsies.org/www/",
#       "dotsies.org"=>"/xiki@xiki.org/var/www/dotsies.org/",
#     }
#   end

end


describe :Menu, "#root_sources_from_path_env" do
  before(:each) do
    stub_menu_path_dirs   # Has to be before each for some reason
  end

  it "finds source for simple menu" do
    options = {:name=>"path", :path=>"path/"}
    Menu.root_sources_from_path_env options
    options.should == {
      :menufied => "/projects/xiki/menu/path",
      :name => "path",
      :path => "path/",
      :sources => [["path.menu"], :incomplete]
      }
  end

  it "finds source for menu with space" do
    options = {:name=>"menu_path", :path=>"menu path/"}
    Menu.root_sources_from_path_env options
    options.should == {
      :menufied => "/projects/xiki/menu/menu_path",
      :name => "menu_path",
      :path => "menu path/",
      :sources => [["menu_path.rb"], :incomplete]
      }
  end
end

describe :Menu, "#expands?" do
  before(:each) do
    stub_menu_path_dirs   # Has to be before each for some reason
  end

  it "recognizes a menu" do
    options = {:name=>"path", :path=>"path/"}
    Menu.expands?(options)

    options.should == {
      :name      => "path",
      :path      => "path/",
      :sources   => [["path.menu"], :incomplete ],
      :menufied  => "/projects/xiki/menu/path",
      :expanders => [Menu]
    }
  end
end
