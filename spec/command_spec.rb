$:.unshift "spec/"

%w"core_ext ol tree core_ext menu launcher expander".each {|o| require "xiki/core/#{o}"}

require './spec/spec_helper'

describe :Command, "#split" do

  it "splits normal strings" do
    Command.split("aa/bb").should == ["aa", "bb"]
  end

  it "doesn't split when no slashes" do
    Command.split("bb").should == ["bb"]
  end

  it "doesn't split escaped slashes" do
    Command.split("aa/|bb").should == ["aa", "|bb"]
  end

# TODO uncomment?

#   it "handles :return_path option" do
#     Command.split("bb", :return_path=>1).should == []
#   end

#   it "handles trailing slashes" do
#     Command.split("dom/body/").should == ["dom", "body"]
#     Command.split("dom/body/", :return_path=>1).should == ["body"]
#   end

#   it "handles pipes" do
#     Command.split("aa/|b/b").should == ["aa", "|b/b"]
#     Command.split("aa/|b/b/|c/c").should == ["aa", "|b/b", "|c/c"]
#   end

#   it "handles :return_path with pipes" do
#     Command.split("dom/| </div>", :return_path=>1).should == ["| </div>"]
#   end
# end

# describe Menu, "#menu_to_hash" do
#   it "handles simple menu" do
#     input = "
#       | dotsies.loc : /projects/dotsies.org/www/
#       | dotsies.org : /xiki@xiki.org/var/www/dotsies.org/
#       ".unindent

#     Command.menu_to_hash(input).should == {
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
    Command.root_sources_from_path_env options
    options.should == {
      :menufied => "/projects/xiki/roots/path",
      :name => "path",
      :path => "path/",
      :sources => [["path.menu"], :incomplete]
      }
  end

  it "finds source for menu with space" do
    options = {:name=>"command_path", :path=>"command path/"}
    Command.root_sources_from_path_env options
    options.should == {
      :menufied => "/projects/xiki/roots/commands_path",
      :name => "command_path",
      :path => "command path/",
      :sources => [["command_path.rb"], :incomplete]
      }
  end
end

describe :Menu, "#expands?" do
  before(:each) do
    stub_menu_path_dirs   # Has to be before each for some reason
  end

  it "recognizes a menu" do
    options = {:name=>"path", :path=>"path/"}
    Command.expands?(options)

    options.should == {
      :name      => "path",
      :path      => "path/",
      :sources   => [["path.rb"], :incomplete ],
      :menufied  => "/projects/xiki/roots/path",
      :expanders => [Menu]
    }
  end

  it "doesn't try to handle when extension" do
    options = {:name=>"path", :path=>"path.txt/", :extension=>".txt"}
    Command.expands?(options)
    options[:expanders].should == [MenuSource]
  end

end
