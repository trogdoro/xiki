$:.unshift "spec/"

require './spec/spec_helper'
require "xiki/core/launcher"
require "xiki/core/xik"
Dir["./lib/xiki/handlers/*_handler.rb"].each{|o|
  require o.sub("./lib/", "")
}
%w"path code tree menu command_suggester pre_pattern pattern file_tree bookmarks".each {|o| require "xiki/core/#{o}"}

require 'xiki/core/expander'
require 'xiki/core/pattern'
require 'xiki/core/control_tab'

# describe Expander, "#expand" do
describe Expander, "#extract_ancestors" do
  it "pulls out one path" do
    args = "a/=b/", {}
    Expander.extract_ancestors *args
    args.should == ["b/", {:ancestors=>["a/"]}]
  end

  it "works when no nesting" do
    args = "a/b/", {}
    Expander.extract_ancestors *args
    args.should == ["a/b/", {}]
  end

  it "ignores quoted path" do
    args = "a/| a/=b/", {}
    Expander.extract_ancestors *args
    args.should == ["b/", {:ancestors=>["a/| a/"]}]
  end
end

describe Expander, "#expand_file_path" do
  it "expands dot" do
    Expander.expand_file_path("../a").should =~ %r"\w+/a$"
    Expander.expand_file_path("./a").should =~ %r"\w+/a$"
  end

  it "expands home dir" do
    Expander.expand_file_path("~/a").should =~ %r"\w+/a$"
  end

  it "expands bookmarks" do
    stub(Bookmarks).[]("^d") {"/tmp/dir/"}
    Expander.expand_file_path("^d/a//b").should == "/tmp/dir/a//b"
    Expander.expand_file_path("^d").should == "/tmp/dir"
    Expander.expand_file_path("^d/").should == "/tmp/dir/"
  end

  it "doesn't remove double slashes for home and current dir" do
    Expander.expand_file_path("./a//b").should =~ %r"a//b"
    Expander.expand_file_path("~/a//b").should =~ %r"a//b"
  end

  it "doesn't remove double slashes for bookmarks" do
    stub(Bookmarks).[]("^links") {"/tmp/file.txt"}
    Expander.expand_file_path("^links//").should == "/tmp/file.txt//"
  end
end

describe Expander, "#parse" do

  it "handles pure dir paths" do
    Expander.parse("/tmp/a/b/").should ==
      {:file_path=>"/tmp/a/b/"}
  end

  it "handles file paths" do
    Expander.parse("/tmp/a/b").should ==
      {:file_path=>"/tmp/a/b"}
  end

  it "handles file path with spaces" do
    Expander.parse("/tmp/a/b c").should ==
      {:file_path=>"/tmp/a/b c"}
  end

  it "just passes through when input already a hash" do
    Expander.parse(:foo=>"bar").should ==
      {:foo=>"bar"}
  end

  it "handles menufied paths" do
    Expander.parse("/tmp/a//").should ==
      {:menufied=>"/tmp/a", :path=>"/tmp/a//"}
  end

  it "handles menufied path with items" do
    Expander.parse("/tmp/a//b/").should ==
      {:menufied=>"/tmp/a", :items=>["b"], :path=>"/tmp/a//b/"}
  end

  it "handles filesystem root menufied path" do
    Expander.parse("//").should ==
      {:menufied=>"/", :path=>"//"}
  end

  it "handles name that looks kind of menufied" do
    Expander.parse("a/http://notmenufied.com/").should ==
      {:name=>"a", :items=>["http:", "", "notmenufied.com"],
       :path=>"a/http://notmenufied.com/"
      }
  end

  it "handles names" do
    Expander.parse("a").should ==
      {:name=>"a", :path => "a"}
  end

  it "handles name when space" do
    Expander.parse("a b").should ==
      {:name=>"a_b", :path => "a b"}
  end

  it "handles name with items" do
    Expander.parse("a/b/c/").should ==
      {:name=>"a", :items=>["b", "c"], :path => "a/b/c/"}
  end

  it "handles name with quoted items" do
    Expander.parse("a/| hi").should ==
      {:name=>"a", :items=>["| hi"], :path => "a/| hi"}
  end

  it "handles name with quoted slash" do
    Expander.parse("a/| foo/yau").should ==
      {:name=>"a", :items=>["| foo", "yau"], :path => "a/| foo/yau"}
  end

  it "handles patterns" do
    Expander.parse("select * from users").should ==
      {:path=>"select * from users"}
  end

  it "handles pattern that looks kind of like a file path" do
    Expander.parse("/user@site.com/a/").should ==
      {:path=>"/user@site.com/a/"}
  end

  it "handles name with list of items" do
    Expander.parse("a", ["b", "c"]).should ==
      {:name=>"a", :items=>["b", "c"], :path => "a/b/c"}
  end

  it "handles symbol with list of items" do
    Expander.parse(:a, ["b", "c"]).should ==
      {:name=>"a", :items=>["b", "c"]}
  end

  it "merges path with item and list of items" do
    Expander.parse("a/b", ["c", "d"]).should ==
      {:name=>"a", :items=>["b", "c", "d"], :path=>"a/b/c/d"}
  end

  it "handles ancestors in string" do
    Expander.parse("z/=a/").should ==
      {:name=>"a", :ancestors=>["z/"], :path => "a/"}
  end

  it "handles ancestors with path in string" do
    Expander.parse("x/y/=a/b/").should ==
      {:name=>"a", :items=>["b"], :ancestors=>["x/y/"], :path => "a/b/"}
  end

  it "pulls out ancestors when first arg is array" do
    Expander.parse(["x/y/", "m/n/", "a/b/"]).should ==
      {:name=>"a", :items=>["b"], :ancestors=>["x/y/", "m/n/"],
       :path => "a/b/"
      }
  end

  it "doesn't create ancestors when first arg is array of size 1" do
    Expander.parse(["a/b/"]).should ==
      {:name=>"a", :items=>["b"], :path => "a/b/"}
  end

  it "recognizes escaped slashes" do
    Expander.parse("echo/a;/b").should == {
      :items => ["a/b"],
      :name => "echo",
      :path => "echo/a;/b"
    }
  end


  it "pulls out extension" do
    Expander.parse("echo.txt").should == {
      :name=>"echo",
      :extension=>".txt",
      :path=>"echo.txt"
    }
  end

  it "pulls out path and extension" do
    Expander.parse("echo.txt/a/b").should == {
      :items=>["a", "b"],
      :name=>"echo",
      :extension=>".txt",
      :path=>"echo.txt/a/b"
    }
  end

  it "pulls out period-only extension" do
    Expander.parse("echo.").should == {
      :name=>"echo",
      :extension=>".",
      :path=>"echo."
    }
  end





  it "moves task items into :task" do
    options = Expander.parse("hi/* delete")
    options.should == {
      :task=>"delete",
      :name=>"hi",
      :path=>"hi",
    }
  end

    # Also test these...
    # options = Expander.parse("select * from/* delete")
    # options = Expander.parse("/tmp/* delete")

  it "moves pattern task items into :task" do
    options = Expander.parse("select * from hi/* delete")
    options.should == {
      :task=>"delete",
      :path=>"select * from hi"
    }
  end

  it "moves file task items into :task" do
    options = Expander.parse("/tmp/* delete")
    options.should == {
      :task=>"delete",
      :file_path=>"/tmp"
    }
  end


end

describe Expander, "#expand_literal_command method" do
  before(:each) do
    stub_menu_path_dirs   # Has to be before each for some reason
  end

  it "expands command text with path" do
    txt = Expander.expand_literal_command "a/\n  b", :path=>"a"
    txt.should == 'b'
  end

  it "expands when embedded code" do
    txt = Expander.expand_literal_command "a/\n  ! 1 + 1", :path=>"a"
    txt.should == '2'
  end
end

describe Expander, "#expand method" do
  before(:each) do
    stub_menu_path_dirs   # Has to be before each for some reason
  end

  it "expands menu when no path" do
    Expander.def(:echo) { |path| path.inspect }
    Expander.expand("echo").should == '[]'
  end

  it "expands menu when path" do
    Expander.def(:echo) { |path| path.inspect }
    Expander.expand("echo/a").should == '["a"]'
  end

  it "takes a path list as 2nd arg" do
    Expander.def(:echo) { |path| path.inspect }

    Expander.expand("echo", ["a", "b"]).should == '["a", "b"]'
  end

  it "expands menu in XIKI_PATH" do
    Expander.expand("dd").should == "+ a/\n+ b/\n+ cccc/\n+ craig/\n+ keith/\n"
  end

  it "expands plain file path" do
    Expander.expand("#{Xiki.dir}spec/fixtures/menu/dr/").should == "+ a.rb\n+ b.rb\n"
  end

  it "expands menufied path" do
    Expander.expand("#{Xiki.dir}spec/fixtures/menu/dr//").should == "+ a/\n+ b/\n"
  end

  # it "expands when literal text of command passed" do
  #   Expander.expand(["a"], :command_text=>"a/\n  b").should == "b"
  # end

  # it "expands when literal text of command with path" do
  #   Expander.expand(["a/b"], :command_text=>"a/\n  b/\n    c").should == "c"
  # end

end

describe Expander, ".extract_task_items" do
  before(:each) do
    stub_menu_path_dirs   # Has to be before each for some reason
  end

  it "extracts when normal" do
    # Expander.def(:echo) { |path| path.inspect }
    # Expander.expand("echo").should == '[]'
  end
end
