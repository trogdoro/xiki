$:.unshift "spec/"
require 'xiki/core/line'
require 'xiki/core/ol'
require 'xiki/core/core_ext'

def define_vars
  @stack = [
    "/projects/foo/models/cache.rb:77:in `get'",
    "/projects/foo/models/user.rb:28:in `exists?'",
    "/projects/foo/controllers/accounts.rb:248:in `check'",
    "/lib/gems/merb-action-args/abstract_controller.rb:42:in `aa'",
    "/lib/gems/merb-action-args/abstract_controller.rb:42:in `bb'"
     ]
  @stack_limited = [
    "/projects/foo/controllers/accounts.rb:248:in `check'",
    "/projects/foo/models/user.rb:28:in `exists?'",
    "/projects/foo/models/cache.rb:77:in `get'"
    ]
  @stack_limited_previous = [
    "/projects/foo/controllers/accounts.rb:248:in `check'",
    "/projects/foo/models/user.rb:8:in `exists?'",
    "/projects/foo/models/cache.rb:7:in `get'"
    ]
end

describe Ol, "#parse_line" do
  it "pulls out relevant parts" do
    h = Ol.parse_line "/projects/foo/models/cache.rb:77:in `get'"
    h.should == {:path=>"/projects/foo/models/cache.rb", :line=>"77", :method=>'get', :clazz=>'Cache'}
  end
end

describe Ol, "#remove_common_ancestors" do
  before(:each) do
    define_vars
  end

  it "removes lines outside of a project" do
    stack = Ol.remove_common_ancestors @stack_limited, @stack_limited_previous
    stack.should == [nil, @stack_limited[1], @stack_limited[2]]
  end

  it "doesn't remove all lines" do
    stack = Ol.remove_common_ancestors @stack_limited, @stack_limited
    stack.should == [nil, nil, @stack_limited[2]]
  end
end

describe Ol, "#remove_extraneous" do
  before(:each) do
    define_vars
  end

  it "should remove lines outside of a project" do
    Ol.remove_extraneous @stack
    @stack.size.should == 3
    @stack.should == @stack_limited
  end

  it "should leave one line if all don't match" do
    Ol.remove_extraneous @stack, /^\/xx/
    @stack.size.should == 1
    @stack.should == ["/projects/foo/models/cache.rb:77:in `get'"]
  end
end

describe Ol, "#line" do
  before(:each) do
    @line = "/projects/foo/controllers/accounts.rb:24:in `check'"
  end

  it "writes line to log with label like - class.method (line):" do
    Ol.should_receive(:pause_since_last?).and_return false
    Ol.should_receive(:write_to_file).with('/tmp/ds_ol.notes', "- Accounts.check:24) hi\n")
    Ol.should_receive(:write_to_file_lines).with("/tmp/ds_ol.notes", "/projects/foo/controllers/accounts.rb:24\n")

    Ol.line "hi", @line, "", "ds"#, @@last_log
  end
end

describe Ol, "#log" do
  before(:each) do
    define_vars
  end

  it "writes lines to log when stack is passed in" do
    Ol.should_receive(:pause_since_last?).and_return false
    Ol.should_receive(:write_to_file).with('/tmp/ds_ol.notes', "- Cache.get:77)\n  - User.exists?:28)\n    - Accounts.check:248) hi\n")
    Ol.should_receive(:write_to_file_lines).with("/tmp/ds_ol.notes", "/projects/foo/models/cache.rb:77\n/projects/foo/models/user.rb:28\n/projects/foo/controllers/accounts.rb:248\n")

    Ol.log "hi", @stack[0..2], "ds"#, @@last_log
  end

  # TODO 1 reverse them
  # TODO 1 make it indent
end


describe Ol, "#nesting_match" do
  it "finds root path in a stack regardless of line number" do
    roots = [Ol.nesting_match_regexp("/foo/bar.rb:45:in `speak'")]
    stack = [
      "/foo/bar.rb:41:in `hi'",
      "/foo/bar.rb:46:in `speak'",
      "/foo/bar.rb:49:in `eval'",
      ]
    Ol.nesting_match(stack, roots).should == 1
  end

  it "returns nil when no match" do
    roots = [Ol.nesting_match_regexp("nested.rb:10:in `b'")]
    stack = ["nested.rb:6:in `a'", "nested.rb:18:in `<main>'"]
    Ol.nesting_match(stack, roots).should == nil
  end

  it "finds root when mutliple" do
    roots = [
      Ol.nesting_match_regexp("/foo/bar.rb:45:in `speak'"),
      Ol.nesting_match_regexp("/foo/baz.rb:75:in `sit'"),
      ]
    stack = [
      "/foo/bar.rb:41:in `hi'",
      "/foo/bar.rb:46:in `speak'",
      "/foo/bar.rb:49:in `eval'",
      ]
    Ol.nesting_match(stack, roots).should == 1
  end

  it "finds deepest root when mutliple matches" do
    roots = [
      Ol.nesting_match_regexp("/foo/bar.rb:45:in `speak'"),
      Ol.nesting_match_regexp("/foo/bar.rb:48:in `program'"),
      ]
    stack = [
      "/foo/bar.rb:41:in `hi'",
      "/foo/bar.rb:46:in `speak'",
      "/foo/bar.rb:49:in `program'",
      ]
    Ol.nesting_match(stack, roots).should == 2
  end
end

describe Ol, "#nesting_match_regexp" do
  it "creates proper regex" do
    Ol.nesting_match_regexp("aa:45:bb").should == /^aa:[0-9]+:bb$/
  end

  it "patterns match correctly" do
    ("/tmp/a.rb:45:in `speak'" =~ Ol.nesting_match_regexp("/tmp/a.rb:46:in `speak'")).should == 0
  end
end
