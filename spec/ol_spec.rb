$:.unshift "spec/"
require 'el_mixin'
require 'line'
require 'ol'
require 'core_ext'

def define_vars
  @stack = [
    "/projects/moo/moo.merb/app/models/cache.rb:77:in `get'",
    "/projects/moo/moo.merb/app/models/user.rb:28:in `exists?'",
    "/projects/moo/moo.merb/app/controllers/accounts.rb:248:in `check_availability'",
    "/Library/Ruby/Gems/1.8/gems/merb-action-args-1.1/lib/merb-action-args/abstract_controller.rb:42:in `__send__'",
    "/Library/Ruby/Gems/1.8/gems/merb-action-args-1.1/lib/merb-action-args/abstract_controller.rb:42:in `_call_action'"
     ]
  @stack_limited = [
    "/projects/moo/moo.merb/app/controllers/accounts.rb:248:in `check_availability'",
    "/projects/moo/moo.merb/app/models/user.rb:28:in `exists?'",
    "/projects/moo/moo.merb/app/models/cache.rb:77:in `get'"
    ]
  @stack_limited_previous = [
    "/projects/moo/moo.merb/app/controllers/accounts.rb:248:in `check_availability'",
    "/projects/moo/moo.merb/app/models/user.rb:8:in `exists?'",
    "/projects/moo/moo.merb/app/models/cache.rb:7:in `get'"
    ]
end

describe Ol, "#parse_line" do
  it "pulls out relevant parts" do
    h = Ol.parse_line "/projects/moo/moo.merb/app/models/cache.rb:77:in `get'"
    h.should == {:path=>"/projects/moo/moo.merb/app/models/cache.rb", :line=>"77", :method=>'get', :clazz=>'Cache'}
  end
end

describe Ol, "#remove_redundance" do
  before(:each) do
    define_vars
  end

  it "removes lines outside of a project" do
    stack = Ol.remove_redundance @stack_limited, @stack_limited_previous
    stack.should == [nil, @stack_limited[1], @stack_limited[2]]
  end

  it "doesn't remove all lines" do
    stack = Ol.remove_redundance @stack_limited, @stack_limited
    stack.should == [nil, nil, @stack_limited[2]]
  end
end

describe Ol, "#limit_stack" do
  before(:each) do
    define_vars
  end

  it "should remove lines outside of a project" do
    Ol.limit_stack @stack
    @stack.size.should == 3
    @stack.should == @stack_limited
  end

  it "should leave one line if all don't match" do
    Ol.limit_stack @stack, /^\/xx/
    @stack.size.should == 1
    @stack.should == ["/projects/moo/moo.merb/app/models/cache.rb:77:in `get'"]
  end
end

describe Ol, "#line" do
  before(:each) do
    @line = "/projects/moo/moo.merb/app/controllers/accounts.rb:24:in `check'"
  end

  it "writes line to log with label like - class.method (line):" do
    Ol.should_receive(:pause_since_last?).and_return false
    Ol.should_receive(:write_to_file).with('/tmp/ds_ol.notes', "- Accounts.check (24): hi\n")
    Ol.should_receive(:write_to_file_lines).with("/tmp/ds_ol.notes", "/projects/moo/moo.merb/app/controllers/accounts.rb:24\n")

    Ol.line "hi", @line, "", "ds"#, @@last_log
  end
end

describe Ol, "#log" do
  before(:each) do
    define_vars
    #     @line = "/projects/moo/moo.merb/app/controllers/accounts.rb:24:in `check'"
  end

  it "writes lines to log when stack is passed in" do
    Ol.should_receive(:pause_since_last?).and_return false
    Ol.should_receive(:write_to_file).with('/tmp/ds_ol.notes', "- Cache.get (77):\n  - User.exists? (28):\n    - Accounts.check_availability (248): hi\n")
    Ol.should_receive(:write_to_file_lines).with("/tmp/ds_ol.notes", "/projects/moo/moo.merb/app/models/cache.rb:77\n/projects/moo/moo.merb/app/models/user.rb:28\n/projects/moo/moo.merb/app/controllers/accounts.rb:248\n")

    Ol.log "hi", @stack[0..2], "ds"#, @@last_log
  end

  # TODO 1 reverse them
  # TODO 1 make it indent

end
