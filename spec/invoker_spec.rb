$:.unshift "spec/"

require 'xiki/core/invoker'

require './spec/spec_helper'

# describe Invoker, "#expand" do
describe Invoker, "#actionify" do
  it "single action" do
    Invoker.actionify(["bb"], [true]).should == ["bb", []]
  end

  it "second arg is action" do
    Invoker.actionify(["act", "b"], [nil, true]).should == ["b", ["act"]]
  end

  it "one action, one param" do
    Invoker.actionify(["act", "b"], [true, nil]).should == ["act", ["b"]]
  end

  it "two actions" do
    Invoker.actionify(["act", "act2", "b"], [true, true, nil]).should == ["act2", ["b"]]
  end

  it "doesn't mess up the args" do
    args = ["Hey you"]
    Invoker.actionify(args, [true, true, nil]).should == ["hey_you", []]
    args.should == ["Hey you"]
  end
end

describe Invoker, "#extract_ruby_module" do
  it "no module" do
    Invoker.extract_ruby_module("
      class Foo
      end
      ".unindent).should == nil
  end

  it "extracts one module" do
    Invoker.extract_ruby_module("
      module Modern
        class Foo
        end
      end
      ".unindent).should == "Modern"
  end

  it "extracts two modules" do
    Invoker.extract_ruby_module("
      module Modern
        module Modest
          class Foo
          end
        end
      end
      ".unindent).should == "Modern::Modest"
  end

  it "isn't confused by stuff after start of class" do
    Invoker.extract_ruby_module("
      module Modern
        # stuff
        class Foo
        end
        module Herring
        end
      end
      ".unindent).should == "Modern"
  end

  it "handles when all on one line" do
    Invoker.extract_ruby_module("
      class Modern::Foo
        # stuff
      end
      ".unindent).should == "Modern"
  end

end
