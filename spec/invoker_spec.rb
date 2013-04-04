$:.unshift "spec/"

%w"xiki/core_ext".each {|o| require o}

require 'xiki/invoker'

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
end
