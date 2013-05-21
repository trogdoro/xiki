$:.unshift "spec/"

require 'xiki/core/xi'
require './spec/spec_helper'

describe Xi, "#hput" do
  before :each do
    @empty = {}
    @hash = {"x"=>{"y"=>"z"}}
  end

  it "updates actual hash" do
    Xi.hset(@empty, "a", "b")
    @empty.should == {"a"=>"b"}
  end

  it "sets one key and one val" do
    Xi.hset(@empty, "a", "b").should == {"a"=>"b"}
  end
  it "sets when ancestor" do
    Xi.hset(@empty, "a", "b", "c").should == {"a"=>{"b"=>"c"}}
  end
  it "sets when 3 deep" do
    Xi.hset(@empty, "a", "b", "c", "d", "e").should == {"a"=>{"b"=>{"c"=>{"d"=>"e"}}}}
  end

  it "adds next to existing" do
    Xi.hset(@hash, "a", "b").should == {"x"=>{"y"=>"z"}, "a"=>"b"}
  end
  it "merges with existing" do
    Xi.hset(@hash, "x", "a", "b").should == {"x"=>{"y"=>"z", "a"=>"b"}}
  end
end

describe Xi, "#hget" do
  before :each do
    @hash = {"a"=>"b"}
    @deep = {"a"=>{"b"=>"c"}}
  end

  it "reads when one level" do
    Xi.hget(@hash, "a").should == "b"
  end
  it "reads when two level" do
    Xi.hget(@deep, "a", "b").should == "c"
  end

  it "returns nil when key not there" do
    Xi.hget(@hash, "x", "m").should == nil
  end
end
