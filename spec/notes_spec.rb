$:.unshift "spec/"

require './spec/spec_helper'

%w"path code tree menu menu_suggester pre_pattern pattern file_tree bookmarks".each {|o| require "xiki/#{o}"}

require 'xiki/notes'

describe Notes, "#drill_split" do
  it "pulls out head ang quote" do
    Notes.drill_split(["> head", "| quote"]).should == [[], ["> head"], ["| quote"]]
  end

  it "works when blank quote" do
    Notes.drill_split(["> big", "|"]).should == [[], ["> big"], ["|"]]
  end
end
