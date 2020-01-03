$:.unshift "spec/"

require './spec/spec_helper'

Dir["./lib/xiki/handlers/*_handler.rb"].each{|o| require o.sub("./lib/", "") }
%w"xik expander command code_tree path code tree topic notes xikihub_client menu command_suggester pre_pattern pattern file_tree bookmarks topic_expander".each {|o| require "xiki/core/#{o}"}

%w"path code tree menu command_suggester pre_pattern pattern file_tree bookmarks".each {|o| require "xiki/core/#{o}"}

require 'xiki/core/notes'

describe Notes, "#replace_section" do
  it "add wiki section" do
    txt = Expander.expand "xiki api/add wiki section"
    txt.should == %`
      | > Foo
      | New
      | section prepended!
      |
      |
      | > Herring
      | Unrelated heading
    `.unindent
  end

  it "update existing wiki section replace" do
    txt = Expander.expand "xiki api/update existing wiki section replace"
    txt.should == %`
      | > Foo
      | New
      | section replaced old!
      |
      |
    `.unindent
  end

end


describe Notes, "#drill_split" do
  it "pulls out head ang quote" do
    Notes.drill_split(["> head", "| quote"]).should == [[], ["> head"], ["| quote"]]
  end

  it "works when blank quote" do
    Notes.drill_split(["> big", "|"]).should == [[], ["> big"], ["|"]]
  end
end
