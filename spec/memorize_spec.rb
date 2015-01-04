# $:.unshift "spec/"
# require './spec/spec_helper'
# require 'json'

# %w"path tree".each {|o| require "xiki/core/#{o}"}
# # %w"path code tree menu menu_suggester pre_pattern pattern file_tree bookmarks".each {|o| require "xiki/core/#{o}"}

# require_relative '../menu/memorize.rb'

# # describe Memorize, "#expand" do
# describe Memorize, "#propagate_edits" do
#   it "updates when just question" do

#     txt = Tree.children(Memorize::MENU_HIDDEN.unindent, "test/deserialize/").gsub /^: /, ''
#     o = Memorize.new txt

#     o.propagate_edits
#     o.pending[3].should == "Japana : Tokyo"

#   end

#   it "updates when just question and answer" do
#     txt = Tree.children(Memorize::MENU_HIDDEN.unindent, "test/deserialize/").gsub /^: /, ''

#     txt.sub! "___", "Tokistan"

#     o = Memorize.new txt

#     o.propagate_edits
#     o.pending[3].should == "Japana : Tokistan"
#   end
# end

# describe Memorize, "#deserialize" do
#   it "works" do

#     txt = Tree.children(Memorize::MENU_HIDDEN.unindent, "test/deserialize/").gsub /^: /, ''
# Ol.a txt

#     o = Memorize.new

#     o.deserialize txt
#     o.heading.should == ["> Example"]
#     o.completed.length.should == 3
#     o.progress.should == [[3, 1, 2], []]
#   end
# end
