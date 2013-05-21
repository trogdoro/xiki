$:.unshift "spec/"

require './spec/spec_helper'

require 'xiki/core/conf_loading_handler'

%w"xiki/core/tree".each {|o| require o}

describe ConfLoadingHandler, "#parse" do
  it "makes a hash of one line" do
    txt = "- password: BaaRamEwe"
    ConfLoadingHandler.parse(txt).should == {"password"=>"BaaRamEwe"}
  end

  it "makes a hash of two lines" do
    txt = "
      - password: BaaRamEwe
      - motto: Baa
      ".unindent
    ConfLoadingHandler.parse(txt).should == {"password"=>"BaaRamEwe", "motto"=>"Baa"}
  end

  it "ignores headings and blank lines" do
    txt = "
      > Ignore headings
      - password: BaaRamEwe

      - motto: Baa
      ".unindent
    ConfLoadingHandler.parse(txt).should == {"password"=>"BaaRamEwe", "motto"=>"Baa"}
  end

  it "doesn't require dashes" do
    txt = "password: BaaRamEwe"
    ConfLoadingHandler.parse(txt).should == {"password"=>"BaaRamEwe"}
  end


  # it "handles nesting" do
  #
  # - What about the "- motto: Baa" vs "- motto/Baa" question?
  #   - For the xi syntax
  #
  #   txt = "
  #     - sheep password: Baa Ram Ewe
  #     - sheep mottos
  #       - Just keep baa'ing
  #       - Curlyness is next to godliness
  #     ".unindent

end
