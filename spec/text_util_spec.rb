$:.unshift "spec/"
require './spec/spec_helper'
require 'xiki/core/text_util'

describe TextUtil, "#unindent" do

  it "indents to the left" do
    TextUtil.unindent(
      "  hey
          you
      ".gsub(/^      /, '')).should ==
      "hey
        you
      ".gsub(/^      /, '')
  end

  it "doesn't delete blank lines" do
    TextUtil.unindent(
      "  hey

          you
      ".gsub(/^      /, '')).should ==
      "hey

        you
      ".gsub(/^      /, '')
  end

  it "works when inital linebreak" do
    TextUtil.unindent(
      "
        hey
          you
      ".gsub(/^      /, '')).should ==
      "hey
        you
      ".gsub(/^      /, '')
  end

  it "works when tabs exist" do
    TextUtil.unindent(
      "\they
      \tyou
      ".gsub(/^      /, '')).should ==
      "hey
      you
      ".gsub(/^      /, '')
  end

  it "works when space on 2nd line" do
    TextUtil.unindent(
      "Dev Todo
             * Ad
             * 70:
             ".gsub(/^      /, '')).should ==
      "Dev Todo
      * Ad
      * 70:
      ".gsub(/^      /, '')
  end

  it "shouldn't change if only 2 spaces over" do
    TextUtil.unindent(
      "Dev Todo
        * Ad
        * 70:
      ".gsub(/^      /, '')).should ==
      "Dev Todo
        * Ad
        * 70:
      ".gsub(/^      /, '')
  end

  it "doesn't change indent when any subsequent line not indented" do
    shouldnt_change = "  [
          1,
          2
      not indented
      ".gsub(/^      /, '')
    TextUtil.unindent(shouldnt_change).should == shouldnt_change
  end

  it "adds linebreak for consistency when any subsequent line not indented" do
    TextUtil.unindent("  a\nnot indented").should == "  a\nnot indented\n"
  end

  it "Doesn't remove trailing spaces on last line" do
    TextUtil.unindent(
      "
        hey 
        you 
      ".gsub(/^      /, '')).
      should == "hey \nyou \n"
  end


end

describe TextUtil, "#snake_case" do
  it "converts to snake" do
    TextUtil.snake_case("core_platform").should == "core_platform"
    TextUtil.snake_case("CorePlatform").should == "core_platform"
    TextUtil.snake_case("/CorePlatform").should == "core_platform"
    TextUtil.snake_case("core platform").should == "core_platform"
    TextUtil.snake_case("core-platform").should == "core_platform"

    hi = "HiYou"
    TextUtil.snake_case! hi
    hi.should == "hi_you"

    TextUtil.snake_case("core+platform").should == "core_platform"

  end
end

describe TextUtil, "#camel_case" do
  it "converts to camel" do
    TextUtil.camel_case("core_platform").should == "CorePlatform"
    TextUtil.camel_case("CORE_PLATFORM").should == "CorePlatform"
    TextUtil.camel_case("CorePlatform").should == "CorePlatform"
    TextUtil.camel_case("/CorePlatform").should == "CorePlatform"
    TextUtil.camel_case("core platform").should == "CorePlatform"
    TextUtil.camel_case("core-platform").should == "CorePlatform"
    TextUtil.camel_case("core/platform").should == "CorePlatform"
  end
end

describe TextUtil, "#title_case" do
  it "converts to title case" do
    TextUtil.title_case("core_platform").should == "Core Platform"
    TextUtil.title_case("CorePlatform").should == "Core Platform"
    TextUtil.title_case("/CorePlatform").should == "/Core Platform"
    TextUtil.title_case("core platform").should == "Core Platform"
    TextUtil.title_case("core-platform").should == "Core Platform"
  end
end

describe TextUtil, "#word_wrap" do
  it "wraps to 5 chars" do
    TextUtil.word_wrap("hi hi hi", 5).should == "hi hi\nhi"
  end

  it "wraps to 4 chars" do
    TextUtil.word_wrap("hi hi hi", 4).should == "hi\nhi\nhi"
  end
end
