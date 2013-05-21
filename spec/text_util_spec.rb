$:.unshift "spec/"
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

  it "does nothing when subsequent line not indented" do
    shouldnt_change = "[
          1,
          2,
          3
      ]
      ".gsub(/^      /, '')
    TextUtil.unindent(shouldnt_change).should == shouldnt_change
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
