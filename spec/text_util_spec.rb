$:.unshift "spec/"
require 'el_mixin'
require 'text_util'

describe TextUtil, "#unindent" do
  it "indents to the left" do
    before =
      "  hey
          you
      ".gsub(/^      /, '')
    after =
      "hey
        you
      ".gsub(/^      /, '')
    TextUtil.unindent(before).should == after
  end

  it "doesn't delete blank lines" do
    before =
      "  hey

          you
      ".gsub(/^      /, '')
    after =
      "hey

        you
      ".gsub(/^      /, '')
    TextUtil.unindent(before).should == after
  end

  it "works when inital linebreak" do
    before =
      "
        hey
          you
      ".gsub(/^      /, '')
    after =
      "hey
        you
      ".gsub(/^      /, '')
    TextUtil.unindent(before).should == after
  end

  it "works when tabs exist" do
    before =
      "\they
      \tyou
      ".gsub(/^      /, '')
    after =
      "hey
      you
      ".gsub(/^      /, '')
    TextUtil.unindent(before).should == after
  end

  it "works when space on 2nd line" do
    before =
      "Dev Todo
             * Ad
             * 70:
             ".gsub(/^      /, '')
    after =
      "Dev Todo
      * Ad
      * 70:
      ".gsub(/^      /, '')
    TextUtil.unindent(before).should == after
  end

  it "shouldn't change if only 2 spaces over" do
    before =
      "Dev Todo
        * Ad
        * 70:
      ".gsub(/^      /, '')
    after =
      "Dev Todo
        * Ad
        * 70:
      ".gsub(/^      /, '')
    TextUtil.unindent(before).should == after

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
