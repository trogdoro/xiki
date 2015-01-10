$:.unshift "spec/"

require './spec/spec_helper'
class Keys
  def self.prefix; nil; end
end
%w"ol path tree xik code".each {|o| require "xiki/core/#{o}"}
%w"menu_handler".each {|o| require "xiki/handlers/#{o}"}

describe Xik, ".expand" do
  it "expands a" do
    x = Xik.new "a/\n  b/"
    x["a"].should == "b/"
  end

  it "doesn't mess up when two times in a row" do
    menu = Xik.new "
      ~ a/
      ~ b/
        - c/
        - d/
      "
    menu[''].should == "~ a/\n~ b/"
    menu[''].should == "~ a/\n~ b/"
  end

  it "doesn't mess up pluses" do
    x = Xik.new "
      a/
      b/
        - c
        - d
      "
    x["b"].should == "- c\n- d"
  end

  it "evals" do
    x = Xik.new "
      a/
        ! 1 + 2
      "
    x["a", :eval=>1].should == "3"
  end

  it "adds :nest=>1 when :eval and wasn't code" do
    options = {}
    x = Xik.new "
      a/
        b/
      "
    x["a", :eval=>options].should == "b/"
    options[:nest].should == 1
Ol "options", options
  end

end

describe Xik, ".parse_hash_or_array" do

  it "handles hash" do
    Xik.parse_hash_or_array({"a"=>"b"}).join("\n").should == "
      - a/
        - b/
      ".unindent.strip
  end

  it "handles array" do
    Xik.parse_hash_or_array(["a", "b"]).join("\n").should == "
      - ./
        - a/
      - ./
        - b/
      ".unindent.strip
  end

  it "handles a hash of arrays" do
    Xik.parse_hash_or_array({"a"=>["aa", "ab"], "b"=>"ba"}).join("\n").should == "
      - a/
        - ./
          - aa/
        - ./
          - ab/
      - b/
        - ba/
      ".unindent.strip
  end

end

$:.unshift "spec/"

require './spec/spec_helper'
%w"ol path tree xik".each {|o| require "xiki/core/#{o}"}

describe Xik, ".parse_hash_or_array" do

  it "handles hash" do
    Xik.parse_hash_or_array({"a"=>"b"}).join("\n").should == "
      - a/
        - b/
      ".unindent.strip
  end

  it "handles array" do
    Xik.parse_hash_or_array(["a", "b"]).join("\n").should == "
      - ./
        - a/
      - ./
        - b/
      ".unindent.strip
  end

  it "handles hash of arrays" do
    Xik.parse_hash_or_array({"a"=>["aa", "ab"], "b"=>"ba"}).join("\n").should == "
      - a/
        - ./
          - aa/
        - ./
          - ab/
      - b/
        - ba/
      ".unindent.strip
  end
end

describe Xik, ".initialize" do
  it "creates from string" do
    x = Xik.new "
      - a/
        - b/
      "
    x.lines.should == ["- a/", "  - b/", ""]
  end

  it "creates from hash of arrays" do
    x = Xik.new "a"=>["aa"]
    # x = Xik.new "
    #   - a/
    #     - ./
    #       - aa/
    #   "
    x.lines.should == ["- a/", "  - ./", "    - aa/"]
  end

  it "keeps indent when :leave_indent" do
    x = Xik.new "  a/\n    b/", :leave_indent=>1
    x.lines.should == ["  a/", "    b/"]
  end
end

describe Xik, ".cursor" do
  it "starts at 0" do
    x = Xik.foo
    x.cursor.should == 1
  end

  it "works on 2nd line" do
    x = Xik.foo
    x.next
    x.cursor.should == 4
  end

  it "works on 3rd line" do
    x = Xik.foo
    x.line = 3
    x.cursor.should == 11
  end

  it "works when cursor not at axis" do
    x = Xik.foo
    x.line = 3
    x.column = 3
    x.cursor.should == 13
  end

end

describe Xik, ".current" do
  it "starts at 1st line" do
    Xik.new("a\nb").current.should == "a"
  end
end

describe Xik, ".line" do
  it "starts at 1st line" do
    Xik.new("a").line.should == 1
  end
end

describe Xik, ".next" do
  it "moves to 2nd line" do
    x = Xik.new("a\nb")
    x.next
    x.line.should == 2
  end

  it "doesn't move past the file" do
    x = Xik.new("a\nb")
    x.line.should == 1
    x.current.should == "a"

    x.next
    x.line.should == 2
    x.current.should == "b"

    x.next   # Can't move past last line
    x.line.should == 2
    x.current.should == "b"
    # x.line.should == 1
  end
end

describe Xik, ".at_last_line?" do
  it "works when blank" do
    x = Xik.new("")
    x.at_last_line?.should == true
  end
  it "works when no trailing and one line" do
    x = Xik.new("a")
    x.at_last_line?.should == true
  end
  it "works when one line" do
    x = Xik.new("a\n")
    x.at_last_line?.should == false
    x.next
    x.at_last_line?.should == true
  end
end
