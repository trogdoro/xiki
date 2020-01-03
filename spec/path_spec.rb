$:.unshift "spec/"

require './spec/spec_helper'

require 'xiki/core/path'

describe Path, "#escape" do
  it "escapes slash" do
    Path.escape("a/b").should == "a;/b"
  end
  it "escapes at" do
    Path.escape("@a@b").should == ";@a@b"
  end
  it "escapes linebreaks" do
    Path.escape("a\nb").should == "a;0b"
  end
end

describe Path, "#unescape" do
  it "unescapes slash" do
    Path.unescape("a;/b").should == "a/b"
  end
  it "unescapes at" do
    Path.unescape("a;@b").should == "a@b"
  end
  it "unescapes linebreak" do
    Path.unescape("a;0b").should == "a\nb"
  end
end

describe Path, "#escape and #unescape" do
  it "escapes slash" do
    txt = "a//b!!c;;d /! !/ /; ;/ !; !;"
    result = txt.dup
    Path.unescape(Path.escape txt).should == result
  end
end


describe Path, "#split" do
  it "splits normal slashes" do
    Path.split("a/b").should == ["a", "b"]
  end

  it "returns empty list when blank" do
    Path.split("").should == []
  end


  it "doesn't split when no slashes" do
    Path.split("bb").should == ["bb"]
  end
  it "doesn't split escaped slashes" do
    Path.split("aa;/bb").should == ["aa/bb"]
  end
  it "only escapes unescaped slashes" do
    Path.split("aa;/bb/cc").should == ["aa/bb", "cc"]
  end

  it "splits ancestors when :outer" do
    Path.split("a/b/=c/d/", :outer=>1).should == ["a/b/", "c/d/"]
  end
  it "doesn't get confused by escaped slashes before ats" do
    Path.split("a/b;/=c/d/", :outer=>1).should == ["a/b;/=c/d/"]
  end
  it "doesn't get confused by escaped slashes ats" do
    Path.split("a/b;/=c/d/", :outer=>1).should == ["a/b;/=c/d/"]
  end

  it "splits on dollar signs when command" do
    Path.split("a/$ f", :outer=>1).should == ["a/", "$ f"]
  end
  it "splits on dollar signs when blank prompt in middle" do
    Path.split("a/$/b", :outer=>1).should == ["a/", "$/b"]
  end
  it "splits on dollar signs when blank prompt at end" do
    Path.split("a/$", :outer=>1).should == ["a/", "$"]
  end

  it "doesn't split when dollar whitout space after it" do
    Path.split("a/$f", :outer=>1).should == ["a/$f"]
  end


  it "handles :return_path option" do
    Path.split("bb", :return_path=>1).should == []
  end

  it "doesn't add extra item when trailing slashes" do
    Path.split("dom/body/").should == ["dom", "body"]
    Path.split("dom/body/", :return_path=>1).should == ["body"]
  end

  it "adds extra item when trailing slashes if :trailing" do
    Path.split("a/b/", :trailing=>1).should == ["a", "b", ""]
    Path.split("dom/body/", :return_path=>1).should == ["body"]
  end

  #   it "handles pipes" do
  #   # TODO what should we test here - pipes are probably already gone by this point?
  #     #     Path.split("aa/|b").should == ["aa", "|b"]
  #     #Path.split("aa/|b/b/|c/c").should == ["aa", "|b/b", "|c/c"]
  #   end

  it "turns ;l into linebreaks" do
    Path.split("a;lb").should == ["a\nb"]
  end

  # Todo > remove this > ;o is the new way
  it "turns ;0 into linebreaks" do
    Path.split("a;0b").should == ["a\nb"]
  end


  it "splits ancestors when spaces" do
    Path.split("a/b/= c/d/", :outer=>1).should == ["a/b/", "c/d/"]
  end




  it "splits on colon space" do
    Path.split("aa/bb: cc").should == ["aa", "bb", "cc"]
  end

  it "only escapes unescaped colons" do
    Path.split("aa/bb;: cc").should == ["aa", "bb: cc"]
  end




#   it "handles :return_path with pipes" do
#     Path.split("dom/| </div>", :return_path=>1).should == ["| </div>"]
#   end

# describe Menu, "#menu_to_hash" do
#   it "handles simple menu" do
#     input = "
#       | dotsies.loc : /projects/dotsies.org/www/
#       | dotsies.org : /xiki@xiki.org/var/www/dotsies.org/
#       ".unindent

#     Menu.menu_to_hash(input).should == {
#       "dotsies.loc"=>"/projects/dotsies.org/www/",
#       "dotsies.org"=>"/xiki@xiki.org/var/www/dotsies.org/",
#     }
#   end
end


describe Path, "#extract_quote" do
  it "extracts pipe quote" do
    path = "/hey/| quote"
    Path.extract_quote(path).should == " quote"
    path.should == "/hey"
  end

  it "extracts pipe followed by plus" do
    Path.extract_quote("/hey/|+quote").should == "+quote"
  end

  it "extracts pipe with word and no space" do
    Path.extract_quote("/hey/|quote").should == "quote"
  end

  it "extracts colon quote" do
    Path.extract_quote("/hey/: quote").should == " quote"
  end

  it "extracts colon and plus" do
    Path.extract_quote("/hey/:+quote").should == "+quote"
  end

  it "does not extract colon with word and no space" do
    Path.extract_quote("/hey/:quote").should == nil
  end

  it "removes both but returns just last when multiple" do
    path = "/hey/| quote/| another"
    Path.extract_quote(path).should == " another"
    path.should == "/hey"
  end

end

describe Path, "#extract_line_number" do
  it "extracts line number normally" do
    Path.extract_line_number("/hey/:22").should == "22"
  end

  it "extracts line number when slash at end" do
    Path.extract_line_number("/hey/:22/").should == "22"
  end

  it "removes line number from path" do
    path = "/hey/:22"
    Path.extract_line_number(path).should == "22"
    path.should == "/hey"
  end
end

describe Path, "#join" do
  it "joins path with 2 items" do
    Path.join(["a", "b"]).should == "a/b"
  end

  it "joins and escapes slashes" do
    Path.join(["a/a", "b"]).should == "a;/a/b"
  end
end
