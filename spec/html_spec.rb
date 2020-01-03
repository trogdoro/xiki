$:.unshift "spec/"
module Xiki; end
require 'xiki/core/tree'
require 'xiki/core/html'
require './spec/spec_helper'

describe Html, "#to_html_tags" do
  it "handles one tag" do
    Html.to_html_tags("
      p/
        hi
      ".unindent).should == "
      <p>
        hi
      </p>
      ".unindent
  end

  it "handles single with no contents" do
    Html.to_html_tags("
      p/
      ".unindent).should == "
      <p>
      </p>
      ".unindent
  end

  it "doesn't confuse comments" do
    Html.to_html_tags("
      p/
        /* hey */
      ".unindent).should == "
      <p>
        /* hey */
      </p>
      ".unindent
  end

  it "adds closing tags to html" do
    Html.to_html_tags("
      <p>
        hi
      ".unindent).should == "
      <p>
        hi
      </p>
      ".unindent
  end

  it "doesn't close comment tags" do
    Html.to_html_tags("
      <!-- hey -->
      hi
      ".unindent).should == "
      <!-- hey -->
      hi
      ".unindent
  end
end
