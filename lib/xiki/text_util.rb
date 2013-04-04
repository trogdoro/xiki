require 'xiki/line'
require 'xiki/ol'

class TextUtil

  def self.menu
    %`
    > Change case
    @ TextUtil.camel_case "hey you"
    @ TextUtil.hyphen_case "hey you"
    @ TextUtil.snake_case "hey you"
    @ TextUtil.title_case "hey you"

    > Modify vars in-place
    You can also use bang versions, like:
    @ s = "hey you";  TextUtil.camel_case! s;  p s

    > Unindent
    @ TextUtil.unindent "hey you"
    `
  end

  def self.case_choices
    [
      ['upper', lambda {|o| o.upcase}],
      ['lower', lambda {|o| o.downcase}],
      ['title', lambda {|o| TextUtil.title_case(o)}],
      ['camel', lambda {|o| TextUtil.camel_case(o)}],
      ['snake', lambda {|o| TextUtil.snake_case(o)}],
      ['plus', lambda {|o| TextUtil.plus_case(o)}],
      ['hyphen', lambda {|o| TextUtil.hyphen_case(o)}],
      ]
  end

  def self.unindent txt, options={}
    txt = txt.sub(/\A\n/, '')# if ! options[:no_strip]   # Delete initial blank line if there

    txt.gsub!(/^\s+/) { |t| t.gsub("\t", '        ') }   # Untab indent

    # Does nothing if any subsequent line (not first) isn't indented
    return txt if txt[/.*?\n(.*)/m, 1] =~ /^[^\s\n]/

    # If 1st line has no indent and 2nd line has indent (at least 3 spaces)
    if txt !~ /\A / and txt =~ /\A.+\n(   +)/
      indent = $1   # Indent left by 2nd indent
      return txt.gsub(/^#{indent}/, '')
    end

    old_indent = Line.indent(txt)   # Get indent of first line

    txt.gsub!(/^#{old_indent}/, '')   # Delete current indent
    "#{txt.strip}\n"
  end

  # TextUtil.snake_case("hi there")   # -> hi_there
  def self.snake_case s
    s.gsub(/[ +-]/, '_').
      gsub(/([a-z0-9])([A-Z])/) {"#{$1}_#{$2}"}.downcase.
      gsub(/[^\w]/, "").
      gsub(/__+/, "_")
  end

  # TextUtil.plus_case("hi there")   # -> hi+there
  def self.plus_case s
    self.snake_case(s).gsub('_', '+')
  end

  def self.snake_case! s
    s.replace self.snake_case(s)
  end

  # TextUtil.hyphen_case("hi there")   # -> hi-there
  def self.hyphen_case s
    s.gsub(/[ _]/, '-').
      gsub(/([a-z])([A-Z0-9])/) {"#{$1}-#{$2}"}.downcase.
      gsub(/--+/, "-")
  end

  # TextUtil.camel_case("hi there")   # -> HiThere
  def self.camel_case s
    # If it's all capitals, make subsequent copitals lowercase
    if s =~ /^[A-Z_-]+$/
      s = s.gsub(/([A-Z][A-Z]+)/) {"#{$1.capitalize}"}
    end

    s.gsub(/[ \/-]/, '_').
      gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.
      sub(/(.)/) {$1.upcase}.
      gsub("_", "").
      gsub(/[^\w]/, "")
  end

  def self.camel_case! s
    s.replace self.camel_case(s)
  end

  # TextUtil.title_case("hi there")   # -> Hi There
  def self.title_case s, options={}
    s = s.gsub(/[ -]/, '_').
      gsub(/([a-z])([A-Z0-9])/) {"#{$1}_#{$2}"}.downcase.
      gsub(/([a-z]+)/) {"#{$1.capitalize}"}.
      gsub(/__*/, " ")

    s.gsub! " ", "_" if options[:underscores]

    s
  end

  def self.title_case! s
    s.replace self.title_case(s)
  end

end
