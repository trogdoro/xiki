require 'line'
require 'ol'

class TextUtil

  def self.case_choices
    [
      ['upper', lambda {|o| o.upcase}],
      ['lower', lambda {|o| o.downcase}],
      ['title', lambda {|o| TextUtil.title_case(o)}],
      ['camel', lambda {|o| TextUtil.camel_case(o)}],
      ['snake', lambda {|o| TextUtil.snake_case(o)}],
      ['hyphen', lambda {|o| TextUtil.hyphen_case(o)}],
      ]
  end

  def self.unindent txt, options={}
    txt.sub!(/\A\n/, '')# if ! options[:no_strip]   # Delete initial blank line if there

    txt.gsub!(/^\s+/) { |t| t.gsub("\t", '        ') }   # Untab indent

    # If 1st line has no indent and 2nd line has indent (at least 3 spaces)
    if txt !~ /\A / and txt =~ /\A.+\n(   +)/
      indent = $1   # Indent left by 2nd indent
      return txt.gsub(/^#{indent}/, '')
    end

    old_indent = Line.indent(txt)   # Get indent of first line

    txt.gsub!(/^#{old_indent}/, '')   # Delete current indent
    "#{txt.strip}\n"
  end

  def self.snake_case s
    s.gsub(/[ -]/, '_').
      gsub(/([a-z0-9])([A-Z])/) {"#{$1}_#{$2}"}.downcase.
      gsub(/[^\w]/, "").
      gsub(/__+/, "_")
  end

  def self.snake_case! s
    s.replace self.snake_case(s)
  end

  def self.hyphen_case s
    s.gsub(/[ _]/, '-').
      gsub(/([a-z])([A-Z0-9])/) {"#{$1}-#{$2}"}.downcase.
      gsub(/--+/, "-")
  end

  def self.camel_case s
    # If it's all capitals, make subsequent copitals lowercase
    if s =~ /^[A-Z_-]+$/
      s = s.gsub(/([A-Z][A-Z]+)/) {"#{$1.capitalize}"}
    end

    s.gsub(/[ -]/, '_').
      gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.
      sub(/(.)/) {$1.upcase}.
      gsub("_", "").
      gsub(/[^\w]/, "")
  end

  def self.camel_case! s
    s.replace self.camel_case(s)
  end

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
