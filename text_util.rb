require 'line'

class TextUtil
  def self.unindent txt
    # Delete initial blank line if there
    txt.sub!(/^\n/, '')

    # Untab indent
    txt.gsub!(/^\s+/) { |t| t.gsub("\t", '        ') }

    # If 1st line has no indent and 2nd line has indent (at least 3 spaces)
    if txt !~ /\A / and txt =~ /\A.+\n(   +)/
      # Indent left by 2nd indent
      indent = $1
      return txt.gsub(/^#{indent}/, '')
    end

    # Get indent of first line
    old_indent = Line.indent(txt)
    # Delete current indent
    txt.gsub!(/^#{old_indent}/, '')
    txt
  end

  def self.snake_case s
    s.gsub(/[ -]/, '_').
      gsub(/([a-z])([A-Z0-9])/) {"#{$1}_#{$2}"}.downcase.
      gsub(/__+/, "_")
  end

  def self.camel_case s
    s.gsub(/[ -]/, '_').
      gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.
      sub(/(.)/) {$1.upcase}.
      gsub("_", "")
  end

end
