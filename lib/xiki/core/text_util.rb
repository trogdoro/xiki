require 'xiki/core/line'
require 'xiki/core/ol'

module Xiki
  class TextUtil

    def self.case_choices
      [
        ['upper', lambda {|o| o.upcase}],
        ['lower', lambda {|o| o.downcase}],
        ['title', lambda {|o| TextUtil.title_case(o)}],
        ['camel', lambda {|o| TextUtil.camel_case(o)}],
        ['snake', lambda {|o| TextUtil.snake_case(o)}],
        ['plus', lambda {|o| TextUtil.plus_case(o)}],
        ['hyphen', lambda {|o| TextUtil.hyphen_case(o)}],
        ['regex', lambda {|o| TextUtil.regex_case(o)}],
        ['whitespace', lambda {|o| TextUtil.whitespace_case(o)}],
      ]
    end

    def self.unindent txt, options={}
      txt = txt.sub(/\A\n/, '')# if ! options[:no_strip]   # Delete initial blank line if there

      txt.gsub!(/^\s+/) { |t| t.gsub("\t", '        ') }   # Untab indent

      # Does nothing if any subsequent line (not first) isn't indented
      if txt[/.*?\n(.*)/m, 1] =~ /^[^\s\n]/
        # Just add linebreak if none there, for consistent handling
        return txt =~ /\n\z/ ? txt : "#{txt}\n"
      end

      # If 1st line has no indent and 2nd line has indent (at least 3 spaces)
      if txt !~ /\A / and txt =~ /\A.+\n(   +)/
        indent = $1   # Indent left by 2nd indent
        return txt.gsub(/^#{indent}/, '')
      end

      old_indent = Line.indent(txt)   # Get indent of first line

      txt.gsub!(/^#{old_indent}/, '')   # Delete current indent

      txt.sub! /\n +\z/, ''   # Delete spaces after final linebreak

      txt.sub!(/\A\n+/, '')
      txt.sub!(/\n+\z/, '')
      "#{txt}\n"

    end

    # TextUtil.snake_case("hi there")   # -> hi_there
    def self.snake s
      self.snake_case s
    end
    def self.snake_case s
      s.gsub(/[ \/+-]/, '_').
        gsub(/([a-z0-9])([A-Z])/) {"#{$1}_#{$2}"}.downcase.
        gsub(/[^\w]/, "").
        gsub(/__+/, "_").
        gsub(/_$/, "")
    end

    # Just words and spaces
    # TextUtil.word_case("hi there")   # -> hi there
    def self.word_case s
      self.snake_case(s).gsub('_', ' ')
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

    # TextUtil.regex_case("hi there")   # -> hi-there
    def self.regex_case s
      s.gsub(/[^a-z0-9]/i, '.').
        gsub(/([a-z])([A-Z0-9])/) {"#{$1}.#{$2}"}.downcase.
        gsub(/\.\.+/, ".")
    end

    # TextUtil.whitespace_case("hi-there")
    #   hi there
    def self.whitespace_case txt
      txt.gsub(/[._-]/, ' ')
    end

    # TextUtil.camel_case("hi there")   # -> HiThere
    def self.camel s
      self.camel_case s
    end

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

    def self.word_wrap txt, max=64
      words = txt.split(/ /)
      txt = ""
      words.each do |w|
        # If it would make last line longer than max make new line
        last_line = txt[/.*\z/]
        txt.sub!(/ ?\z/, "\n") if last_line.length + w.length > max

        txt << "#{w} "   # Add word
      end
      txt.strip
    end

    # TextUtil.ap({1=>2}).inspect
    #   "1 => 2"
    def self.ap txt
      txt = txt.ai
      txt.sub! /\A\{\n/, ''
      txt.sub! /\n\}\z/, ''
      txt.gsub! /^  /, ""
      txt.sub! "[\n  [", "[["
      txt
    end

    # p TextUtil["1 => 2"]
    #   {1=>2}
    def self.[] txt
      txt = "{#{txt}}"
      eval txt
    end

    # TextUtil.symbolize_hash_keys({"hi"=>"you"}).inspect
    def self.symbolize_hash_keys hash
      hash.keys.inject({}) {|new_hash, key|
        new_hash[key.to_sym] = hash[key]
        new_hash
      }
    end

    def self.parse_time txt
      txt.sub(/ .*/, '').sub(/^:/, '0:').sub(/^\d+$/, "\\0:00")
    end


    def self.regexp_escape txt
      txt = Regexp.escape txt   # Do standard escape
      # Then, remove backslashes before spaces...
      result, slash_escaped = "", false
      txt.split(//).each do |c|

        # Last was slash
        if slash_escaped
          slash_escaped = false   # Stop remembering escape
          next result.<< " " if c == " "   # Current is space, so just do space
          # Current is something else, so add it with slash
          next result.<< "\\#{c}"
        end
        # Current is slash, so just remember and do nothing
        next slash_escaped = true if c == "\\"
        # Normal unescaped char, so just append
        result.<< c
      end
      result
    end

  end
end
