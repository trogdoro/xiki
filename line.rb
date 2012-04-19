class Line
  extend ElMixin

  def self.menu
    "
    - api/
      > Moving
      @ Line.previous   # up
      @ Line.next   # down
      |
      > Getting text
      @ p Line.value   # line text
      @ p Line.indent   # indent
      @ p Line.without_indent   # without indent
      @ p Line.label   # label
      @ p Line.without_label   # without label
      |
      > Getting Positions
      @ p Line.left   # line start
      @ p Line.right   # line ending
      |
      > Tests
      @ p Line.blank?   # blank
      @ Line[/s...thing/]   # matches something
    "
  end

  # Text on current line (minus linebreak)
  def self.[] regex, index=0
    self.value[regex, index]
  end

  def self.value n=1, options={}
    eol = "(point-at-eol #{n})"

    # Optionally include linebreak
    eol = "(+ 1 #{eol})" if options[:include_linebreak]

    result = el4r_lisp_eval("(buffer-substring (point-at-bol #{n}) #{eol})")

    if options[:delete]
      el4r_lisp_eval("(delete-region (point-at-bol #{n}) #{eol})")
    end

    result
  end

  # Whether line matches pattern
  def self.matches o, line=nil
    line ||= self.value
    line[o]
  end

  # Return value of next line
  def self.next_value n=2
    #buffer_substring(point_at_bol(n), point_at_eol(n))
  end

  # Whether next line matches pattern
  def self.next_matches pattern
    self.next_value =~ pattern
  end

  def self.indent line=nil
    line ||= self.value
    line[/^\s*/].gsub("\t", '        ')
  end

  def self.right
    point_at_eol
  end

  def self.at_right
    point_at_eol == point
  end

  def self.blank?
    self.matches /^$/
  end

  def self.left down=1
    point_at_bol down
  end

  def self.at_left
    point_at_bol == point
  end

  def self.without_indent txt=nil
    txt ||= self.value
    txt.sub(/^\s+/, "")
  end

  def self.txt
    self.without_indent
  end

  def self.bounds
    [point_at_bol, point_at_eol]
  end

  def self.delete leave_linebreak=nil
    value = self.value
    bol, eol = point_at_bol, point_at_eol
    eol += 1 unless leave_linebreak
    delete_region(bol, eol)
    value
  end

  # Gets symbol at point
  def self.symbol options={}
    symbol = thing_at_point(:symbol)
    # Delete if option passed
    if options[:delete]
      delete_region(* bounds_of_thing_at_point(:symbol))
    end
    symbol
  end

  # Moves down, going to first column
  def self.to_next times=nil
    self.next times
  end

  # Moves up, going to first column
  def self.to_previous times=nil
    self.previous times
  end

  # Moves down, going to first column
  def self.next times=nil
    forward_line times
    nil
  end

  # Moves up, going to first column
  def self.previous times=nil
    times = times ?
      -times :
      -1
    forward_line times
    nil
  end

  def self.beginning
    forward_line 0
    nil
  end

  def self.to_left
    self.beginning
  end

  def self.end
    end_of_line
  end

  def self.to_right
    end_of_line
  end

  def self.to_end
    self.to_right
  end

  def self.to_start
    self.to_left
  end

  def self.to_words
    beginning_of_line
    skip_chars_forward "[^ \t]"
  end

  def self.to_beginning options={}
    down = options[:down]
    prefix = options[:prefix] || Keys.prefix
    down ||= prefix

    # If prefix go down n lines first
    Line.next down if down.is_a? Fixnum

    Line.to_left

    prefix == :u || options[:quote]?
      $el.skip_chars_forward("[^ \t]") :
      $el.skip_chars_forward("[^ \t|]")   # If quoted, skip quote unless :u

    nil
  end


  def self.label line=nil
    line ||= self.value
    # Space or blank can follow colon
    label = line[/^\s*[+-] (.+?): /, 1]
    label ||= line[/^\s*[+-] (.+?):$/, 1]
  end

  def self.without_label options={}
    line = options[:line] || self.value

    # Delete comment (parenthesis)
    line = line.sub /^(\s*)(?:[+-]|<+) [^\n\(]+\) (.*)/, "\\1\\2"

    # If paren at end of line, delete label
    line.sub! /^(\s*)(?:[+-]|<+) [^\n\(]+?\)$/, "\\1"
    # If just bullet
    line.sub! /^(\s*)(?:[+-]|<+) (.+)/, "\\1\\2"
    # Remove whitespace by default
    line.sub!(/^ */, '') unless options[:leave_indent]

    line
  end

  def self.=~ regex
    self.value =~ regex
  end

  class << self
    alias :start :beginning
    alias :content :without_label
  end

  def self.is_bullet? line=nil
    line ||= self.value
    self.matches /^\s*[+-] /, line
  end

  def self.number pos=nil
    $el.xiki_line_number pos || $el.point
  end

  def self.to_blank
    re_search_forward "^[ \t]*$"
  end

  def self.duplicate_line
    column = View.column

    line = "#{Line.value}\n"
    Line.to_left
    prefix = Keys.prefix
    Code.comment(:line) if prefix == :u
    times = if prefix.nil?
        1
      elsif prefix == :u
        1   # Put commented line after
        # 0   # Put commented line before
      elsif prefix == 0
        0
      elsif prefix > 0
        prefix + 1
      elsif prefix < 0
        prefix
      end

    Line.next times
    View.insert line
    Line.previous

    View.column = column
  end

  def self.move direction
    column = View.column
    many = Keys.prefix_times

    many = (0 - many) if direction == :previous
    line = Line.value 1, :include_linebreak => true, :delete => true   # Get line
    Line.to_next many
    View.insert line

    Line.previous
    View.column = column
  end

  def self.sub! from, to
    orig = Location.new
    value = Line.value
    return unless value.sub! from, to
    self.delete :leave
    self.insert value
    orig.go :assume_file=>1
    value
  end

  def self.gsub! from, to
    orig = Location.new
    value = Line.value
    return unless value.gsub! from, to
    self.delete :leave
    self.insert value
    orig.go :assume_file=>1
    value
  end

  def self.<< txt
    Keys.clear_prefix
    Move.to_end
    View.insert txt
  end

  def self.< txt
    self.delete :leave
    self.insert txt
  end

  def self.add_slash
    Line << "/" unless Line =~ /\/$/
    Move.to_end
  end

  def self.before_cursor
    View.txt self.left, View.cursor
  end

  def self.init
    # Define lisp function to get list of displayed lines
    # In case something has been done to change them
    el4r_lisp_eval %q[
      (defun xiki-line-number (pos)
        (save-excursion
          (goto-char pos)
          (forward-line 0)
          (1+ (count-lines 1 (point))))
      )
    ]
  end

end

Line.init
