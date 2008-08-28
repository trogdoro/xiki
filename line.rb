class Line
  extend ElMixin

  CODE_SAMPLES = %q<
    # Moving
    - up: Line.previous
    - down: Line.next

    # Getting text
    - line text: p Line.value
    - indent: p Line.indent
      - without indent: p Line.without_indent
    - label: p Line.label
      - without label: p Line.without_label

    # Getting Positions
    - line start: p Line.left
    - line ending: p Line.right

    # Tests
    - blank: p Line.blank?
    - matches something: p Line.matches(/s...thing/)
  >

  # Text on current line (minus linebreak)
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

  def self.without_indent
    self.value.sub(/^\s+/, "")
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

  def self.to_next times=nil
    self.next times
  end

  def self.to_previous times=nil
    self.previous times
  end

  def self.next times=nil
    forward_line times
  end

  def self.previous times=nil
    times = times ?
      -times :
      -1
    forward_line times
  end

  def self.beginning
    forward_line 0
    #beginning_of_line
  end

  class << self
    alias :start :beginning
    #alias :to_left :beginning
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

  def self.label line=nil
    line ||= self.value
    # Space or blank can follow colon
    label = line[/^\s*- (.+?): /, 1]
    label ||= line[/^\s*- (.+?):$/, 1]
  end

  def self.without_label line=nil
    line ||= self.value
    line[/^\s*- .+?: (.*)/, 1] ||
    line[/^\s*- .+?:()$/, 1] ||  # Blank if colon at end
    line[/^\s*- (.+)/, 1] ||
    line.sub(/^ */, '')
  end

  def self.is_bullet? line=nil
    line ||= self.value
    self.matches /^\s*- /, line
  end

  def self.number
    line_number_at_pos
  end

  def self.to_blank
    re_search_forward "^[ \t]*$"
  end

end
