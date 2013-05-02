require 'xiki/keys'

# Provides different ways of moving cursor.
class Move

  # Go to last line having indent
  def self.to_indent
    direction_down = true   # Assume down
    prefix = Keys.prefix
    line = Line.value

    if prefix.is_a? Fixnum   # If U, reverse
      indent = prefix   # If numeric, make that be the indent
    else
      if prefix == :u   # If U, reverse
        direction_down = false
      else
      end
    end

    column = View.column
    indent ||= Line.indent(line).size

    # If negative, reverse direction and amke positive
    if indent < 0
      direction_down = false
      indent = 0 - indent
    end

    orig = Location.new
    # Search for matching in right direction
    if direction_down == false
      Line.to_left
      success = Search.backward "^ \\{#{indent}\\}[^ \t\n]"
    else
      Line.next
      success = Search.forward "^ \\{#{indent}\\}[^ \t\n]"
    end

    Move.to_column prefix.is_a?(Fixnum) ? indent : column

    unless success
      View.beep
      orig.go
    end
  end

  def self.to_next_paragraph options={}
    prefix = Keys.prefix :clear=>1
    if prefix == :u || prefix == :uu   # If C-u, just go to end
      if Line.blank?
        Line.value(2) =~ /./ ?   # If on blank line but next line has stuff, just move down
          Line.next :
          Move.to_next_paragraph
      end

      Search.forward "^[ \t]*$", :go_anyway=>1

      Line.previous if prefix == :uu

      Move.to_axis
      return
    end

    return Line.next if options[:no_skip] && Line.blank? && Line.value(2) =~ /./   # Don't skip if right before paragraph

    prefix = prefix.is_a?(Fixnum) ? prefix : 1
    prefix.times do
      Search.forward "\n[ \t]*\\(\n+[ \t]*\\)+", :go_anyway=>1
    end
    $el.beginning_of_line
  end

  def self.to_previous_paragraph
    prefix = Keys.prefix :clear=>1
    if prefix == :u   # If C-u, just go to end

      # If line before us isn't blank, move past this paragraph first
      Move.to_previous_paragraph if Line.value(0) =~ /./

      Search.backward ".", :go_anyway=>1   # Go to non-blank line
      Line.next
      return
    end

    prefix = prefix.is_a?(Fixnum) ? prefix : 1

    prefix.times do
      $el.skip_chars_backward "\n "
      $el.re_search_backward "\n[ \t]*\\(\n+[ \t]*\\)+", nil, 1
    end
    $el.skip_chars_forward "\n "
    $el.beginning_of_line
  end

  def self.to_window n, options={}

    views = View.list   # Get views in this window

    if n >= views.size   # If they wanted to go further than exists
      $el.select_window(views[views.size - 1])
    else
      $el.select_window(views[n-1])
    end

    Effects.blink(:what=>:line) if options[:blink]
  end

  def self.to_last_window options={}

    View.to_nth(View.list.size - 1)

    Effects.blink(:what=>:line) if options[:blink]
  end

  def self.to_line n=nil
    # Use arg or numeric prefix or get input
    n = n || $el.elvar.current_prefix_arg || Keys.input(:prompt=>"Go to line number: ")
    $el.goto_line n.to_i
  end

  # Move to the specified column.
  def self.to_column n=nil

    prefix = Keys.prefix :clear=>1
    # If dash+, go to specific char
    if prefix == :- && ! n   # Don't prompt if called with a param
      return View.cursor = Keys.input(:prompt=>"Point to go to: ", :timed=>1).to_i
    end

    n = n || prefix || Keys.input(:prompt=>"column to go to: ").to_i
    if n < 0
      Move.to_end
      n = $el.abs(n)
      n > length = Line.txt.length and n = length
      Move.backward n
      return
    end
    $el.move_to_column n# - 1
  end

  # Go to opposite bracket
  def self.to_other_bracket
    prefix = Keys.prefix
    # If prefix or after closing bracket, go backward
    last_char = $el.point == 1 ? "" : $el.buffer_substring($el.point-1, $el.point)

    # If numeric prefix
    if prefix.class == Fixnum
      if prefix > 0
        prefix.times { $el.forward_sexp }
      else
        (0-prefix).times { $el.backward_sexp }
      end
    elsif prefix == :u or last_char =~ /[)}\]'">]/
      $el.backward_sexp
    # Otherwise, go forward
    else
      $el.forward_sexp
    end
  end

  def self.backward count=nil
    count ||= Keys.prefix# :clear => true
    count ||= 1
    case count
    when :u; $el.backward_word 1
    when :uu; $el.backward_word 2
    when :uuu; $el.backward_word 3
    else
      $el.backward_char count
    end
  end

  def self.forward count=nil

    count ||= Keys.prefix# :clear => true
    count ||= 1
    case count
    when :u
      $el.forward_word 1
    when :uu
      $el.forward_word 2
    when :uuu
      $el.forward_word 3
    else
      $el.forward_char(count) rescue nil   # In case tried to move past end
    end
  end

  def self.top
    $el.beginning_of_buffer
  end

  def self.bottom
    $el.end_of_buffer
  end

  def self.to_quote
    prefix = Keys.prefix :clear=>true
    if prefix.nil?
      times = 1
      Keys.clear_prefix
    elsif prefix.is_a? Fixnum
      times = prefix
      View.to_relative
    end

    found = nil
    (times||1).times do
      Line.next if Line.matches(/^ *(\||:[ +-])/)
      found = Search.forward "^ *\\(|\\|:[ +-]\\)"
      return nil if ! found
      $el.backward_char
    end

    found

    # If on a quote, move off
  end

  # Move to file in tree (not dir) ?
  def self.to_junior
    Keys.prefix_times.times do
      # Move to line without / at end
      Line.next if Line.matches(/^ +[+-]? ?[a-zA-Z_-].+[^\/\n]$/)
      $el.re_search_forward "^ +[+-]? ?[a-zA-Z_-].+[^\/\n]$"
      Line.to_words
    end
  rescue Exception=>e
    # Do nothing, because there may be no junior
  end

  # Moves cursor to left of line:
  #
  # Move.to_axis
  # Move.to_axiss
  def self.to_axis
    n = Keys.prefix_n   # Check for numeric prefix
    Line.next(n) if n.is_a? Fixnum   # If there, move down
    Line.to_left
  end

  # Moves cursor to left of line:
  #
  # Move.to_end
  def self.to_end n=nil
    n ||= Keys.prefix_n   # Check for numeric prefix
    Line.next(n) if n.is_a? Fixnum   # If there, move down
    Line.to_right
  end

end
