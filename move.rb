require 'keys'

# Provides different ways of moving cursor.
class Move
  extend ElMixin

  # Go to last line having indent
  def self.to_indent
    direction_down = true   # Assume down
    if Keys.prefix_u   # If U, reverse
      direction_down = false
    else
      column = Keys.prefix   # If numeric, make that be the indent
    end

    line = Line.value
    # If end of block, reverse direction
    if line =~ /^ *(end|\]|\}|\))$/
      direction_down = ! direction_down
    end

    column ||= Line.indent(line).size

    # If negative, reverse direction and amke positive
    if column < 0
      direction_down = false
      column = 0 - column
    end

    orig = Location.new
    # Search for matching in right direction
    if direction_down == false
      Line.to_left
      success = Search.backward "^ \\{#{column}\\}[^ \t\n]"
      Move.to_column column
    else
      Line.next
      success = Search.forward "^ \\{#{column}\\}[^ \t\n]"
      Move.to_column column
    end
    unless success
      View.beep
      orig.go
    end
  end

  #   def self.to_indent
  #     indent = Keys.prefix

  #     # If ending, to matching (reversing if U)
  #     if Line.matches(/^ *(end|\]|\}|\))$/)
  #       return Keys.prefix_u? ? self.to_same_indent : self.to_same_indent(:up)
  #     elsif Keys.prefix_u?   # If U, go up
  #       return self.to_same_indent(:up)
  #     elsif indent == nil   # If no numeric prefix, go down to arg
  #       return self.to_same_indent
  #     end

  #     # If numeric prefix, go to last one of same
  #     spaces = " " * indent.abs
  #     orig = Line.number
  #     # If negative, go backwards
  #     indent >= 0 ? re_search_forward(/^#{spaces}[a-zA-Z<>{}|\/+-]/) : re_search_backward(/^#{spaces}[a-zA-Z<>{}-]/)
  #     # If still there, move forward and do again
  #     if Line.number == orig
  #       Line.next
  #       indent >= 0 ? re_search_forward(/^#{spaces}[a-zA-Z<>{}|\/+-]/) : re_search_backward(/^#{spaces}[a-zA-Z<>{}-]/)
  #     end
  #     move_to_column(indent.abs)
  #   end

  #   # Move down to the next line that is indented to the column in which 
  #   # the cursor currontly is.
  #   def self.to_same_indent up=false
  #     indent = Line.indent.size

  #     if up
  #       Line.to_left
  #       Search.backward "^ \\{#{indent}\\}[^ \n]"
  #     else
  #       Line.next
  #       Search.forward "^ \\{#{indent}\\}[^ \n]"
  #     end

  #     Move.to_line_text_beginning

  #   end

  def self.to_next_paragraph
    pref = Keys.prefix || 1
    if Keys.prefix_u?  # If C-u, just go to end
      re_search_forward "^[ \t]*$", nil, 1
      beginning_of_line
      return
    end
    pref.times do
      re_search_forward "\n[ \t]*\\(\n+[ \t]*\\)+", nil, 1
    end
    beginning_of_line
  end

  def self.to_previous_paragraph
    pref = elvar.current_prefix_arg || 1
    pref.times do
      skip_chars_backward "\n "
      re_search_backward "\n[ \t]*\\(\n+[ \t]*\\)+", nil, 1
  #    search_backward_regexp "\n\n+"
    end
    skip_chars_forward "\n "
    beginning_of_line
  end

  def self.to_window n, options={}

    # Get views in this window
    views = window_list(window_frame(frame_first_window), true, frame_first_window).to_ary

    # If they wanted to go further than exists
    if n >= views.size
      select_window(views[views.size - 2])
    else
      select_window(views[n-1])
    end

    Effects.blink(:what=>:line) if options[:blink]
  end

  def self.to_line n=nil
    # Use arg or numeric prefix or get input
    n = n || elvar.current_prefix_arg || Keys.input
    goto_line n.to_i
  end

  # Move to the specified column.
  def self.to_column n=nil
    n = n || elvar.current_prefix_arg || Keys.input(:prompt => "Enter number of column to go to: ").to_i
    move_to_column n# - 1
  end

  def self.to_line_text_beginning down=nil
    down ||= Keys.prefix_n
    # If prefix go down n lines first
    Line.next down if down

    Line.to_left
    skip_chars_forward "[^ \t]"
  end

  # Go to opposite bracket
  def self.to_other_bracket
    prefix = Keys.prefix
    # If prefix or after closing bracket, go backward
    last_char = point == 1 ? "" : buffer_substring(point-1, point)

    # If numeric prefix
    if prefix.class == Fixnum
      if prefix > 0
        prefix.times { forward_sexp }
      else
        (0-prefix).times { backward_sexp }
      end
    elsif prefix == :u or last_char =~ /[)}\]'">]/
      backward_sexp
    # Otherwise, go forward
    else
      forward_sexp
    end
  end

  def self.backward count=nil
    count ||= Keys.prefix :clear => true
    count ||= 1
    case count
    when :u; backward_word 1
    when :uu; backward_word 2
    when :uuu; backward_word 3
    else
      backward_char(count)
    end
  end

  def self.forward count=nil

    count ||= Keys.prefix :clear => true
    count ||= 1
    case count
    when :u
      forward_word 1
    when :uu
      forward_word 2
    when :uuu
      forward_word 3
    else
      forward_char(count)
    end
  end

  def self.top
    beginning_of_buffer
  end

  def self.bottom
    end_of_buffer
  end

  def self.to_quote
    # If on a quote, move off
    Line.next if Line.matches(/^ *\|/)
    re_search_forward "^ +|"
    backward_char
  end

  # Move to file in tree (not dir) ?
  def self.to_junior
    Keys.prefix_times.times do
      # Move to line without / at end
      Line.next if Line.matches(/^ +[+-]? ?[a-zA-Z_-].+[^\/\n]$/)
      re_search_forward "^ +[+-]? ?[a-zA-Z_-].+[^\/\n]$"
      Line.to_words
    end
  end

  def self.to_axis
    n = Keys.prefix_n   # Check for numeric prefix
    # If there, move down
    if n > 0
      Line.next
    end

  end
end
