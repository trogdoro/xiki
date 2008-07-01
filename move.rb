require 'keys'

# Provides different ways of moving cursor.
class Move
  extend ElMixin

  # Go to last line having indent
  def self.to_indent
    indent = Keys.prefix
    # If no prefix, go down to same indent as this one
    if Keys.prefix_u || Line.matches(/^ *(end|\]|\}|\))$/)
      return self.to_same_indent(:up)
    elsif indent == nil
      return self.to_same_indent
    end
    # If numeric prefix, go to last one of same
    spaces = " " * indent.abs
    orig = Line.number
    # If negative, go backwards
    indent >= 0 ? re_search_forward(/^#{spaces}[a-zA-Z<>{}|\/-]/) : re_search_backward(/^#{spaces}[a-zA-Z<>{}-]/)
    # If still there, move forward and do again
    if Line.number == orig
      Line.next
      indent >= 0 ? re_search_forward(/^#{spaces}[a-zA-Z<>{}|\/-]/) : re_search_backward(/^#{spaces}[a-zA-Z<>{}-]/)
    end
    move_to_column(indent.abs)
  end

  # Move down to the next line that is indented to the column in which 
  # the cursor currontly is.
  def self.to_same_indent up=false
    c = current_column
    # Store current column
    orig_indent = Line.indent.length
    while((up ? Line.previous : Line.next) == 0)
      line = Line.value
      indent = Line.indent(line).length
      # Found if same indent, and at least one char after indent
      break if indent == orig_indent && line.length > indent# && line !~ /^\s*#/
    end
    move_to_column c
  end

  def self.to_next_paragraph
    pref = Keys.prefix || 1
    if Keys.prefix_u  # If C-u, just go to end
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

  def self.to_window n

    # Get views in this window
    views = window_list(window_frame(frame_first_window), true, frame_first_window).to_ary

    # If they wanted to go further than exists
    if n >= views.size
      select_window(views[views.size - 2])
    else
      select_window(views[n-1])
    end
  end

  def self.to_line n=nil
    # Use arg or numeric prefix or get input
    n = n || elvar.current_prefix_arg || Keys.input
    goto_line n.to_i
  end

  # Move to the specified column.
  def self.to_column n=nil
    n = n || elvar.current_prefix_arg || Keys.input(:prompt => "Enter number of column to go to: ").to_i
    move_to_column n - 1
  end

  def self.to_line_text_beginning
    beginning_of_line
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
    when :u; backward_kill_word 1
    when :uu; backward_kill_word 2
    when :uuu; backward_kill_word 3
    else
      backward_char(count)
    end
  end

  def self.forward count=nil
    count ||= Keys.prefix :clear => true
    count ||= 1
    case count
    when :u
      kill_word 1
    when :uu
      kill_word 2
    when :uuu
      kill_word 3
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

  def self.to_junior
    Keys.prefix_times.times do
      # Move to line without / at end
      Line.next if Line.matches(/^ +[a-zA-Z-].+[^\/\n]$/)
      re_search_forward "^ +[a-zA-Z-].+[^\/\n]$"
      Line.to_words
    end
  end
end
