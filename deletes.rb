class Deletes
  extend ElMixin
  def self.delete_whitespace
    # If at end of line, delete 1 char forward
    if point == Line.right and ! Line.blank?
      delete_char 1
      forward_char
    end

    # If nothing on line, delete blank lines
    if buffer_substring(point_at_bol, point_at_eol)[/^[\t ]*$/]
      delete_blank_lines
      # Kill blank line if one is left over
      kill_line if buffer_substring(point_at_bol, point_at_eol)[/^[\t ]*$/]
      # Otherwise, delete whitespace around point
    else
      delete_horizontal_space
    end
  end

  def self.backward
    prefix = Keys.prefix
    case prefix
    when :u
      backward_kill_word 1
    when :uu
      backward_kill_word 2
    when :uuu
      backward_kill_word 3
    else
      delete_backward_char(prefix || 1)
    end
  end
end
