class Deletes
  extend ElMixin
  def self.delete_whitespace

    # If at end of line, go forward, and remember to delete backward
    was_blank = Line.blank?
    was_at_end = (Line.at_right and (! was_blank))
    was_at_beginning = (Line.at_left and (! was_blank))
    if was_blank  # If blank, stay on line
      # Do nothing
    elsif was_at_end
      forward_char
    elsif was_at_beginning and not View.char =~ /\s/
      backward_char
    else   # If not at end of a line, simply delete horizontal
      return delete_horizontal_space
    end

    # Delete any blank lines
    delete_blank_lines if Line.blank?
    delete_char(1) if Line.blank?   # Delete line if left

    Deletes.backward if was_at_end
    if was_at_beginning
      Deletes.backward if Line.at_left
      delete_char(1) if Line.at_right
    end
    delete_horizontal_space unless was_blank
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
