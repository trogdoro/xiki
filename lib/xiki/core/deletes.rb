module Xiki
  class Deletes
    def self.delete_whitespace

      prefix = Keys.prefix(:clear=>true)   # Number prefix means add that many lines after deleting

      if prefix == :u   # If U, remove whitespace within region
        txt = View.selection :delete=>true
        linebreak_on_end = txt[/\n\z/]
        txt.gsub! /^ *[+-] /, ''
        txt.gsub! /[\n\t ]/, ''
        View.insert("#{txt}#{linebreak_on_end}", :dont_move=>true)
        return
      end

      # If at end of line, go forward, and remember to delete backward
      was_blank = Line.blank?
      was_at_end = (Line.at_right? and (! was_blank))
      was_at_beginning = (Line.at_left? and (! was_blank))
      if was_blank  # If blank, stay on line
        # Do nothing
      elsif was_at_end
        $el.forward_char
      elsif was_at_beginning and not View.char =~ /\s/
        $el.backward_char
      else   # If not at end of a line, simply delete horizontal
        $el.delete_horizontal_space
        View.insert(" " * prefix) if prefix
        return
      end

      # Delete any blank lines
      $el.delete_blank_lines if Line.blank?
      $el.delete_char(1) if Line.blank?   # Delete line if left

      Deletes.backward if was_at_end
      if was_at_beginning
        Deletes.backward if Line.at_left?
        $el.delete_char(1) if Line.at_right?
      end
      if was_blank
        if prefix
          View.insert("\n" * prefix)
          Move.backward prefix
        end
      else
        $el.delete_horizontal_space
        View.insert(" " * prefix) if prefix
      end
    end


    def self.backward
      prefix = Keys.prefix
      case prefix
      when :u
        $el.backward_kill_word 1
      when :uu
        $el.backward_kill_word 2
      when :uuu
        $el.backward_kill_word 3
      else
        $el.delete_backward_char(prefix || 1)
      end
    end
  end
end
