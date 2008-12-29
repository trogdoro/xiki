# Starting and stopping macros
class Macros
#  include ElMixin
  extend ElMixin
  def self.record
    # If ending a macro
    if elvar.defining_kbd_macro
      end_kbd_macro nil
    # If starting a macro
    else
      start_kbd_macro nil
    end
  end

  def self.run
    # If defining a macro, just end it and run it
    if elvar.defining_kbd_macro
      end_kbd_macro nil
    end

    # If U prefix prefix, apply until blank line
    if Keys.prefix_u?
      orig = Location.new
      Line.next
      left = point
      Search.forward "^$"
      beginning_of_line
      apply_macro_to_region_lines left, point
      orig.go
      return
    elsif Keys.prefix == 0   # If 0, do to region
      $el.apply_macro_to_region_lines View.range_left, View.range_right
      return
    end

    # Run it prefix times
    call_last_kbd_macro elvar.current_prefix_arg || 1
  end
end
