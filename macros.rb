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

    # If 0 prefix, apply until blank line
    if Keys.prefix == 0 or Keys.prefix_u?
      beginning_of_line
      next_line
      top = point
      re_search_forward "^[ \t]*$", nil, 1
      beginning_of_line
      apply_macro_to_region_lines top, point
      return
    end

    # Run it prefix times
    call_last_kbd_macro elvar.current_prefix_arg || 1
  end
end
