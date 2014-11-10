module Xiki
  # Starting and stopping macros
  class Macros

    def self.menu
      "
      docs/
        > Keys
        | as+job - Start or stop recording macro
        | do+job - Run macro (the last recorded one)
        | up+do+job - Stop recording and apply macro to any following contiguous lines
      "
    end

    def self.record
      # If ending a macro
      if $el.elvar.defining_kbd_macro
        $el.end_kbd_macro nil
      # If starting a macro
      else
        $el.start_kbd_macro nil
      end
    end

    def self.run
      # If defining a macro, just end it and run it
      if $el.elvar.defining_kbd_macro
        $el.end_kbd_macro nil
      end

      # If U prefix prefix, apply until blank line
      if Keys.prefix_u?
        orig = Location.new
        Line.next
        left = $el.point
        Search.forward "^$"
        $el.beginning_of_line
        $el.apply_macro_to_region_lines left, $el.point
        orig.go
        return
      elsif Keys.prefix == 0   # If 0, do to region
        $el.apply_macro_to_region_lines View.range_left, View.range_right
        return
      end

      # Run it prefix times
      $el.call_last_kbd_macro $el.elvar.current_prefix_arg || 1

      # Make C-. run macro again (macros might have changed this)

      Keys.remember_key_for_repeat(proc {Macros.run})

    end
  end
end
