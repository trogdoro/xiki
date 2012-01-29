# Starting and stopping mac
class Mac

  def self.menu
    %`
    - Enable mac keyboard shortcuts/
      | Enable macintosh-like keyboard shortcuts, like command-c
      @ Mac.define_keys
    - Important dirs/
      @/Users/
      @/Applications/
      @/Applications/Utilities/
      @ ~/Library/Fonts/
    - api/
      | Define standard mac shortcuts
      @ Mac.define_keys
    `
  end

  def self.define_keys
    self.keys
    View.flash "- defined Command-C, Command-V, Command-Q, etc."
  end

  def self.keys
    Keys._N do
      View.to_buffer "untitled"
      $el.rename_uniquely
      Notes.mode
      View.>> "\n"
    end

    Keys._Q { $el.save_buffers_kill_emacs }   # Command-Q to quit
    Keys._C {
      left, right = View.range
      Effects.blink :left=>left, :right=>right
      $el.kill_ring_save left, right
      }   # Command-C
    Keys._V { $el.yank }   # Command-V
    Keys._A { View.to_highest; Clipboard.copy_everything }   # Command-V

    Keys._X {
      left, right = View.range
      Effects.blink :left=>left, :right=>right
      $el.kill_region left, right
    }   # Command-V

    Keys._S { DiffLog.save }   # save (or, with prefix, save as) **

    $el.define_key :global_map, $el.kbd("M-X"), :execute_extended_command
  end
end
