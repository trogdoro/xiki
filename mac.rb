# Starting and stopping mac
class Mac

  def self.menu
    %`
    | Define standard mac shortcuts
    - .define_keys/
    - important dirs/
      - @/Users/
      - @/Applications/
      - @/Applications/Utilities/
    `
  end

  def self.define_keys
    self.keys
    View.glow "- defined Command-C, Command-V, Command-Q, etc."
  end

  def self.keys
    Keys._N { View.to_buffer "untitled"; $el.rename_uniquely }

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

    $el.define_key :global_map, $el.kbd("M-X"), :execute_extended_command
  end
end
