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

  #
  # Use this if you're using aquamacs
  #
  # Put this line in your init.rb file to use the below key mappings.
  #
  #   Mac.keys_for_aquamacs
  #
  def self.keys_for_aquamacs

    # Changes aquamacs behavior so it opens in same frame.  Is there
    # an aquamacs setting for this that could be used instead of redefining
    # key?
    $el.define_key :osx_key_mode_map, $el.kbd("A-n") do
      View.to_buffer "untitled"
      $el.rename_uniquely
      Notes.mode
      View.>> "\n"
    end

  end

  #
  # Use this if you're not using aquamacs
  #
  # Put this line in your init.rb file to use the below key mappings.
  #
  #   Mac.keys
  #
  def self.keys

    #   def self.keys
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
    }

    Keys._S { DiffLog.save }   # save (or, with prefix, save as) **

    $el.define_key :global_map, $el.kbd("M-X"), :execute_extended_command
  end
end
