# Helps you switch between files, in a way very similar to how Ctrl-tab
# switches between windows in most operating systems.  Call #keys
# to map it to Ctrl-tab in emacs.
class ControlTab
  extend ElMixin

  @@edited = nil

  @@switch_index = 0

  # Primary method.  Is mapped to C-tab and does the switching.
  def self.go
    prefix = Keys.prefix

    if prefix == 6   # If C-0 prefix, just burry buffer
      $el.bury_buffer
      # Store original order, and windows originally opened
      @@original = buffer_list.to_a   # Hide evidence that we were on top (lest it restore us)
      @@open_windows = window_list.collect {|b| window_buffer b}
      @@consider_test = lambda{|b| ! buffer_name(b)[/Minibuf/] }
      return
    end

    first_tab_in_sequence = Keys.before_last !~ /\btab$/   # If first tab, clear edited

    @@edited = nil if first_tab_in_sequence

    if prefix == :u   # If U prefix (must be first alt-tab in sequence)
      # Go to last edited file, and store list
      @@edited = elvar.editedhistory_history.to_a
      @@edited -= View.list_files
      find_file @@edited.shift

      return
    elsif @@edited   # Subsequent alt-tab
      find_file @@edited.shift
      return
    end

    # If this was the first tab in this sequence
    if first_tab_in_sequence# || last_was_modeline_click

      # Store original order, and windows originally opened
      @@original = buffer_list.to_a
      @@open_windows = window_list.collect {|b| window_buffer b}

      # Check for prefix, and store correct test for files to go through accordingly
      case prefix
      when 0   # Non-files
        @@consider_test = lambda{|b| ! buffer_file_name(b) && ! buffer_name(b)[/Minibuf/]}

      when 1   # Only files
        @@consider_test = lambda{|b| buffer_file_name(b)}
      when 3   # ...css
        @@consider_test = lambda{|b| buffer_name(b) =~ /\.(css|sass)/}
      when 4   # Consoles
        @@consider_test = lambda{|b| buffer_name(b) =~ /^(\*console|\*merb) /i}
      when 5   # .rhtml files
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.html/}
      when 6   # Ruby files only
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.rb$/}
      when 68   # controller
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\/app\/controllers\//}
      when 66   # Models
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\/app\/models\//}
      when 60   # Tests
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /_(spec|test)\.rb$/}
      when 7   # .notes files
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.notes$/}
      when 8   # Anything (just like no arg)
        @@consider_test = lambda{|b| ! buffer_name(b)[/Minibuf/] }
      when 9   # js
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.js$/}
      else   # Anything (except minibuffer)
        @@consider_test = lambda{|b| ! buffer_name(b)[/Minibuf/] }
      end

      # Remember we're starting at the top of the buffer list
      @@switch_index = 0

      # Go to next eligible buffer
      self.move_to_next

      switch_to_buffer(@@original[@@switch_index])

    # If we've been typing tabs
    else
      self.restore_original_order   # Restore order up to this buffer
      self.move_to_next   # Point to next eligible buffer
      switch_to_buffer(@@original[@@switch_index])  # Switch to eligible

    end
  end

  def self.restore_original_order
    # Move backwards through original list, moving each to front
    (0..(@@switch_index)).each do |i|
      switch_to_buffer(@@original[@@switch_index-i])
    end
  end

  # Advances @@switch_index to next eligible buffer
  def self.move_to_next
    buffer_started_at = @@switch_index

    @@switch_index += 1   # Move to next
    self.to_next_unless_nil   # Go there so test can look at buffer mode, etc

    # Keep moving until we find an eligible buffer (that isn't already viewed)
    while(
        @@open_windows.member?(@@original[@@switch_index]) ||   # Already viewed
        ! @@consider_test[@@original[@@switch_index]]   # Not what we're looking for
    )
      @@switch_index += 1

      # Stop moving forward if we're at end
      if @@switch_index >= @@original.size
        @@switch_index = buffer_started_at
        #         View.to_buffer buffer_started_at
        View.beep;  View.message('None left')
        break
      end

      self.to_next_unless_nil
    end
  end

  def self.to_next_unless_nil
    to_buffer = @@original[@@switch_index]
    if to_buffer.nil?
      View.beep
      return View.message('None left')
    end
    $el.set_buffer(to_buffer)   # Go there so test can look at buffer mode, etc
  end

  def self.keys
    Keys.set("C-<tab>") do
      ControlTab.go
    end
  end
end
