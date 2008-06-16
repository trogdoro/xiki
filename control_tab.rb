# Helps you switch between files, in a way very similar to how Ctrl-tab
# switches between windows in most operating systems.  Call #keys
# to map it to Ctrl-tab in emacs.
class ControlTab
  extend ElMixin

  @@edited = nil

#  @@switch_index = 0

  # Primary method.  Is mapped to C-tab and does the switching.
  def self.go
#     puts recent_keys.to_s
#     ml recent_keys.class
#     ml "ControlTab: 2"
#     ml recent_keys.to_s
#     ml "ControlTab: 3"

    last_char_typed = recent_keys.to_s[/\S+ (\S+) \S+\]$/, 1]
#    puts "ary: #{recent_keys.to_a.inspect}"

# puts "last: #{$el.pp_to_string(recent_keys.to_a.last)}"
# puts "last was modeline click: #{$el.pp_to_string(recent_keys.to_a.last) =~ /^\(mouse-1\n.+mode-line$/}"
# puts "last_char_typed: #{last_char_typed}"
    #last_was_modeline_click = $el.pp_to_string(recent_keys.to_a.last) =~ /^\(mouse-1\n.+mode-line$/

    first_tab_in_sequence = ! last_char_typed[/tab$/]

    # If first tab, clear edited
    @@edited = nil if first_tab_in_sequence

    if Keys.prefix_u   # If U prefix (must be first alt-tab in sequence)
      # Go to last edited file, and store list
      @@edited = elvar.editedhistory_history.to_a
      #ml edited.size
      @@edited -= View.list_files
      #ml edited.size
      find_file @@edited.shift
      # Remove currently viewed
      #View.list_files
      #ml elvar.editedhistory_history.to_a[0]

      return
    elsif @@edited   # Subsequent alt-tab
      find_file @@edited.shift
      return
    end

#     ml "ControlTab: 2.0"
    # If this was the first tab in this sequence
    if first_tab_in_sequence# || last_was_modeline_click

      # Store original order, and windows originally opened
      @@original = buffer_list.to_a
      @@open_windows = window_list.collect {|b| window_buffer b}

      puts "ControlTab: prefix was #{Keys.prefix}"

      # Check for prefix, and store correct test for files to go through accordingly
      case Keys.prefix
      when 0
        # Not dirs or files
        @@consider_test = lambda{|b| ! buffer_file_name(b) && ! buffer_name(b)[/Minibuf/] && ! elvar.mode_name[/^Dired/] }
      when 1
        # Files only
        @@consider_test = lambda{|b| buffer_file_name(b)}
      when 2
        # Dirs only
        @@consider_test = lambda{|b| elvar.mode_name[/^Dired/] }
      when 3
        # ...trees only
        @@consider_test = lambda{|b| buffer_name(b) =~ /^\*\*tree /}
      when 4
        # .notes files
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.notes$/}
      when 5
        # .rhtml files
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.(html\.haml|html\.erb|rhtml)$/}
      when 6
        # Ruby files only
        @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.rb$/}
      when 8
        # Shells ("shell" or "**" in buffer name)
        @@consider_test = lambda{|b| name = buffer_name(b);  (name[/\*shell/i] || name[/\*/]) && ! buffer_name(b)[/Minibuf/] }
      else
        puts "ControlTab: 4"

        if View.in_bar?  # If in the bar, only notes or trees
          @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.notes$/ ||
            buffer_name(b) =~ /^\*\*tree/ ||
            buffer_name(b) =~ /^\*\*CodeTree/
            }
        else
          # Any buffer but minibuffer
          @@consider_test = lambda{|b| ! buffer_name(b)[/Minibuf/] }
        end
      end

#       # If we're not on a file, just switch to a file
#       if ! buffer_file_name
#         ControlTab.switch_to_nth 1
#       # Otherwise, go to next file
#       else

      puts "ControlTab: 5"

      # Remember we're starting at the top of the buffer list
      @@switch_index = 0

      puts "ControlTab: 6"

      # Go to next eligible buffer
      self.move_to_next

      puts "ControlTab: 7"

      switch_to_buffer(@@original[@@switch_index])

    # If we've been typing tabs
    else
      self.restore_original_order   # Restore order up to this buffer
      self.move_to_next   # Point to next eligible buffer
      switch_to_buffer(@@original[@@switch_index])  # Switch to eligible
      #insert @@switch_index.to_s

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

    @@switch_index += 1   # Move to next
    set_buffer(@@original[@@switch_index])  # Go there so test can look at buffer mode, etc
    # Keep moving until we find an eligible buffer (that isn't already viewed)
    while(
        @@open_windows.member?(@@original[@@switch_index]) ||   # Already viewed
        ! @@consider_test[@@original[@@switch_index]]   # Not what we're looking for
    )
      @@switch_index += 1

      # Stop moving forward if we're at end
      break if @@switch_index >= @@original.size

      set_buffer(@@original[@@switch_index])  # Go there so test can look at buffer mode, etc
    end
  end

#   def self.switch_to_nth n
#     # Switch to nth file
#     found = 0
#     buffer_list.to_a.each do |b|

#       found += 1
#       if found == n
#         switch_to_buffer b
#         break
#       end
#     end
#   end

  def self.keys

#     # Redundantly map tab to indent (so tab will be mapped separately to C-i)
#     el4r_lisp_eval %q[
#       (define-key global-map (kbd "<tab>") 'indent-for-tab-command)
#       ]
    Keys.set("C-<tab>") do
      ControlTab.go
    end
  end
end
