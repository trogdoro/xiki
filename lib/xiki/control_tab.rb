# Helps you switch between files, in a way very similar to how Ctrl-tab
# switches between windows in most operating systems.  Call #keys
# to map it to Ctrl-tab in emacs.
class ControlTab

  @@edited = nil

  @@switch_index = 0
  @@dash_prefix_buffer = nil

  # Primary method.  Is mapped to C-tab and does the switching.
  def self.go

    prefix = Keys.prefix :clear=>1

    first_tab_in_sequence = Keys.before_last !~ /\btab$/   # If first tab, clear edited
    @@edited = @@dash_prefix = @@ol_prefix = @@color_prefix = @@difflog_prefix = nil if first_tab_in_sequence

    if prefix == :- || @@dash_prefix   # Go to next quote in $f

      if @@dash_prefix_buffer
        View.to_buffer @@dash_prefix_buffer   # => "files.notes"
      else
        View.layout_files :no_blink=>1
      end

      found = Move.to_quote

      if ! found
        View.to_highest   # to beginning of file
        Move.to_quote
      end

      Effects.blink
      @@dash_prefix = true


      options = {:no_recenter=>1}

      # Go to other view (leave index visible) if no bar or not in bar
      options[:other_view] = 1 if !View.bar? || !View.is_at_left


      FileTree.launch options
      return
    end

    # If C-1, C-7, C-8, C-9, step through in special ways...

    return self.go_in_outlog(prefix) if [9, 99, 1, 11].member?(prefix) || @@ol_prefix
    return self.go_in_color(prefix) if [8, 88].member?(prefix) || @@color_prefix
    return self.go_in_difflog(prefix) if [7, 77].member?(prefix) || @@difflog_prefix


      #     if prefix == 9   # Just burry buffer
      #       $el.bury_buffer
      #       # Store original order, and windows originally opened
      #       @@original = buffer_list.to_a   # Hide evidence that we were on top (lest it restore us)
      #       @@open_windows = window_list.collect {|b| window_buffer b}
      #       @@consider_test = lambda{|b| ! buffer_name(b)[/Minibuf/] }
      #       return
      #     end


    # If C-u, toggle through $f...

    if prefix == :u   # If U prefix (must be first alt-tab in sequence)
      # Go to last edited file, and store list
      @@edited = $el.elvar.editedhistory_history.to_a
      @@edited -= View.files :visible=>1   # Exclude currently visible files
      $el.find_file @@edited.shift

      return
    elsif @@edited   # Subsequent up+Control+Tab
      $el.find_file @@edited.shift
      return
    end

    # If this was the first tab in this sequence ...

    if first_tab_in_sequence# || last_was_modeline_click

      # Store original order, and windows originally opened
      @@original = $el.buffer_list.to_a
      @@open_windows = $el.window_list.collect {|b| $el.window_buffer b}

      # Based on prefix, store correct check for files to go through accordingly...

      case prefix
      when 0   # Handled above - tabs through outlog lines
        @@consider_test = lambda{|b| ! $el.buffer_file_name(b) && ! $el.buffer_name(b)[/Minibuf/]}
      when 1   # Only files
        @@consider_test = lambda{|b| $el.buffer_file_name(b)}
        #       when 3   # ...css
        #         @@consider_test = lambda{|b| buffer_name(b) =~ /\.(css|sass)/}
      when 2   # Non-files
        @@consider_test = lambda{|b| ! $el.buffer_file_name(b) && ! $el.buffer_name(b)[/Minibuf/]}
      when 3   # ...css
        @@consider_test = lambda{|b| $el.buffer_name(b) =~ /^#/}
      when 4   # haml.html files
        @@consider_test = lambda{|b| $el.buffer_file_name(b) =~ /\.html/}
      when 5   # Consoles
        @@consider_test = lambda{|b|
          next if $el.buffer_file_name b
          $el.set_buffer b
          next if $el.elvar.major_mode.to_s != 'shell-mode'
          name = $el.buffer_name b
          next if name == "*ol"
          true
        }
      when 6   # Ruby files only
        @@consider_test = lambda{|b| $el.buffer_file_name(b) =~ /\.rb$/}
      when 68   # controller
        @@consider_test = lambda{|b| $el.buffer_file_name(b) =~ /\/app\/controllers\//}
      when 66   # Models
        @@consider_test = lambda{|b| $el.buffer_file_name(b) =~ /\/app\/models\//}
      when 67   # Tests
        @@consider_test = lambda{|b| $el.buffer_file_name(b) =~ /_(spec|test)\.rb$/}

        #       when 7   # .notes files
        #         @@consider_test = lambda{|b| $el.buffer_file_name(b) =~ /\.notes$/}

        #       when 8   # Non-files
        #         @@consider_test = lambda{|b| ! $el.buffer_file_name(b) && ! $el.buffer_name(b)[/Minibuf/]}
        #       when 9   # js
        #         @@consider_test = lambda{|b| buffer_file_name(b) =~ /\.js$/}

        # when 7, 8, 9   # Handled above

      else   # Anything (except minibuffer)
        @@consider_test = lambda{|b| ! $el.buffer_name(b)[/Minibuf/] }
      end

      # Remember we're starting at the top of the buffer list
      @@switch_index = 0

      # Go to next eligible buffer
      self.move_to_next

    # If we've been typing tabs
    else
      self.restore_original_order   # Restore order up to this buffer
      self.move_to_next   # Point to next eligible buffer
    end

    $el.switch_to_buffer(@@original[@@switch_index])   # Switch to eligible
  end

  def self.go_in_color prefix
    @@color_prefix ||= prefix   # Remember to call this for subsequent tabs

    # If 88, search in all buffers...

    if @@color_prefix == 88

      # This only gets called the 1st tab in the sequence (subsequent ones are routed to Dash+Tab)

      Launcher.open 'mark/show/'

      return if View.txt =~ /^  - no marks found!/

      path = Tree.construct_path.sub(/\|.*/, '')   # Get filename we're on

      # Don't show buffers for now

      # If file we're going to is currently shown
      if View.file_visible? path
        # Don't split
      else   # Else, split!
        View.create :u
      end

      FileTree.launch :no_recenter=>1

      @@color_prefix, @@dash_prefix = nil, true

      return
    end

    # 8+Tab, so just go to next in this view...

    found = Color.next

    if ! found   # Try again if went to bottom without finding
      View.to_highest
      Color.next
    end
  end

  def self.go_in_difflog prefix
    @@difflog_prefix ||= prefix   # Remember prefix if passed in


    if prefix   # If first tab in sequence

      if View.buffer_visible? "difflog.notes"   # If already visible, just go there
        was_open = true
        View.to_buffer "difflog.notes"
      else   # Otherwise, open it and go to bottom
        DiffLog.open
      end

      Search.backward "^  - "
      first_diff_file = Tree.construct_path

      Move.to_quote

      # If 1st diff isn't todo.notes, and difflog not already open!
      if ! View.file_visible?(first_diff_file) && ! was_open
        View.create
        View.recenter(View.line - View.number_of_lines)
        View.previous
        FileTree.launch
      else
        FileTree.launch :other_view=>1
      end

    else   # Subsequent times tabbed

      View.to_buffer "difflog.notes"

      Search.backward "^  - "

      if @@difflog_prefix == 77   # User wants distinct files, grab line and search for file that's not this!
        was_here_already = Line.value

        while Line.value == was_here_already && View.cursor != 1 do
          Search.backward "^  - "
        end

      else   # Otherwise, last file is fine
        Search.backward "^  - "
      end

      Move.to_quote

      # If cursor at top of view, scroll down

      if View.scroll_position <= 2
        View.recenter -8
      end

      FileTree.launch :other_view=>1
    end
  end


  def self.go_in_outlog prefix

    @@ol_prefix ||= prefix   # Remember prefix if passed in

    View.to_buffer "*ol"
    Move.to_end

    target = @@ol_prefix == 1 ? "^ *-.*!$" : "^ *-"

    Search.forward target, :go_anyway=>1, :beginning=>true
    if View.cursor == View.bottom
      Move.to_previous_paragraph
      Search.forward target, :go_anyway=>1, :beginning=>true
    end

    value = @@ol_prefix == 99 ? Ol.grab_value(Line.value) : nil

    Effects.blink
    Color.mark "green" if @@ol_prefix == 11
    Launcher.launch_unified

    # Replace or add comment if there's a value
    if value.any?
      Ol.update_value_comment value
    end

    Color.mark "green" if @@ol_prefix == 11

    return
  end

  def self.dash_prefix_buffer= txt
    @@dash_prefix_buffer = txt
  end

  def self.restore_original_order
    # Move backwards through original list, moving each to front
    (0..(@@switch_index)).each do |i|
      $el.switch_to_buffer(@@original[@@switch_index-i])
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
        View.beep 'None left'
        break
      end

      self.to_next_unless_nil
    end
  end

  def self.to_next_unless_nil
    to_buffer = @@original[@@switch_index]
    if to_buffer.nil?
      View.beep 'None left'
    end
    $el.set_buffer(to_buffer)   # Go there so test can look at buffer mode, etc
  end

  def self.keys
    Keys.set("C-<tab>") do
      ControlTab.go
    end
  end
end
