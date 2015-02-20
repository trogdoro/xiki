module Xiki
  # Helps you switch between files, in a way very similar to how Ctrl-tab
  # switches between windows in most operating systems.  Call #keys
  # to map it to Ctrl-tab in emacs.
  class ControlTab

    @@edited = nil

    @@switch_index = 0
    @@dash_prefix_buffer, @@dash_prefix, @@ol_prefix, @@color_prefix, @@difflog_prefix = nil, nil, nil, nil, nil
    @@open_windows = nil
    @@original = []

    @@clear_once = nil
    @@last_escape_was_something_else = nil

    def self.clear_once
      @@clear_once = 1
    end

    # Primary method.  Is mapped to C-tab and does the switching.
    def self.go options={}

      # Selection exists, so just deselect
      return View.deselect if View.selection?

      # In minibuffer, so just escape out

      return $el.keyboard_escape_quit if View.name =~ /^ \*Minibuf/


      # They pressed escape to get here, so maybe quit or kill...

      if options[:from_escape]
        views_open = Buffers.list.map { |o| name = $el.buffer_name(o) }.select { |o| o !~ /^ ?\*/ && o != "views/" }

        # In a filter or hotkey search, so do nothing
        if $el.boundp :xiki_filter_just_finished
          @@last_escape_was_something_else = 1

          # Return (gracefully stop search) if it's a permanent-named view, and it's not xsh with only one other view
            # (if xsh and just one other view, it should continue)

          return if View.name !~ /\/$/ && ! (View.name == "xsh" && views_open.length == 1)

          # This was the 1st search done in this temporary view, so kill view

          View.kill if $el.elvar.xiki_filter_count == 1

          DiffLog.quit if views_open.length == 1

          return

        end

        # No other view open (aside from *... and views/), so just quit
        return DiffLog.quit if views_open.length == 1

      end

      Keys.remember_key_for_repeat(proc {ControlTab.go :subsequent=>1}, :movement=>1)

      prefix = Keys.prefix :clear=>1
      recent_few = Keys.recent_few

      first_tab_in_sequence = true
      # It'll be the first tab, if the keys before the last one weren't C-\, \ or double escape

      last_key_was_escape = recent_few[1] == 27

      last_key_was_escape = nil if @@last_escape_was_something_else
      @@last_escape_was_something_else = nil   # Reset, since no longer relevant

      # Trying out > just single escape
      first_tab_in_sequence = false if last_key_was_escape   # One before last was / or C-/
      first_tab_in_sequence = false if recent_few[1] == 28 || recent_few[1] == 92   # One before last was / or C-/

      first_tab_in_sequence = nil if options[:subsequent]





      if @@clear_once   # If .clear_once was called recently
        first_tab_in_sequence = true
        @@clear_once = nil
      end

      @@edited = @@dash_prefix = @@ol_prefix = @@color_prefix = @@difflog_prefix = nil if first_tab_in_sequence

      if prefix == :- || @@dash_prefix   # Go to next quote in :n

        if @@dash_prefix_buffer
          View.to_buffer @@dash_prefix_buffer   # => "nav.notes"
        else
          View.layout_nav :no_blink=>1
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

        Launcher.launch
        return
      end

      # If C-1, C-7, C-8, C-9, step through in special ways...

      return self.go_in_outlog(prefix) if [9, 99, 1, 11, 111].member?(prefix) || @@ol_prefix
      return self.go_in_color(prefix) if [8, 88].member?(prefix) || @@color_prefix
      return self.go_in_difflog(prefix) if [7, 77].member?(prefix) || @@difflog_prefix

      # If C-u, toggle through :n...

      if prefix == :u   # If U prefix (must be first alt-tab in sequence)
        # Go to last edited file, and store list
        #         @@edited = $el.elvar.editedhistory_history.to_a
        @@edited = DiffLog.file_list
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

          # when 1   # Handled above

        when 2   # .notes files
          @@consider_test = lambda{|b| $el.buffer_name(b) =~ /\.notes[<>0-9]*$/}

          #         when 1   # Only files
          #           @@consider_test = lambda{|b| $el.buffer_file_name(b)}
          #       when 3   # ...css
          #         @@consider_test = lambda{|b| buffer_name(b) =~ /\.(css|sass)/}
          #         when 2   # Non-files
          #           @@consider_test = lambda{|b| ! $el.buffer_file_name(b) && ! $el.buffer_name(b)[/Minibuf/]}
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

        else   # Anything (except minibuffer)
          @@consider_test = lambda{ |b|
            $el.buffer_name(b) !~ /^ ?(Minibuf|\*)/
          }
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

      if @@original == :at_end
        return Launcher.open("views/")
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

        Louncher.launch :no_recenter=>1

        @@color_prefix, @@dash_prefix = nil, true

        return
      end

      # 8+Tab, so just go to next in this view...

      column = View.column
      found = Color.next

      if ! found   # Try again if went to bottom without finding
        View.to_highest
        Color.next :column=>column
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

        Move.to_quote :pipes=>1

        # If 1st diff isn't tasks.notes, and difflog not already open!
        if ! View.file_visible?(first_diff_file) && ! was_open
          View.create
          View.recenter(View.line - View.number_of_lines)
          View.previous
          Launcher.launch
        else
          Launcher.launch :other_view=>1
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

        Move.to_quote :pipes=>1

        # If cursor at top of view, scroll down

        if View.scroll_position <= 2
          View.recenter -8
        end

        Launcher.launch :other_view=>1
      end
    end


    def self.go_in_outlog prefix

      @@ol_prefix ||= prefix   # Remember prefix if passed in

      View.to_buffer "*ol"
      Move.to_end

      target =
        if @@ol_prefix == 1; "^ *-.*!$"
        elsif @@ol_prefix == 11; "^ *-.* check!$"
        else; "^ *-"
        end

      Search.forward target, :go_anyway=>1, :beginning=>true
      if View.cursor == View.bottom
        Move.to_previous_paragraph
        Search.forward target, :go_anyway=>1, :beginning=>true
      end

      value = @@ol_prefix == 99 ? Ol.grab_value(Line.value) : nil

      Effects.blink
      Color.mark "green" if @@ol_prefix == 111
      Launcher.launch

      # Replace or add comment if there's a value
      if value.any? && value != "!"
        Ol.update_value_comment value
      end

      Color.mark "green" if @@ol_prefix == 111

      return
    end

    def self.dash_prefix_buffer= txt
      @@dash_prefix_buffer = txt
    end

    def self.restore_original_order
      return if ! @@original.is_a?(Hash)

      # Move backwards through original list, moving each to front
      (0..(@@switch_index)).each do |i|
        $el.switch_to_buffer(@@original[@@switch_index-i])
      end
    end

    # Advances @@switch_index to next eligible buffer
    def self.move_to_next

      buffer_started_at = @@switch_index

      @@switch_index += 1   # Move to next

      return if @@original == :at_end

      result = self.to_next_unless_nil   # Go there so test can look at buffer mode, etc

      # Keep moving until we find an eligible buffer (that isn't already viewed)
      while(
          @@open_windows.member?(@@original[@@switch_index]) ||   # Already viewed
          ! @@consider_test[@@original[@@switch_index]]   # Not what we're looking for
      )
        @@switch_index += 1

        # Stop moving forward if we're at end
        if @@switch_index >= @@original.size
          @@switch_index = buffer_started_at
          return @@original = :at_end   # To indicate to stop
        end

        result = self.to_next_unless_nil
      end
    end

    def self.to_next_unless_nil
      to_buffer = @@original[@@switch_index]
      if to_buffer.nil?
        return :at_end
      end
      $el.set_buffer(to_buffer)   # Go there so test can look at buffer mode, etc
    end

  end
end
