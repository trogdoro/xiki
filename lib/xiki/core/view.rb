# -*- coding: utf-8 -*-

module Xiki
  #
  # Represents a division of a window (in emacs terms, it's a window (which is within a frame))
  #
  class View

    def self.menu
      %`
      << @window/
      - .flashes/
      - api/
        > Summary
        | View class is the catch-all class for dealing with editing text.
        | It has methods for inserting text and grabbing text, and quite a few
        | other things.

        > Text
        @View << 'Hello'    # Inserts into the view.
        @p View.txt         # Return all the text.
        @p View.txt 1, 5    # Return text in a range.
        @p View.cursor      # Return where the cursor is.
        @p View.line        # Returns the line number the cursor is on.

        > Files and dirs
        @p View.name                  # Returns file name.
        @p View.file                  # Returns file with whole path.
        @p View.dir                   # Returns dir of file.
        @View.open "/tmp/"            # Opens file in the first view, (switching to it if it's already open)
        @View.open nil, :txt=>"Hi"    # Show message in new buffer.

        > Messages
        @View.flash                 # Shows temporary message inline.
        @View.flash 'Saved'
        @View.message 'Hi there'    # Shows message at bottom.
        @View.prompt                # Prompt user to type at end af line.
        @View.prompt 'Type here'
        @View.beep                  # Makes a noise.

        > Advanced
        @View.<< "hey", :dont_move=>1    # Without moving cursor

        > Also see
        << line/

      - docs/
        > Summary
        | Menus to deal with the layout, etc.

        - Keys/
          | layout+create - TODO should we just refer to a @layout menu for these?
      `
    end

    # Stores things user copies
    @@hash = {}

    def self.windows_in_my_column
      my_left = left_edge
      $el.window_list(nil, nil, $el.frame_first_window).to_a.select { |w| left_edge(w) == my_left }
    end

    # Make current window larger.  Take into account that there might be other vertical windows
    def self.height window=nil
      return $el.window_height(window) if window

      $el.window_height
    end

    def self.heights

      View.list.map do |w|
        self.height w
      end
    end

    def self.frame_height= chars
      $el.set_frame_parameter nil, :height, chars
    end

    def self.frame_width= chars
      $el.set_frame_parameter nil, :width, chars
    end

    def self.frame_height options={}
      return $el.frame_parameter(nil, :height) if options.empty?
      return unless options.is_a?(Hash)

      if options[:add]
        self.frame_height = self.frame_height + 1
      end

      nil
    end

    def self.frame_width options={}
      return $el.frame_parameter(nil, :width) if options.empty?
      return unless options.is_a?(Hash)

      if options[:add]
        self.frame_width = self.frame_width + 1
      end

      nil
    end

    def self.screen_width options={}
      $el.x_display_pixel_width
    end

    def self.screen_height options={}
      $el.x_display_pixel_height
    end


    def self.enlarge height=nil

      default_height = 3
      small = Keys.prefix || height || default_height
      small = default_height if small == :u

      small += 1

      # If universal prefix and in bar, widen bar
      self.balance if Keys.prefix_u and View.bar?

      ws = self.windows_in_my_column

      wnum = ws.length   # Get number of windows

      usable_height = $el.frame_height - 1 - wnum

      biggest = usable_height - ((wnum-1) * (small-1))
      selected = $el.selected_window


      # Remember borders, and get rid of external border temporarily (it messes up heights)
      $el.elvar.xiki_modeline_box_tmp = Styles.attribute "mode-line", "box"
      $el.elvar.xiki_modeline_inactive_box_tmp = Styles.attribute "mode-line-inactive", "box"
      Styles.mode_line_inactive :border=>:nil
      Styles.mode_line :border=>:nil

      # Maybe have to redraw here?!
      View.refresh

      # Set border to 0

      self.enlarge_internal :up, ws, selected, biggest, small

      # Restore borders
      $el.set_face_attribute :mode_line, nil, ':box'.to_sym, $el.elvar.xiki_modeline_box_tmp
      $el.set_face_attribute :mode_line_inactive, nil, ':box'.to_sym, $el.elvar.xiki_modeline_inactive_box_tmp

      nil
    end

    def self.enlarge_internal direction, ws, selected, biggest, small
      ws = ws.reverse if direction != :up

      3.times do
        ws.each_with_index do |w, i|
          # If current window, set to remaining
          if w == selected
            height = biggest
            $el.set_window_text_height w, height
          else  # Other small windows
            height = small - 1
            $el.set_window_text_height w, height
          end
        end
      end

    end

    # Creates a new window by splitting the current one
    def self.create_vertical options={}
      $el.split_window_horizontally

      # up+, so make existing view be at left...

      if options[:prefix] == :u ||  Keys.prefix_u
        ControlTab.go
        View.next
        return
      end

      # Make existing view be at right...

      View.next
      ControlTab.go

    end

    def self.create_horizontal prefix=nil
      prefix ||= Keys.prefix

      # Xsh and first time we've split, so change theme to default...

      if prefix == :u
        $el.split_window_vertically
      elsif prefix == 4
        $el.split_window_vertically
        Keys.clear_prefix
        View.next
        View.enlarge
      else   # Normal split
        $el.split_window_vertically
        View.next
        ControlTab.go
      end
    end

    def self.show_txt txt, options={}

      View.to_buffer(options[:name] || "message")
      View.kill_all
      Notes.mode
      # View << "#{txt.strip}\n"
      View << "#{txt.unindent}\n"
      View.to_highest
      if options[:line_found]
        View.line = options[:line_found]
        Line.to_words
      end
      nil
    end

    # Opens file (or whatever) from the path (can contain $bookmarks), just
    # moving to or exposing its view if it's already open.
    # By default it will open in 2nd view if we're in the bar view.
    #
    # View.open "/etc/paths"
    # View.open "hello", :txt=>"> Hi\nMessage to show in new buffer."
    # View.open "/etc/paths", :to=>"local"
    def self.open path, options={}
      raise ".open called with nil path" if ! path

      # :unique option, so make sure name is unique
      path = View.unique_name path if options[:unique]

      # \n in path, so use it as the contents...

      if path =~ /\n/
        path, options[:txt] = "command", path
      end

      path, options = nil, path if path.is_a?(Hash)

      return self.show_txt(options[:txt], {:name=>path}.merge(options)) if options[:txt]

      # Pull off line number if there
      path = path.sub(/(.+?:\d+).*/, "\\1")
      line_number = path.slice!(/:\d+$/)

      # Open after bar if in bar
      if View.in_bar? && (! options[:stay_in_bar]) && path != "$0" && path != Bookmarks['%n'] && path != Bookmarks['%links']
        View.to_after_bar
      end

      # Expand ^bookmark strings at beginning

      # Was ~..., so remember old path to use when opening (expanded will be used for comparing to existing views, etc.)
      # Don't expand ~ to home dir
      if path =~ /^~/
        expanded_tilde = path
      end

      # expanded = Bookmarks.expand(path) # if path !~ /^~/
      expanded = File.expand_path Bookmarks.expand(path) # if path !~ /^~/

      if expanded == ""   # If nothing there, return false
        return nil
      end

      # Open it in appropriate view...

      # If :other_view and not already visible, and other views in our column
      if options[:other_view] && ! View.file_visible?(expanded) && View.windows_in_my_column.length > 1
        # If we're at the top, go to next
        Keys.clear_prefix
        View.windows_in_my_column.index(View.current) == 0 ?
          View.next : View.previous
        # Else go to previous
        # possibly move to a window...
        # Do nothing if already visible
      end

      if options[:same_view]
        $el.find_file expanded
      elsif expanded == $el.buffer_file_name
        # If already there, do nothing...

      elsif ( ( $el.window_list.collect {|b| $el.window_buffer b} ).collect {|u| $el.buffer_file_name u} ).member?(expanded)
        # If already displayed, just move to where it is...

        $el.find_file_other_window expanded   # This function reuses the window it's already open in
      else
        # If not visible, just open it...

        if File.directory? expanded   # If a dir, open it in new buffer
          expanded = FileTree.add_slash_maybe expanded

          return Launcher.open (expanded_tilde || expanded), options.merge(:buffer_name=>"#{expanded[/([^\/]*)\/$/]}", :buffer_dir=>expanded)
        end

        $el.find_file expanded
      end

      if to = options[:to]
        if to.is_a? Integer
          View.line = to
        else
          View.to_highest
          Search.forward to
          Line.to_beginning
        end
      end

      if line_number
        View.to_line line_number[/\d+/]
      end

      nil
    end

    # Saves the configuration
    def self.save name=nil
      name ||= Keys.input(:optional=>true)
      name ||= "0"
      @@hash[name] = $el.current_window_configuration
    end

    # Saves the configuration
    def self.restore name=nil
      name ||= Keys.input(:optional=>true)   # Get single key from user if no param
      name ||= "0"   # Set to "0" if user waited too long and entered nothing
      if(name == "o")   # Todo: if "o", just show :t and $o
        View.hide_others :all=>1
        View.open "%n"
        View.create_horizontal
        Code.open_log_view
        View.previous
        return
      end

      if(name == "l")   # Todo: if "l", winner_undo
        $el.winner_undo
        return
      end
      # Use it to get configuration out of hash
      $el.set_window_configuration(@@hash[name])
    end

    # Return list of windows (buffers that are visible)

    def self.list
      $el.window_list(nil, nil, $el.frame_first_window).to_a
    end

    # Return list of windows (buffers that are visible)

    def self.list_names
      self.list.map{|v| $el.buffer_name $el.window_buffer(v)}
    end

    def self.visible_files
      self.files :visible=>1
    end

    def self.files options={}
      if options[:visible]
        return $el.window_list.
          map {|b| $el.window_buffer b}.
          collect {|u| $el.buffer_file_name u}.
          select {|f| f}
      end

      Buffers.list.map { |b| $el.buffer_file_name(b) }.select{|path| path}
    end

    # Move to nth window
    def self.to_nth_paragraph n
      View.to_relative :line=>1

      if Line.blank? && ! Line.value(2).blank?   # Weird case when just one blank line at top
        n -= 1
        Line.next if n == 0
      end

      n.times { Move.to_next_paragraph }
    end

    # Move to nth window
    def self.to_nth_fraction n
      height = View.height
      lines_down = height * (n / 10.0)
      lines_down = lines_down.floor

      View.to_relative :line=>1
      Line.next lines_down
    end

    def self.to_nth n
      # If greater than size of windows, open last
      #insert self.list.size.to_s
      if n+1 > self.list.size
        return $el.select_window(self.list.last)
      end
      # Otherwise, open nth
      $el.select_window(self.list[n])
    end

    def self.[] n
      self.nth n
    end
    def self.nth n
      orig = View.window
      View.to_nth n
      win = View.window
      View.to_window orig
      win
    end

    def self.file_of_nth n
      orig = View.window
      View.to_nth n
      file = View.file
      View.to_window orig
      file
    end

    def self.buffer_of_nth n
      window = View.window
      View.to_nth n
      buffer = $el.window_buffer
      View.to_window window
      buffer
    end

    def self.dir_of_nth n
      window = View.window
      View.to_nth n
      dir = View.dir
      View.to_window window
      dir
    end

    def self.dir_of_after_bar
      window = View.window
      View.to_after_bar
      dir = View.dir
      View.to_window window
      dir
    end

    # Open the bar
    def self.bar
      # If bar already there, just go to it
      if self.bar?
        View.to_nth(0)
        return
      end

      # Remember buffers and heights
      orig = []
      self.list.each do |w|
        orig << [$el.window_buffer(w), $el.window_height(w)]
      end
      $el.delete_other_windows

      # Width of bar
      $el.split_window_horizontally

      $el.other_window 1
      o = nil
      # For each window but last
      orig[0..-2].each do |w|
        $el.switch_to_buffer w[0]
        $el.split_window_vertically
        $el.set_window_text_height nil, w[1]
        o = $el.other_window 1
      end
      # Last window
      $el.switch_to_buffer orig[-1][0]
      $el.set_window_text_height o, orig[-1][1] if o
      # Go to first window
      $el.select_window self.first
    end

    # Returns whether bar is open
    def self.bar?
      $el.window_width(self.first) < $el.frame_width
    end

    # Returns whether we're in the bar
    def self.in_bar?
      self.bar? &&  # Bar is open
        View.edges[0] == 0  # Window is at left of frame
    end

    def self.first
      $el.frame_first_window
    end

    def self.last
      View.list[-1]
    end

    # Accounts for bar
    def self.balance options={}

      # No vertical split, so just resize...

      $el.balance_windows

      return if ! self.bar?

      # Only continue if up+ or flag, or
      return if ! Keys.prefix_u && ! options[:narrow_bar]# && ! heights_didnt_change

      self.balance_with_bar

    end

    def self.balance_with_bar

      buffer = $el.selected_window
      $el.select_window $el.frame_first_window

      # Width of bar (balance)
      $el.enlarge_window((42 - $el.window_width), true)

      $el.select_window buffer

    end

    # Returns true if anything is hidden
    def self.hidden?
      $el.point_min != 1 || $el.point_max != $el.buffer_size+1
    end

    def self.hide options={}

      size = View.list.size

      # It's the only window, so kill the view
      if size == 1
        View.kill if options[:kill_if_only]
        return
      end

      Keys.prefix_times.times do
        left = View.left_edge

        # If there's one above me and before me
        index = View.index
        middle = false
        if index > 0 && index < (size - 1)   # Check existance
          if( left == View.left_edge(View.list[index - 1]) &&
            left == View.left_edge(View.list[index + 1]) )  # Check alignment
            middle = true
          end
        end

        # If I'm the last
        last = index == (size - 1)

        $el.delete_window

        # Go back if we were at the last frame
        # Only do in emacs 22.  Emacs 24 does this already (or only aquamacs?)
        if Emacs.v22
          $el.previous_multiframe_window if View.left_edge != left || middle || last
        end

      end
      nil
    end

    # Keys: layout+all
    def self.hide_others options={}

      was_at_left = self.is_at_left

      # In a special circumstance, so delete all other windows...

      delete_all = false
      # We're in the bar, so delete all other windows
      delete_all = true if $el.elvar.current_prefix_arg || self.in_bar? || options[:all]
      # If there are windows to the left, but none above or below
      delete_all = true if ! was_at_left && View.windows_in_my_column.length == 1
      return $el.delete_other_windows if delete_all

      # Only delete windows above and below...

      selected = View.current

      self.windows_in_my_column.each do |w|
        next if w == selected
        $el.delete_window w
      end

      # If wasn't at left, but at left now, go to right
      View.to_upper if ! was_at_left && self.is_at_left
    end

    def self.next options={}
      (Keys.prefix_times || options[:times] || 1).times do
        $el.other_window 1
      end
      Effects.blink(:what=>:line) if options[:blink]
      rescue
        # Do nothing
    end

    def self.previous options={}
      (Keys.prefix_times || options[:times] || 1).times do
        $el.other_window -1
      end
      Effects.blink(:what=>:line) if options[:blink]
    end

    def self.show_dir
      ($el.elvar.current_prefix_arg || 1).times do
        $el.dired_jump
      end
    end

    # Return selected text (aka the "region")
    def self.selection?
      $el.elvar.mark_active
    end

    def self.selection options={}
      return nil if ! self.selection? && ! options[:always]

      txt = $el.buffer_substring($el.region_beginning, $el.region_end)
      $el.delete_region($el.point, $el.mark) if options[:delete]
      txt
    end

    def self.selection= coords
      left, right = coords

      View.mark = right
      View.cursor = left

      $el.elvar.deactivate_mark = nil
    end

    # Starting and ending of selection
    def self.range
      # Avoids "mark not set" error, when view recently opened
      return [nil, nil] if ! $el.mark
      [$el.region_beginning, $el.region_end]
    end

    def self.range_left
      $el.region_beginning
    end

    def self.range_right
      $el.region_end
    end

    def self.buffer
      $el.window_buffer
    end

    # Return currently-selected window
    def self.current
      $el.selected_window
    end

    # Move to window
    def self.to_window window
      $el.select_window(window)
    end

    def self.open_in_bar
      # Remember buffer
      buffer = self.buffer

      # If already open, just go there
      if View.bar?
        $el.select_window(View.first)
      else
        View.bar
      end
      View.to_buffer buffer
    end

    def self.open_in_right
      # Remember buffer
      buffer = self.buffer

      View.bar unless View.bar?  # If not open yet, open it
      # Go to after bar
      View.to_after_bar
    end

    def self.handle_bar
      self.to_after_bar if self.in_bar?
    end

    def self.to_upper options={}

      return Move.to_last_window(:blink=>true) if Keys.prefix_u

      down = Keys.prefix_times - 1
      Keys.clear_prefix
      View.to_after_bar

      # If there's only one column (last view is at left), go to top
      Move.to_window(1) if self.edges[0] == 0
      down.times { View.next }
      Effects.blink(:what=>:line) if options[:blink]
    end

    def self.to_after_bar
      return unless self.bar?

      # Get width of last window
      width_of_last = $el.window_width(self.last)

      # Go to first window not on left margin
      self.list.each do |w|
        if self.left_edge(w) != 0  # Window is at left of frame
          $el.select_window(w)
          break
        end
      end
    end

    def self.is_at_left
      self.left_edge == 0
    end

    def self.edges view=nil
      view ||= self.current
      $el.window_edges(view).to_a
    end

    def self.left_edge view=nil
      self.edges(view)[0]
    end

    def self.top_edge view=nil
      view ||= $el.selected_window  # Default to current view
      self.edges(view)[1]
    end

    # Switches to a buffer
    def self.to_view name, options={}
      self.to_buffer name, options
    end

    def self.to_buffer name, options={}

      name = View.unique_name name if options[:unique]

      # Ol.stack
      if $el.buffer_name == name   # If we're here already, do nothing
        View.clear if options[:clear]
        return
      end

      View.to_after_bar if options[:after_bar] && View.in_bar?

      found = View.list.find do |w|
        $el.buffer_name($el.window_buffer(w)) == name
      end
      found ? View.to_window(found) : $el.switch_to_buffer(name)

      View.clear if options[:clear]
      View.dir = options[:dir] if options[:dir]
    end

    def self.txt options={}, right=nil
      options.is_a?(Hash) && buffer = options[:buffer] and return Buffers.txt(buffer)

      # If 2nd arg is there, we were passed right,left
      if right
        left = options
      else
        left = options[:left] || $el.point_min
        right = options[:right] || $el.point_max
      end

      # If :utf8 option, write to file via elisp and read via ruby (for correct encoding)
      if options.is_a?(Hash) && options[:utf8]
        # Write to file via elisp
        $el.write_region left, right, "/tmp/utf.txt", nil

        # Read via ruby (so correct encoding is obtained)
        return File.read("/tmp/utf.txt")
      end

      $el.buffer_substring left, right
    end

    # Returns text from view according to prefix...
    # - 3 means 3 lines, etc.
    # - no prefix means the notes block
    # - etc
    def self.txt_per_prefix prefix=nil, options={}

      prefix ||= Keys.prefix(:clear=>1)

      prefix = prefix.abs if prefix.is_a?(Fixnum)
      left, right = [nil, nil]

      case prefix
      when 0   # Do paragraph
        left, right = View.paragraph(:bounds=>true)
      when 1..6   # Should probably catch all numeric prefix?
        left = Line.left
        right = $el.point_at_bol(prefix+1)
      end

      left, right = View.range if View.selection?

      # If no prefixes
      if left == nil
        if options[:default_is_line]
          left, right = [Line.left, Line.right]
        else
          ignore, left, right = View.block_positions("^>")
        end
      end

      Effects.blink(:left=>left, :right=>right) if options[:blink]
      txt = options[:just_positions] ? nil : View.txt(left, right)

      if options[:remove_heading] && txt =~ /^>/
        txt.sub! /.+\n/, ''
        # left won't be fixed, but who cares, for now
      end

      return txt if options[:just_txt]

      return [txt, left, right]
    end


    def self.part tile, options={}

      context_lines = options[:context] || 0

      txt = ""

      # If we're in the buffer already, save the cursor, because with_current_buffer won't restore it in this case
      cursor_to_restore = tile == View.buffer ? View.cursor : nil

      $el.with(:with_current_buffer, tile) do

        orig = View.line

        # Line above exists, to grab it...

        context_lines.times do
          break if View.line == 1
          Line.previous
          txt = "| #{Line.value}\n#{txt}"
        end

        # Grab original line...
        View.line = orig

        txt << "| #{Line.value}\n"

        # Line below exists, so grab it...

        context_lines.times do
          Line.next
          break if View.line == orig
          txt << "| #{Line.value}\n"
          orig = View.line
        end

        View.cursor = cursor_to_restore if cursor_to_restore
      end

      txt.gsub /^([|:]) $/, "\\1"

    end



    # Returns bounds of block in the form [left, after_header, right].
    def self.block_positions regex="^> "

      orig = $el.point
      # Go to the end of the line, so if we're at the heading we'll find it
      Line.end
      found = $el.re_search_backward regex, nil, 1
      if found
        left = $el.point
        after_header = Line.left 2
      else
        left = after_header = $el.point
      end
      Line.end
      $el.re_search_forward regex, nil, 1
      right = ($el.point == $el.point_max) ? $el.point_max : Line.left
      $el.goto_char orig

      [left, after_header, right]

    end

    def self.to_highest
      prefix = Keys.prefix
      return self.to_line(prefix) if prefix.is_a?(Fixnum)   # If prefix, go to that line
      self.to_top
    end

    def self.to_top
      $el.beginning_of_buffer
    end

    def self.to_bottom
      prefix = Keys.prefix :clear=>1
      $el.end_of_buffer
      Line.previous prefix if prefix.is_a? Fixnum
      View.message ""
    end

    def self.at_bottom
      self.cursor == self.bottom
    end

    def self.to_end
      $el.end_of_buffer
    end

    def self.beginning
      $el.point_min
    end

    def self.top
      $el.point_min
    end

    def self.bottom
      $el.point_max
    end

    def self.at_top?
      self.cursor == self.top
    end

    def self.at_bottom?
      self.cursor == self.bottom
    end

    def self.clear name=nil
      if name
        if View.buffer_visible?(name)
          View.to_buffer(name)
          View.clear
        end
        return
      end

      $el.erase_buffer
    end

    def self.dir options={}

      return "/tmp/" if ! $el   # Tmp: if not $el, default to /tmp/

      result = File.expand_path($el.elvar.default_directory)

      # :ignore_shell_dir, so just return just current one?

      # We're in ~/xiki, so use dir emacs started in instead
      if options[:startup_dir_if_topic] && result == File.expand_path("~/xiki")
        result = File.expand_path($el.elvar.command_line_default_directory)
      end

      if options[:force_slash]
        return result =~ /\/$/ ? result : "#{result}/"
      end

      result
    end

    def self.dir= dir
      dir = File.expand_path dir
      dir << "/" if dir != "/"
      $el.elvar.default_directory = dir
    end

    # Returns path to current file if saved.
    # If file not saved or file is a buffer, it writes it to a temp file and returns that path.
    #
    # View.as_file_or_temp_file :buffer=>"tasks.notes"
    # View.as_file_or_temp_file :file=>"^n"
    def self.as_file_or_temp_file options={}

      buffer = options[:buffer]

      # :file, so...
      if options_file = options[:file]
        options_file = Bookmarks[options_file]
        buffer = $el.get_file_buffer options_file
        # If file open in buffer, continue on - it'll move to it and check for modified
        # If not, return the path
        return options_file if ! buffer
      end

      # Buffer passed in, so switch to it...

      if buffer
        orig = View.name
        $el.set_buffer buffer
      end

      file = View.file

      # Unmodified file, so just return path...

      if file && ! self.modified? # && ! options[:file]   # If :file passed, don't try current file
        $el.set_buffer orig if buffer
        return file
      end

      # Modified file or buffer, so write to temp file...

      name = TextUtil.snake(file ? file : View.name)

      file = "/tmp/#{name}"

      $el.write_region nil, nil, file

      $el.set_buffer orig if buffer

      file
    end

    def self.file
      return nil if ! $el
      # Uncommenting might cause problems > 2014-10-25 > slightly risky because this is used everywhere
      file = $el.elvar.buffer_file_name
      file ? File.expand_path(file) : nil
    end

    def self.extension
      file = View.file
      return nil if ! file
      file[/\.(\w+)$/, 1]
    end

    def self.file_or_buffer
      self.file || self.name
    end

    def self.file_name
      $el.buffer_file_name ?
        $el.file_name_nondirectory($el.buffer_file_name) :
        nil
    end

    def self.path options={}
      # TODO: merge with .dir?
      $el.elvar.default_directory
    end

    def self.frame
      $el.selected_frame
    end

    def self.window
      $el.selected_window
    end

    # Returns whether a buffer is open / exists
    # View.buffer_open? "tasks.notes"
    #   #<buffer tasks.notes>
    def self.buffer_open? name
      $el.get_buffer(name) ? true : nil
    end

    def self.buffer_visible? name
      View.list.
        collect {|b| $el.window_buffer b}.
        collect {|u| $el.buffer_name u}.
        member?(name)
    end

    def self.file_visible? path
      View.visible_files.member? path
    end

    def self.wrap on_or_off=:on
      $el.elvar.truncate_lines = on_or_off.to_sym == :off
    end

    def self.set_mark pos=nil
      pos ||= self.cursor
      $el.set_mark pos
    end

    def self.mark= pos=nil
      self.set_mark pos
    end

    def self.mark
      $el.mark
    end

    def self.select
      self.set_mark
    end

    def self.insert txt, options={}
      if options[:utf8]
        File.open("/tmp/tmp.txt", "w") {|f| f << txt}
        orig = $el.elvar.coding_system_for_read   # Read file as utf-8
        $el.elvar.coding_system_for_read = 'utf-8'.to_sym
        $el.insert_file_contents "/tmp/tmp.txt"

        # breaks when utf8 - doesn't go far enaugh??!
        # Maybe get length of file instead of string?
        # Or, set encoding of buffer to utf8?

        $el.elvar.coding_system_for_read = orig
        Move.forward txt.size unless options[:dont_move]   # .insert_file_contents leaves cursor at beginning
      else
        $el.insert txt
        Move.backward txt.size if options[:dont_move]
      end
    end

    def self.<< txt, options={}
      self.insert txt, options
    end

    def self.unindent txt

      # Trim off optional first line
      txt = txt.sub /^\n/, ''
      # Get indent of first line
      indent = txt[/\A +/]
      # Delete this much indent on other lines
      txt.gsub! /^#{indent}/, ''
      # Delete empty space at end
      txt.sub!(/ +\z/, '')

      $el.insert txt
    end

    def self.empty?
      $el.point_min == $el.point_max
    end

    def self.recenter n=nil
      if n.is_a? String

        View.to_highest
        Search.forward n, :beginning=>1
        View.recenter 0

        return
      end

      n ||= Keys.prefix

      if n == :-
        indent = Line[/^  ( *)/, 1]
        return View.recenter_under "^#{indent}\\S-", :relative=>1
      end

      if n == :uu
        return View.recenter_under "^\\( *def \\| *it \\|^>\\)", :relative=>1, :incude_comments=>1
      end

      n -= 1 if n.is_a? Fixnum
      $el.recenter n

    end

    #
    # Scrolls window so top line matches pattern (first match above).
    #
    # View.recenter_under "^ *def "
    #
    def self.recenter_under pattern, options={}
      orig = Location.new
      Line.next
      Search.backward pattern

      # If :relative, search backward from here...

      if options[:relative]

        Keys.clear_prefix
        Move.to_previous_paragraph if options[:incude_comments] && Line.value(0) =~ /^ *#/
        line = View.line

        View.recenter_top
        orig.go
        difference = View.line - line

        View.recenter -2 if difference > (View.height-3)
        return
      end

      # Search from top...

      View.to_highest
      Search.forward pattern
      Line.to_beginning
      View.recenter_top
    end

    def self.recenter_top_key
      return View.recenter_under "^\\( *def \\| *it \\|^>\\)", :incude_comments=>1, :relative=>0 if Keys.prefix_u
      self.recenter_top # :incude_comments=>1
    end

    def self.recenter_top
      $el.recenter(Keys.prefix || 0)
    end

    def self.rest
      $el.buffer_substring($el.point, $el.point_max)
    end

    def self.to_line n=nil
      Move.to_line n
    end

    def self.line= n=nil
      self.to_line n
    end

    def self.to_line_with_prefix first=""
      line = %'#{first}#{Keys.input(:prompt=>"goto line: #{first}")}'
      View.to_line line
    end

    def self.to n
      $el.goto_char n
    end

    def self.focus
      $el.x_focus_frame($el.selected_frame)
    end

    def self.index
      View.list.index(View.window)
    end

    def self.count_matches
      pattern = Keys.input 'pattern to count: '
      View.message View.txt.scan(/#{Regexp.quote pattern}/).length
    end

    def self.line
      Line.number
    end

    def self.line_number
      Line.number
    end

    def self.column
      ($el.point - $el.point_at_bol) + 1
    end

    def self.column= to
      Move.to_column to
      nil
    end

    def self.word_at_cursor
      $el.thing_at_point(:symbol)
    end

    def self.cursor
      $el.point
    end

    def self.cursor= n
      $el.goto_char n
    end


    def self.visible_line_number
      Line.number - Line.number($el.window_start) + 1
    end

    def self.visible_line_number= num
      line = Line.number($el.window_start) + num - 1
      View.line = line
    end

    #
    # Delete string between two points, and returns deleted text.
    #
    # View.delete 1, 10
    # View.delete :line
    # View.delete :char
    # View.delete   # Delete's selection (and returns it)
    #
    def self.delete_all
      $el.delete_region 1, View.bottom
    end

    def self.delete left=nil, right=nil
      return Line.delete if left == :line
      left, right = [self.cursor, self.cursor+1] if left == :char

      left, right = [left.begin, left.end] if left.is_a?(Range)

      # Default to deleting region
      left, right = View.range if left.nil?

      txt = $el.buffer_substring left, right
      $el.delete_region left, right
      txt
    end

    def self.char
      $el.buffer_substring($el.point, $el.point+1) rescue nil
    end

    def self.char_before
      $el.buffer_substring($el.point-1, $el.point) rescue nil
    end


    def self.scroll_bars= on
      $el.toggle_scroll_bar on ? 1 : 0
    end

    def self.scroll_bars
      $el.frame_parameter(nil, :vertical_scroll_bars) ? true : nil
    end

    def self.stem
      Files.file_name.sub(/\.[^.]+$/, '')
    end

    def self.name
      return nil if ! $el
      $el.buffer_name
    end

    def self.message txt, options={}
      $el.message txt
      View.beep if options[:beep]

      nil
    end

    def self.alert txt
      self.beep
      self.message txt
    end

    #   def self.beep options={}
    def self.beep *args

      # Pull out 1st arg if string
      txt = args[0].nil? || args[0].is_a?(Hash) ? nil : args.shift.to_s

      options = args[0] || {}
      raise "Too many args" if args.size > 1

      $el.beep
      if times = options[:times]
        (times-1).times do
          View.pause 0.11
          View.beep
        end
      end
      View.flash txt, :times=>6 if txt

      options[:return_txt] ? txt : nil
    end

    def self.mode
      return :notes if Notes.enabled?
      $el.elvar.major_mode.to_s.gsub('-','_').to_sym
    end

    def self.init
      @@dimension_options ||= []   # Set to empty if not set yet
    end

    def self.paragraph options={}
      orig = Location.new
      found = Search.backward "^$", :go_anyway=>true
      Line.next if found
      left = Line.left
      Search.forward "^$", :go_anyway=>true
      right = Line.right
      orig.go

      left = Line.left if options[:start_here]
      right = Line.left if options[:end_here]

      return [left, right] if options[:bounds]
      txt = View.txt(left, right)
      View.delete(left, right) if options[:delete]
      return [left, right] if options[:bounds]
      txt
    end

    def self.url txt
      $el.browse_url txt
    end

    def self.kill options={}
      prefix = Keys.prefix :clear=>1

      force_recent = options[:force_recent]
      force_recent = 1 if prefix == :-

      # To avoid "Buffer has a running process; keep the buffer? (y or n)" message
      if process = $el.get_buffer_process(View.buffer)
        $el.set_process_query_on_exit_flag process, nil
      end

      # If :force_recent, grab view that is 2nd most recent (if it's shown in another view, it won't be shown by default)

      if force_recent
        list = Buffers.list[0..3].map{|o| Buffers.name o}
        list.delete_if{|o| o == "ol"}
        recent = list[1]
      end

      currently_viewed = View.list_names

      # Close...

      $el.kill_buffer $el.current_buffer   # Emacs 24 compatible

      # There are no other views except *star views, so show 'getting around' view
      buffers = Buffers.list_names :user_only=>1
      if buffers.length == 0

        # If only viewed is the getting around view then quit
        DiffLog.quit

        return
      end

      View.hide if prefix == :u   # If up+, also close view

      # This view is showing somewhere else, so switch to another
      ControlTab.go if currently_viewed.member? View.name

      if force_recent
        $el.switch_to_buffer recent
      end

      nil
    end


    def self.kill_all
      $el.erase_buffer
    end

    def self.delete_paragraph options={}
      left, right = View.paragraph(:bounds => true)
      Effects.blink :left=>left, :right=>right
      txt = View.delete left, right
      Clipboard.set 0, txt
      return options[:return] if options.key?(:return)
      txt
    end

    def self.kill_paragraph options={}
      left, right = View.paragraph(:bounds => true)
      Effects.blink :left=>left, :right=>right
      txt = View.delete left, right
      return options[:return] if options.key?(:return)
      txt
    end

    def self.expand_path path, options={}

      # Expand ~

      had_slash = path =~ /\/$/   # Check whether / at end

      # This cleans up /./ nad /../ in paths as side-effect of above
      path = File.expand_path path, options[:dir]

      path = "#{path}/" if had_slash && path !~ /\/$/   # Put / back at end, if it was there (and not there now)

      path
    end

    def self.dimensions_set position_x=nil, position_y=nil, size_x=nil, size_y=nil
      self.fullscreen_off
      $el.set_frame_size(View.frame, size_x, size_y)
      $el.set_frame_position(View.frame, position_x, position_y) unless position_x.nil?
      nil
    end

    # Toggle full-screen mode
    #
    # View.dimensions_full
    # View.dimensions_full :force=>1   # Always sets to full, even if already full
    def self.dimensions_full options={}
      if options[:force] || ! $el.frame_parameter(nil, :fullscreen)   # If fullscreen on turn it off
        self.fullscreen_on
      else   # Else, turn it on
        $el.set_frame_parameter(nil, :fullscreen, nil)
      end
    end

    def self.fullscreen_off
      $el.set_frame_parameter(nil, :fullscreen, nil) if $el.frame_parameter(nil, :fullscreen)
    end

    def self.fullscreen_on
      $el.set_frame_parameter nil, :fullscreen, :fullboth
    end

    def self.line_at_top
      Line.number self.start
    end


    # Line at top of visible part of view
    def self.start
      $el.window_start
    end

    def self.end
      $el.window_end
    end

    def self.insert_line
      orig_indent = Line.indent
      cursor = View.cursor
      char, char_before = View.char, View.char_before
      prefix = Keys.prefix :clear=>1   # Check for numeric prefix

      if prefix == 0
        prefix = nil
        Line.previous
      end

      line = Line.value

      indent_txt =
        if prefix == :u || prefix == :-   # Use prefix chars from previous line
          ""
        else
          line[/^[ |#;:!%$*?^+-]*/]
        end

      # remave ":" if it's the last thing in prefix
      indent_txt.sub!(/[^ ]+$/, '')

      # If C--, move to end of previous line, so it goes before
      if prefix == :-
        Move.to_end -1
      end

      indent_txt = "" if Line.at_left?

      # Remove trailing spaces if empty quoted line

      Line.sub!(/ +$/, '') if line =~ /^ *[|:#!] +$/

      times = prefix.is_a?(Fixnum) ? prefix : 1
      times.times{ View.insert "\n#{indent_txt}" }

      if char != "\n"
        View.cursor = cursor
      end
      nil

    end

    def self.shift
      times = Keys.prefix_times;  Keys.clear_prefix

      choice = Keys.input :chars=>1, :prompt=>"Move this view to: (n)ext, (p)revious"
      Effects.blink
      times.times do

        buffer_a =  View.buffer

        # Go to other and switch to A
        choice == "n" ? View.next : View.previous
        buffer_b = View.buffer
        $el.switch_to_buffer buffer_a

        # Go to first and switch to B
        choice == "n" ? View.previous : View.next
        $el.switch_to_buffer buffer_b

        choice == "n" ? View.next : View.previous

      end
      Effects.blink
    end

    def self.layout_right nth=nil
      nth ||= Keys.prefix
      if nth   # If numeric prefix, go to nth
        down = nth - 1
        self.to_after_bar
        # If there's only one column (last view is at left), go to top
        Move.to_window(1) if self.edges[0] == 0
        down.times { self.next }
        Effects.blink(:what=>:line)# if options[:blink]
        return
      end

      current = View.name

      second_visible = Buffers.list.each{|b|
        name = $el.buffer_name(b)
        next unless name != current
        next unless self.buffer_visible?(name)
        next unless self.edges(get_buffer_window(b))[0] != 0  # Window is at left of frame
        break name   # Found
      }
      self.to_buffer second_visible
      Effects.blink(:what=>:line)# if options[:blink]

    end



    def self.layout_todo

      # There's no bar yet and in tasks.notes, so switch to next (avoid showing :t in both views)
      if View.file == Bookmarks["%n"] && ! View.bar?
        View.to_buffer Buffers.list_names.find{|o| ! ["notes.xiki"].member? o}
      end

      View.bar
      View.open "%n", :stay_in_bar=>1
    end

    def self.layout_todo_and_nav options={}

      return self.layout_outlog(:include_ol_view=>1) if Keys.prefix_u(:clear=>1)

      orig = View.name   # Store original view

      # Create notes dir > if not there yet
      FileUtils.mkdir_p Bookmarks["%x"]

      was_already_split = View.bar?

      FileTree.open_in_bar   #> |||

      self.to_nth 2

      return if options[:include_ol_view]   # It's calling us before showing "ol" view, so don't tweak further


      # Switch to one that's not :t or :n   #> continue here > where does it put view on top of ^n?!!
      self.to_buffer Buffers.list_names.find{|o| ! ["notes.xiki", "links.xiki", "ol"].member? o}
      self.to_nth 0

      # Was originally at "*scratch*", so use last edited (that's not notes.xiki, etc)

      View.content_right

      if View.name == "*scratch*"
        # open first_normal_file > before esc-i!
        edited = DiffLog.file_list
        first_normal_file = edited.find{|o| o !~ /(notes|links)\.xiki$/}

        View.open first_normal_file
        return
      end

      self.balance(:narrow_bar=>1) if ! was_already_split

      # Jump to right view   #> This was showing up on top of ^n > Was it needed at all?

    end

    def self.layout_nav

      # There's no bar yet and in nav.xiki, so switch to next (avoid showing :t in both views)
      ControlTab.go if self.file == Bookmarks["%links"] && ! self.bar?

      self.bar
      self.open "%links", :stay_in_bar=>1
    end

    def self.layout_quick

      # There's no bar yet and in nav.xiki, so switch to next (avoid showing :t in both views)
      self.to_buffer Buffers.list_names.find{|o| ! ["notes.xiki", "links.xiki", "ol", "quick.xiki"].member? o}

      self.bar
      self.open "%q", :stay_in_bar=>1
    end

    def self.layout_outlog options={}

      prefix = options[:prefix] || Keys.prefix

      if options[:called_by_launch]
        prefix = nil
      end

      # If already showing, just go to it

      # window+ide, so show ide views, then split before showing ol view...

      if options[:include_ol_view]

        # Hide all other views first, so it doesn't get confused by other windows
        $el.delete_other_windows
        buffer = Buffers.list_names.find{|o| ! ["notes.xiki", "links.xiki", "ol"].member? o}
        self.to_buffer buffer

        self.layout_todo_and_nav :include_ol_view=>1

        self.to_nth 1

        self.create_horizontal

        self.balance
      end

      # If current line has Ol., grab line and path

      # If we were on an Ol line, jump to it later...


      file = self.file

      found = prefix != :u && ! options[:dont_highlight] && Line =~ /^[ |:]*Ol\b/ && OlHelper.source_to_output(self.file, Line.number)   # => nil

      put_value_here = nil
      if prefix == :-   # || prefix == :uu   # If we want to update the commend value from Ol
        return self.flash("- Not found!") if ! found
        put_value_here = Location.new
      end

      Code.open_log_view options


      if options[:include_ol_view]
        raise "Not on Ol view > so don't delete!" if View.name != "ol"
        self.delete_all   # Clear out ol view
        self.content_right
        return
      end

      # If prefix is number, move to nth and open...
      if prefix.is_a? Fixnum
        Search.backward "^>"
        prefix.times { Search.forward "^ *-" }
        Launcher.launch
        return
      end

      if found
        if found >= (Line.number(View.bottom) - 1)   # If too far back to be shown
          found = nil
          View.flash("- Not found!", :times=>1)
        else
          View.to_bottom
          Line.previous(found+1)
        end
      end

      if found
        Effects.blink(:what=>:line)
      end

      if put_value_here

        value = Ol.grab_value Line.value

        put_value_here.go
        Ol.update_value_comment value
      end
    end

    def self.split options={}
      options[:horizontally] ?
        $el.split_window_horizontally :
        $el.split_window_vertically
    end

    def self.to_relative options={}
      prefix = Keys.prefix || options[:line]

      if prefix == nil   # no prefix, so go to middle
        top = Line.number($el.window_start)
        bottom = Line.number($el.window_end)
        View.line = top + ((bottom - top) / 2)
        return
      end

      if prefix == :u   # up+, so go to top
        prefix = -1
      end

      if prefix.is_a?(Fixnum) && prefix <= 0
        View.line = Line.number($el.window_end) - (-prefix+2)
        return
      end

      $el.goto_char $el.window_start
      ((prefix || 1) -1).times do
        Line.next
      end
    end

    def self.gsub! from, to

      line, column = View.line, View.column
      txt = View.delete(View.top, View.bottom)
      txt.gsub! from, to
      View << txt
      View.line, View.column = line, column

      nil

    end

    # Number of lines from the top of the view we are.
    def self.scroll_position
      $el.line_number_at_pos($el.point) - $el.line_number_at_pos($el.window_start)
    end

    def self.scroll_position= pos
      $el.recenter pos
    end

    def self.under txt, options={}
      options[:escape] = '' if options[:escape].nil?
      txt = CodeTree.returned_to_s txt
      Tree.under txt, options  # .merge(:escape=>'')
    end

    def self.>> txt
      View.<< txt, :dont_move=>1
    end

    # Makes message glow at end of line, and adds "/", like
    #
    # foo/Type something here
    # View.prompt
    def self.prompt message="Type something here", options={}

      if ! Line.blank?
        Move.to_end
        View << "/" unless options[:no_slash] || Line =~ /\/$/
      end

      self.insert(message, :dont_move=>1)

      left, right = self.cursor, Line.right
      Effects.glow({:what=>[left, right], :reverse=>1}.merge(options))
      self.delete left, right

      return unless options[:timed]

      View.<< message, :dont_move=>1

      $el.elvar.inhibit_quit = true

      # Wait for first, then loop until pause

      # Avoids weird "Non-character input-event" error, when called via emacsclient
      c = $el.read_char_exclusive("")

      key = Keys.to_letter c
      View.delete View.cursor, Line.right   # Delete temporary message
      View << key

      while(c = $el.read_char("", nil, 0.35))
        key = Keys.to_letter c
        if c == 7   # C-g?
          Cursor.restore :before_input
          $el.elvar.inhibit_quit = nil
          $el.keyboard_quit
        end
        View << key
      end
      $el.elvar.inhibit_quit = nil
    end

    def self.modified?
      $el.buffer_modified_p
    end


    #
    # Makes message temporarily appear and glow, then go away, like:
    #
    # foo/
    #   - Success!
    #
    def self.flash message=nil, options={}

      was_modified = $el.buffer_modified_p

      # File.open("/tmp/flashes.log", "a") { |f| f << "#{message}\n" } if message

      message ||= "- Success!"

      orig = self.cursor
      indent = Line.indent

      blank_line = Line[/^$/]

      Line.next unless blank_line || options[:dont_nest]
      message = "#{message.strip}" if ! options[:dont_nest]
      message << "\n" unless blank_line
      message.gsub!(/^/, "#{indent}  ") if ! options[:dont_nest]

      self.insert message, :dont_move=>1
      left, right = Line.left, Line.left+message.length
      self.cursor = orig

      Effects.glow({:what=>[left, right], :reverse=>1, :times=>2}.merge(options))

      self.delete left, right

      self.cursor = orig

      $el.not_modified if ! was_modified
      View.message ""   # Block "Modification flag cleared" message from appearing

      nil
    end

    def self.flashes
      txt = IO.readlines File.expand_path("/tmp/flashes.log") rescue return "- No messages flashed yet!"
      txt = txt.reverse.uniq.join
    end

    def self.refresh   # redraw
      $el.sit_for 0
    end

    def self.pause n=0.5   # wait / sleep / sit
      $el.sit_for n
    end

    #
    # Set small string in mode line.
    #
    # View.status "aa"
    # View.status nil
    # View.status :nth=>1   # Retrieve
    # View.status "bb", :nth=>2
    # View.status "dim", :nth=>3
    # View.status "dotsies", :nth=>4
    # View.status :scale=>5
    # View.status :scale=>1
    # View.status :scale=>0
    # View.status :bars=>[2, 5]   # Show 2 bars
    #
    def self.status val=nil, options={}

      if !options && ! val || val == :docs   # If nothing passed, show help message
        return "
          > Examples
          @View.status 'aa'
          @View.status :scale=>5
          @View.status 'abc', :nth=>3
          @View.status 'dim', :nth=>4
          "
      end

      options = val if val.is_a? Hash

      nth = options[:nth] || 1

      # If :bars=>[n, n]...

      if bars = options[:bars]
        nth = 3

        a, b = bars
        if a < b
          common, remainder, remainder_char = a, b - a, 'i'
        else
          common, remainder, remainder_char = b, a - b, 'f'
        end

        val = " #{"z" * common}#{remainder_char * remainder}"
      end

      if scale = options[:scale]
        nth = 3
        val = {0=>' ', 1=>'e', 2=>'i', 3=>'v', 4=>'ø', 5=>'ß'}[scale]
      end

      key = "xiki_status#{nth}".to_sym

      # If val is still hash, we must just be returning it...

      if val.is_a? Hash
        return $el.elvar[key]
      end

      $el.make_local_variable key
      $el.elvar[key] = val

      nil
    end

    def self.length
      $el.buffer_size
    end


    def self.new_file *args
      if Keys.prefix_u
        View.to_buffer View.unique_name("untitled.txt")
      else
        View.to_buffer View.unique_name("untitled.xiki")
        Notes.mode
      end

      View >> "\n\n\n"
    end

    def self.confirm message="Are you sure?", options={}
      View.beep :times=>3 if options[:beep]

      View.message message   # Make sure to clear the message out
      View.flash "- #{message}", :times=>(options[:times]||2)
      choice = Keys.input :prompt=>"#{message} (type 'y' for yes)", :chars=>1

      View.message ""   # Make sure to clear the message out

      return choice =~ /[ym]/i
    end

    def self.rename_uniquely
      $el.rename_uniquely
    end

    # View.unique_name "hi"
    # View.unique_name "notes.xiki"
    #   todo2.xiki
    def self.unique_name name
      return name if ! self.buffer_open? name
      i, limit = 2, 1000

      # Increment number until fooX.bar doesn't already exist...

      extension = name.slice! /\.[a-z]+$/i
      while i < limit
        break if ! self.buffer_open? "#{name}#{i}#{extension}"
        i += 1
      end
      "#{name}#{i}#{extension}"

    end

    def self.number_of_lines
      Line.number View.bottom
    end

    # Whether text in this view matches the regex
    # p View =~ /text/   # => 283
    # p View =~ /z{3}/   # => nil
    def self.=~ regex
      self.txt =~ regex
    end


    # Jump to line in file that most closely matches the quote.
    # The quote can be just a line in the file.  We try to find
    # match with the exact indent, then secondarily try to find
    # line with different indent (since the indent of quoted
    # lines isn't strict).
    def self.find_snippet_in_txt txt, quote

      txt = txt.split("\n")
      found = txt.index{|o| o =~ /^#{Regexp.quote(quote)}$/}

      # Search for exact line match

      unless found   # If not found, search for substring of line, but with a break at the end
        found = txt.index{|o| o =~ /#{Regexp.quote(quote)}([^_a-zA-Z0-9\n]|$)/}
      end
      unless found   # If not found, search for substring of line
        found = txt.index{|o| o =~ /#{Regexp.quote(quote)}/}
      end
      unless found   # If not found, search for it stripped
        found = txt.index{|o| o =~ /#{Regexp.quote(quote.strip)}/}
      end

      return nil if ! found

      found + 1


    end

    def self.to_snippet quote

      quote = $el.regexp_quote quote

      View.to_highest

      # Search for exact line match
      found = Search.forward "^#{quote}$"

      unless found   # If not found, search for substring of line, but with a break at the end
        Move.top
        found = Search.forward "#{quote}\\([^_a-zA-Z0-9\n]\\|$\\)", :beginning=>true
      end
      unless found   # If not found, search for substring of line
        Move.top
        found = $el.search_forward_regexp "#{quote}", nil, true
      end
      unless found   # If not found, search for it stripped
        Move.top
        found = $el.search_forward_regexp "#{quote.strip}", nil, true
      end

      Line.to_beginning

      # View.recenter_top if quote =~ /^(> | *(def|function) )/
      self.recenter_appropriately quote, :only_if_at_top=>1

      found

    end

    def self.recenter_appropriately line, options={}

      return self.recenter_top if line =~ /^(> | *(def|function) )/

      return if options[:only_if_at_top]

      self.recenter
    end

    def self.toggle
      prefix = Keys.prefix :clear=>true

      return $el.transpose_words(1) if prefix == :u

      return $el.transpose_paragraphs(1) if prefix == :-

      $el.transpose_chars 1
    end

    def self.minimize
      $el.iconify_frame
    end

    def self.do_move_up
      prefix = Keys.prefix
      line, column = View.line, View.column
      txt = Line.delete
      Tree.to_parent
      Line << "\n#{txt}"
      Line << "\n" if prefix == :u
      View.line, View.column = line+1, column
    end


    # Returns pixel xy coordinates of point, or position
    #
    # View.pixel_xy
    # View.pixel_xy 45418
    # View.pixel_xy 1   # Off the screen, no nil's
    def self.pixel_xy cursor=nil
      pos = $el.posn_x_y $el.posn_at_point(cursor)
      [$el.car(pos), $el.cdr(pos)]
    end

    def self.page_up
      # Get distance from top to here
      window_start = View.start
      return View.beep if window_start == View.top

      down_from_top = View.line - Line.number(window_start)

      # Go up

      distance = View.height-4

      # If it would move past the top, just recenter at bottom

      return View.recenter -2 if distance > View.line

      Line.previous distance

      # Recenter
      $el.recenter down_from_top

      View.message "#{View.percent}%%"
    end

    def self.page_down
      # Get distance from top to here
      window_end = View.end
      return View.beep if window_end == View.bottom

      line = View.line

      down_from_top = line - Line.number(View.start)

      distance = View.height-4

      # Go down
      Line.next distance


      # If went all the way to the bottom, go back and recenter
      if View.cursor == View.bottom
        View.line = line
        return View.recenter(2)
      end

      # Recenter
      $el.recenter down_from_top

      View.message "#{View.percent}%%"
    end

    def self.deselect
      $el.deactivate_mark
    end

    def self.no_deselect
      $el.elvar.deactivate_mark = nil
    end

    def self.zoom

      # If something hidden, just show
      if $el.point_min != 1 || $el.point_max != 1 + $el.buffer_size
        return Hide.reveal
      end

      $el.narrow_to_region($el.region_beginning, $el.region_end)
      View.deselect

    end

    # View.percent
    def self.percent
      total_lines = Line.number View.bottom
      current_line = View.line

      percent = current_line / total_lines.to_f
      percent = (percent * 100).round
    end

    def self.extract_suggested_filename_from_txt txt=nil   #> ""
      txt ||= View.txt

      # Remove text that appears when Ctrl+R
      txt.sub! /\A\|.+\n+/, ''

      # Don't store session if only command is sessions/...
      return if txt.scan(/^[^ \n].*/) == ["interactions/"]

      # Change some chars, to dashes
      txt.gsub!(/ > /, ' - ')

      name = txt[/[a-z][a-z.0-9 '-]*/i]   # '

      return if ! name   # Do nothing if no words in the file

      name.downcase!
      name.strip!

      # name
      name = Notes.heading_to_path name
      name.gsub!(/ /, '_')
      name
    end

    def self.tab_width width
      $el.elvar.tab_width = width
    end

    def self.scan regex
      self.txt.scan regex
    end

    def self.close
      self.kill
      self.hide
    end

    def self.insert_shell_prompt

      # up+, so prompt for bookmark...

      # On blank line, so prompt for dir and insert the prompt underneath
      if Line.blank?
        dir = Keys.bookmark_as_path :prompt=>"Bookmark to make shell prompt in: "
        View.<< Bookmarks[dir], :dont_move=>1
      end


      # Insert "$ " under this dir...

      Tree.<< "$ ", :no_search=>1, :no_slash=>1
      Line.to_right
    end

    def self.content_right

      # Already vertical split, so just move to top-right!...

      if View.bar?
        return View.to_upper #(:blink=>1)
      end

      # No split yet, so split, making current view end up on the right...

      View.create_vertical :prefix=>:u

    end


    def self.remove_last_undo_boundary

      # Altenative implementation > that removes border even if not at 2nd position in list
      # Downside > "(require 'cl)" adds .01s to emacs load time (happens each time xsh is run)
      #   (require 'cl)
      #   (remove* nil '(nil 2 nil 3 nil 4) :count 1)
      #     : (2 nil 3 nil 4)

      # If 2nd item in undo history is nil (an undo boundary) remove it
      $el.el4r_lisp_eval("
      (if (not (car (cdr buffer-undo-list)))
        (setq buffer-undo-list
          (cons (car buffer-undo-list) (cddr buffer-undo-list))
        )
      )
      ")
    end


    def self.highlight

      times = Keys.prefix_n || 1

      Line.to_left
      View.set_mark

      Line.next times

    end

    def self.app_dir
      File.expand_path $el.elvar.command_line_default_directory
    end

    def self.expose

      # Only one buffer open, so show error
      buffers = Buffers.list_names :user_only=>1
      if buffers.length == 1
        return View.message "Only one view is open, so we can't expose them."
      end

      # Save order of views
      original_order = self.remember_buffer_order

      $el.delete_other_windows   # Close all others first
      whole_width = $el.window_width

      # Results in 5 columns
      individual_width = whole_width / 4

      Ol Buffers.list_names(:user_only=>1)

      # Set 1st view
      Ol Buffers.list

      count = 0

      buffers = Buffers.list_names(:user_only=>1)

      Buffers.to buffers[0]

      buffers[1..-1].each do |name|

        count += 1
        $el.split_window_horizontally
        $el.other_window 1
        $el.balance_windows
        Buffers.to name

        break if $el.window_width < individual_width
      end

      $el.other_window 1   # Back to the first

      input = Keys.input :chars=>1, :prompt=>"Type number of window: "

      # Subsequent duplicated click sometimes comes in a little slower > ignore it
      while(char = $el.read_char("", nil, 0.05)) do
      end

      # Restore order of views
      self.restore_order original_order

      # Was mouse click, so select window...

      if match = input.match(/^\e\[<2;(\d+);(\d+)M/)

        # Right mouse click, so select window and close...

        x, y = match[1..2].map(&:to_i)
        window = $el.window_at(x, y)

        # Killed a buffer they clicked on
        View.to_window window
        $el.kill_buffer $el.current_buffer

        # Then redisplay
        if count > 1
          self.expose
        else
          $el.delete_other_windows
        end
        return

      elsif match = input.match(/^\e\[<0;(\d+);(\d+)M/)

        # Single mouse click, so select window...

        x, y = match[1..2].map(&:to_i)

        # 2. Jump to window
        buffer = $el.window_buffer $el.window_at(x, y-1)

        # Hide all windows but this one
        View.hide_others

        # 3. Jump to coords
        View.to_buffer buffer

        return
      end

      # Was number, so go to nth...
      number_map = {
        "One"=>"1",
        "To"=>"2",
        "Three"=>"3",
        "For"=>"4",
        "Five"=>"5",
      }
      if val = number_map[input]
        input = val
      end

      if input =~ /\A\d+\z/
        Move.to_window(input.to_i)

        # Hide all windows but this one
        View.hide_others
      end

      # Typed esc, so just show all

      if input == "\e"
        View.hide_others
      end

      nil
    end


    def self.link_views

      # Save order of views
      original_order = self.remember_buffer_order

      $el.delete_other_windows   # Close all others first
      View.open "%n"

      whole_width = $el.window_width
      individual_width = whole_width / 4   # Results in 5 columns somehow, which is fine

      # Get first "chunk" of sections in ^links...

      links_file = Bookmarks["%links"]
      txt = File.read links_file

      txt = txt[/.+?\n\n\n/m] || txt

      # Search and replace out colon lines
      txt.gsub! /^> .*:\n/, ''

      already_visited = {}

      # Split into sections
      sections = txt.split( /^>(?: |.*)$/ )[1..-1]

      # First pass > split into columns...

      sections.each do |section|
        $el.split_window_horizontally
        $el.other_window 1

        $el.balance_windows

      end

      View.to_nth 0
      count = 0


      # Block redisplay, which causes flickering
      recenter_redisplay_orig = $el.elvar.recenter_redisplay
      $el.elvar.recenter_redisplay = nil

      # Second pass > go to files and lines, splitting vertically if multiple in the block...

      sections.each do |section|
        links = Notes.extract_links :txt=>section, :filter=>/^ *:/

        count += links.length

        links.each_with_index do |link, i|
          file = link[0]

          if i > 0
            $el.split_window_vertically
            $el.balance_windows
          end

          $el.other_window 1

          $el.find_file file

          Color.clear_light unless already_visited[file]
          # Color.clear unless already_visited[file]

          already_visited[file] = 1

          target_quote = link[-1].sub /^: /, ''

          self.to_snippet target_quote
          Color.mark "light", :allow_multiple=>1

        end
      end

      # Third pass > recenter vertically...

      View.to_nth 1

      (View.list.length - 1).times do
        self.recenter_appropriately Line.value
        $el.other_window 1
      end

      # Re-enable redisplay
      $el.elvar.recenter_redisplay = recenter_redisplay_orig

      View.to_nth 0   # Back to the first

      $el.split_window_vertically
      $el.other_window 1
      $el.find_file links_file
      Move.top

      View.to_nth 0

      # Prompt and jump to a window

      # Only one review shown, so do nothing else
      if count == 1
        return View.to_nth 2
      end

      # Prompt for number of window to jump to

      input = Keys.input :chars=>1, :prompt=>"Type number of window: "

      # Subsequent duplicated click sometimes comes in a little slower > ignore it
      while(char = $el.read_char("", nil, 0.05)) do
      end

      # Restore order of views
      self.restore_order original_order

      # Mouse click, so select window...

      if match = input.match(/^\e\[<0;(\d+);(\d+)M/)
        x, y = match[1..2].map(&:to_i)

        window = $el.window_at(x, y)

        View.to_window window

        # Hide all windows but this one
        View.hide_others :all=>1

        View.layout_todo_and_nav

        return
      end

      # Was number, so go to nth...

      number_map = {
        "One"=>"1",
        "To"=>"2",
        "Three"=>"3",
        "For"=>"4",
        "Five"=>"5",
      }
      if val = number_map[input]
        input = val
      end

      if input =~ /\A\d+\z/
        Move.to_window(input.to_i+2)

        # Hide all windows but this one
        View.hide_others :all=>1

        View.layout_todo_and_nav   #> ||

        return
      end

      # Typed esc, so just show all

      if input == "\e"
        View.hide_others :all=>1
      end

      nil
    end


    def self.remember_buffer_order
      $el.buffer_list
    end

    def self.restore_order original_order
      original_order.each do |b|
        next if ! $el.bufferp b
        next if ! $el.buffer_live_p b
        $el.bury_buffer b
      end
    end


    def self.keys_bar_mode
      $el.make_local_variable :xiki_bar_mode
      $el.elvar.xiki_bar_mode = "keys"
    end

    def self.toggle_bar_mode options={}

      # Todo > check if showing key shortcuts by default
        # Extract to function > (xiki-bar-keys-or-path)

      $el.make_local_variable :xiki_bar_mode


      # Order: Keys > path > blank


      # Not defined, so use default to determine

      current_value = $el.boundp(:xiki_bar_mode) ? $el.elvar.xiki_bar_mode : $el.elvar.xiki_bar_default_kind

      $el.elvar.xiki_bar_mode = current_value == "keys" ? "path" : "keys"

    end

    def self.auto_revert
      $el.auto_revert_mode 1
    end

    def self.topic_file?
      file = self.file
      return false if ! file
      file.start_with? File.expand_path("~/xiki/")
    end


    def self.green_box_inline txt, options={}
      trailing_whitespace = $el.elvar.show_trailing_whitespace
      $el.elvar.show_trailing_whitespace = nil

      column = View.column
      orig = View.cursor
      indent = Line.indent
      cursor = nil

      indent << "  " if options[:under]

      txt = "#{txt}"
      txt = txt.gsub(/^/, "#{indent}    ")

      if options[:under]
        Move.down

        txt.split("\n").each do |line|
          Move.to_beginning
          View >> "#{line}\n"
          Overlay.face :diff_green, :left=>(Line.left+indent.length), :right=>(Line.right+1)
          Move.down
          cursor = View.cursor
          View.cursor = orig

          # Pause while ignoring what's typed
          Keys.input :optional=>1, :seconds=>0.02, :prompt=>""

          View.cursor = cursor
        end
        View.cursor = orig

      else

        txt.split("\n").each do |line|
          Move.to_beginning
          View >> "#{line}\n"
          Overlay.face :diff_green, :left=>(Line.left+indent.length), :right=>(Line.right+1)
          Move.down
          View.column = column
          # Pause while ignoring what's typed
          Keys.input :optional=>1, :seconds=>0.02, :prompt=>""
        end

      end

      # Ignore what they type for half a second, to make sure they notice the tip
      Keys.input :optional=>1, :seconds=>0.5, :prompt=>""

      # Read char
      key = Keys.press_any_key :message=>" "

      # Make it go away

      if options[:under]
        View.cursor = cursor
        txt.split("\n").each do |line|
          Move.up
          Line.delete
          View.column = column
          cursor = View.cursor
          View.cursor = orig
          View.pause 0.02
          View.cursor = cursor
        end
        View.cursor = orig

      else
        txt.split("\n").each do |line|
          Move.up
          Line.delete
          View.column = column
          View.pause 0.02
        end
      end

      # Turn trailing space back on
      $el.elvar.show_trailing_whitespace = trailing_whitespace

      nil
    end


    def self.maybe_show_tip bullet, message, options={}

      # Already shown, so don't show it again (double-checking cached value)...

      return false if Topic.cache["xsh"]["> Tips already shown"] =~ /^- #{Regexp.quote bullet}/
      Topic.init_cache   # Seems like we show it > but reload cache to make sure
      return false if Topic.cache["xsh"]["> Tips already shown"] =~ /^- #{Regexp.quote bullet}/

      # Remember to not show it again > store in "> Tips already shown"
      txt = Topic.cache["xsh"]["> Tips already shown"]

      # Add section if wasn't there
      if ! txt
        txt = Topic.cache["xsh"]["> Tips already shown"] = "
          This is where xsh remembers which tips it has already shown you. If
          you want them to be shown again, just delete them from here (and
          reload xsh).
        ".unindent+"\n\n\n"
      end

      # Update in memory (prepend)...

      # Extractable to Notes.update_bullets
      # Remove "- No tips shown yet" line if there
      txt.sub! /^- No tips shown yet\n/, ""
      # Add blank lines if none
      txt.<< "\n\n\n" if txt !~ /\n\n\n/
      # Add after one blank line
      txt.sub! /\n\n+/, "\n\n- #{bullet}\n"

      # Update on disk...

      file = File.expand_path("~/xiki/xsh.xiki")
      topic_txt = File.read(file) rescue nil
      Notes.replace_section topic_txt, "> Tips already shown", txt

      File.open(file, "w") { |f| f << topic_txt }


      # Show green box inline >  "notice A-Z filter in bottom bar"

      View.green_box_inline message, options

      # Return true, indicating we showed dialog > because sometimes caller will want to not continue
      true

    end


  end

  View.init
end

