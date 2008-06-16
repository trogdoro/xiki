# Todo
# - switch_to_buffer generate_new_buffer(buffer_name)
#   - generate-new-buffer-name
# - Function to switch to buffer if open already
# switch-to-buffer-other-window
# switch-to-buffer-other-window-maybe
# Represents a division of a window (in emacs terms, it's a window (which is within a frame))
class View
  include ElMixin
  extend ElMixin

  CODE_SAMPLES = %q<
    # Moving
    - to top: View.to_top
    - to bottom: View.to_bottom

    # Getting positions
    - top: p View.top

    # Path / filename
    - whole path: p View.file
    - filename only: p View.file_name
    - path minus filename: p View.path

    # Select a buffer (going to it if already open)
    - handles views corretly: View.to_buffer("foo")

    # Misc
    - wrap lines: View.wrap

    # Default settings - Call this at startup to set some sensible view-related default behavior
    View.sensible_defaults
  >

  # Stores things user copies
  @@hash = {}

  # Get windows that are the same width as me
  def self.windows_in_my_column
    my_left = left_edge
    window_list(nil, nil, frame_first_window).to_a.select { |w| left_edge(w) == my_left }
  end

  # Make current window larger.  Take into account that there might be other vertical windows
  def self.enlarge
    ws = self.windows_in_my_column

    # TODO: update this to use .in_bar?

    # Get number of windows
    wnum = ws.length
    small = 2
    selected = selected_window
    # New height should be window minus 2 for each window
    ws.each do |w|
      # If current window, set to remaining
      if w == selected
        set_window_text_height w, frame_height - (wnum*small)
      else
        set_window_text_height w, small
      end
    end
  end

  # Creates a new window by splitting the current one
  def self.create
    prefix = elvar.current_prefix_arg
    # If prefix is 3, do Vertical split
    if prefix == 3 || Keys.prefix_u
      split_window_horizontally
      other_window 1
    else
      split_window_vertically
      other_window 1 unless prefix
    end
  end

  # Opens file (or whatever) from the path (can contain $bookmarks), just
  # moving to or exposing its view if it's already open.
  # By default it will open in 2nd view if we're in the bar view.
  def self.open path, options={}
    # Open after bar if in bar
    if View.in_bar? && ! options[:stay_in_bar]
      View.to_after_bar
    end

    # Expand $bookmark strings at beginning
    expanded = Bookmarks.expand(path)
    # Handle opening in other window
    if expanded
      # If already there, do nothing
      if expanded == buffer_file_name
      # If already displayed, move to its window
      elsif ( ( window_list.collect {|b| window_buffer b} ).collect {|u| buffer_file_name u} ).member?(expanded)
        find_file_other_window expanded
      # If not visible, just open it
      else
        find_file expanded
      end
    end
    # Jump to point if :goto_point (we assume path is just a bookmark)
    if options[:go_to_point] == true
      bookmark_jump path.sub(/^\$/, "")
    end
  end

  # Saves the configuration
  def self.save name=nil
    name ||= Keys.input(:optional => true)
    name ||= "0"
    @@hash[name] = current_window_configuration
  end

  # Saves the configuration
  def self.restore name=nil
    name ||= Keys.input(:optional => true)   # Get single key from user if no param
    name ||= "0"   # Set to "0" if user entered nothing
    # Todo: if "l", winner_undo
    if(name == "l")
      winner_undo
      return
    end
    # Use it to get configuration out of hash
    set_window_configuration(@@hash[name])
  end

  # Return list of windows
  def self.list
    window_list(nil, nil, frame_first_window).to_a
  end

  def self.list_files
    View.window_list.
      map {|b| window_buffer b}.
      collect {|u| buffer_file_name u}.
      select {|f| f}
  end

  # Move to nth window
  def self.to_nth n
    # If greater than size of windows, open last
    #insert self.list.size.to_s
    if n+1 > self.list.size
      return select_window(self.list.last)
    end

    # Otherwise, open nth
    select_window(self.list[n])
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
      orig << [window_buffer(w), window_height(w)]
    end
    delete_other_windows
    split_window_horizontally 40
    other_window 1
    o = nil
    # For each window but last
    orig[0..-2].each do |w|
      switch_to_buffer w[0]
      split_window_vertically
      set_window_text_height nil, w[1]
      o = other_window 1
    end
    # Last window
    switch_to_buffer orig[-1][0]
    set_window_text_height o, orig[-1][1]
    # Go to first window
    select_window self.first
  end

  # Returns whether bar is open
  def self.bar?
    window_width(self.first) < frame_width
  end

  # Returns whether we're in the bar
  def self.in_bar?
    self.bar? &&  # Bar is open
      window_edges(View.window)[0] == 0  # Window is at left of frame
  end

  def self.first
    frame_first_window
  end

  def self.last
    View.list[-1]
  end

  # Accounts for bar
  def self.balance
    balance_windows
    return if elvar.current_prefix_arg
    if self.bar?
      buffer = selected_window
      select_window frame_first_window
      enlarge_window (34 - window_width), true
      select_window buffer
    end
  end


  def self.hide
    left = View.left_edge

    # If there's one above me and before me
    index = View.index
    middle = false
    size = View.list.size
    if index > 0 && index < (size - 1)   # Check existance
      if( left == View.left_edge(View.list[index - 1]) &&
        left == View.left_edge(View.list[index + 1]) )  # Check alignment
        middle = true
      end
    end
    # If I'm the last
    last = index == (size - 1)

    delete_window
    previous_multiframe_window if View.left_edge != left || middle || last
  end

  def self.hide_others
    if elvar.current_prefix_arg or self.in_bar?
      delete_other_windows
      return
    end
    ws = self.windows_in_my_column
    selected = selected_window
    # New height should be window minus 2 for each window
    ws.each do |w|
      # If current window, set to remaining
      unless w == selected
        delete_window w
      end
    end
  end

  def self.next
    Keys.prefix_times.times do
      other_window 1
    end
  end

  def self.previous
    Keys.prefix_times.times do
      other_window -1
    end
  end

  def self.show_dir
    (elvar.current_prefix_arg || 1).times do
      dired_jump
    end
  end

  # Return selected text (aka the "region")
  def self.selection options={}
    txt = buffer_substring(region_beginning, region_end)
    delete_region(point, mark) if options[:delete]
    txt
  end

  def self.range
    [region_beginning, region_end]
  end

  def self.buffer
    window_buffer
  end

  # Return currently-selected window
  def self.current
    selected_window
  end

  # Move to window
  def self.to_window window
    select_window(window)
  end

  def self.open_in_bar
    # Remember buffer
    buffer = self.buffer

    # If already open, just go there
    if View.bar?
      select_window(View.first)
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

  def self.to_after_bar
    return unless self.bar?

    # Get width of last window
    width_of_last = window_width(self.last)

    # Go to first window not on left margin
    self.list.each do |w|
      if window_edges(w)[0] != 0  # Window is at left of frame
        select_window(w)
        break
      end
    end

  end


  def self.left_edge view=nil
    view ||= selected_window  # Default to current view

    window_edges(view)[0]
  end

  # Switches to a buffer
  def self.to_buffer name, options={}

    # If we're here already, do nothing
    return if buffer_name == name

    # If already displayed, move to it's window
    if ( ( window_list.collect {|b| window_buffer b} ).collect {|u| buffer_name u} ).member?(name)
      switch_to_buffer_other_window name
    else
      switch_to_buffer name
    end

    View.clear if options[:clear]
  end

  def self.txt left=nil, right=nil
    left ||= point_min
    right ||= point_max
    buffer_substring left, right
  end

  # Returns bounds of block in the form [left, after_header, right].
  def self.block_positions regex="^| "

    orig = point
    # Go to the end of the line, so if we're at the heading we'll find it
    Line.end
    found = re_search_backward regex, nil, 1
    if found
      left = point
      after_header = Line.left 2
    else
      left = after_header = point
    end
    Line.end
    re_search_forward regex, nil, 1
    right = (point == point_max) ? point_max : Line.left
    goto_char orig

    [left, after_header, right]

  end

  def self.to_top
    beginning_of_buffer
  end

  def self.to_bottom
    end_of_buffer
  end

  def self.to_end
    end_of_buffer
  end

  def self.beginning
    point_min
  end

  def self.top
    point_min
  end

  def self.bottom
    point_max
  end

  def self.clear
    erase_buffer
  end

  def self.dir
    elvar.default_directory
  end

  def self.dir= to
    elvar.default_directory = to
  end

  def self.file
    buffer_file_name
  end

  def self.file_name
    buffer_file_name ?
      file_name_nondirectory(buffer_file_name) :
      nil
  end

  def self.path options={}
    elvar.default_directory
  end

  def self.frame
    window_frame(frame_first_window)
  end

  def self.window
    selected_window
  end

  # Returns whether a buffer is open
  def self.buffer_open? name
    buffer_list.find{|b| buffer_name(b) == name}
  end

  def self.buffer_visible? name
    View.list.
      collect {|b| window_buffer b}.
      collect {|u| buffer_name u}.
      member?(name)
  end

  def self.wrap on_or_off=:on
    elvar.truncate_lines = on_or_off.to_sym == :off
  end

  # Call this at startup to set some sensible view-related default behavior
  def self.sensible_defaults
    el4r_lisp_eval("(progn (setq truncate-partial-width-windows nil)
      (set 'default-truncate-lines t)
      )")
  end

  def self.set_mark
    set_mark_command nil
  end

  def self.insert txt
    $el.insert txt
  end

  def self.unindent txt

    # Trim off optional first line
    txt.sub! /^\n/, ''
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

  def self.recenter_top
    $el.recenter 0
  end

  def self.rest
    buffer_substring(point, point_max)
  end


  def self.to_line n=nil
    Move.to_line n
  end

  def self.focus
    x_focus_frame(selected_frame)
  end

  def self.index
    View.list.index(View.window)
  end

  def self.count_matches
    right = $el.buffer_size
    left = 1
    left = point if Keys.prefix_u
    $el.message how_many(Keys.input('pattern to count: a'), left, right).to_s
  end

  def self.line_number
    Line.number
  end

  def self.cursor
    point
  end

  def self.delete left, right
    $el.delete_region left, right
  end

  def self.char
    buffer_substring(point, point+1)
  end

  def self.visibility
    c = Keys.input(:one_char => true, :prompt => 'Layout Visibility: [o]paque, [d]im, [m]edium, [t]ransparent')
    case c.to_sym
    when :o
      el4r_lisp_eval "(set-frame-parameter nil 'alpha 100)"
    when :d
      el4r_lisp_eval "(set-frame-parameter nil 'alpha 96)"
    when :m
      el4r_lisp_eval "(set-frame-parameter nil 'alpha 50)"
    when :t
      el4r_lisp_eval "(set-frame-parameter nil 'alpha 25)"
    end
  end

end
