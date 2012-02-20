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

  def self.menu
    %`
    - @window/
    - .flashes/
    - api/
      > Summary
      | View class is the catch-all class for dealing with editing text.
      | It has methods for inserting text and grabbing text, and quite a few
      | other things.
      |
      > Text
      | Inserts into the view.
      @ View << 'Hello'
      |
      | Return all the text.
      @ p View.txt
      |
      | Return text in a range.
      @ p View.txt 1, 5
      |
      | Return where the cursor is.
      @ p View.cursor
      |
      | Returns the line number the cursor is on.
      @ p View.line
      |
      |
      > Files and dirs
      | Returns file name.
      @ p View.name
      |
      | Returns file with whole path.
      @ p View.file
      |
      | Returns dir of file.
      @ p View.dir
      |
      | Switches to the view (opening if necessary)
      @ View.open "/tmp/"
      |
      > Messages
      | Shows temporary message inline.
      @ View.flash
      @ View.flash 'Saved'
      |
      | Shows message at bottom.
      @ View.message 'Hi there'
      |
      | Prompt user to type at end af line.
      @ View.prompt
      @ View.prompt 'Type something, dude'
      |
      | Makes a noise.
      @ View.beep
      |
      |
      > Advanced
      @ View.<< "hey", :dont_move=>1   # Without moving cursor
      |
      > Also see
      - line/
      |
    - docs/
      > Summary
      | Menus to deal with the layout, etc.
      |
      - Keys/
        | layout+create - TODO should we just refer to a @layout menu for these?
    `
  end

  # Stores things user copies
  @@hash = {}



  def self.windows_in_my_column
    my_left = left_edge
    window_list(nil, nil, frame_first_window).to_a.select { |w| left_edge(w) == my_left }
  end

  # Make current window larger.  Take into account that there might be other vertical windows
  def self.height= chars
    $el.set_frame_parameter nil, :height, chars
  end

  def self.width= chars
    $el.set_frame_parameter nil, :width, chars
  end

  def self.height options={}
    return $el.frame_parameter(nil, :height) if options.empty?
    return unless options.is_a?(Hash)

    if options[:add]
      self.height = self.height + 1
    end

    nil
  end

  def self.width options={}
    return $el.frame_parameter(nil, :width) if options.empty?
    return unless options.is_a?(Hash)

    if options[:add]
      self.width = self.width + 1
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

    usable_height = frame_height - 1 - wnum

    biggest = usable_height - ((wnum-1) * (small-1))
    selected = selected_window

    # Do multiple times (emacs daesn't get it right he first time)
    5.times do
      self.enlarge_internal :up, ws, selected, biggest, small
    end
    #     self.enlarge_internal :down, ws, selected, biggest, small
    nil
  end

  def self.enlarge_internal direction, ws, selected, biggest, small
    ws = ws.reverse if direction != :up

    ws.each do |w|
      # If current window, set to remaining
      if w == selected
        height = biggest # - 1
        set_window_text_height w, height
      else
        height = small - 1
        set_window_text_height w, height
      end
    end
  end

  # Creates a new window by splitting the current one
  def self.create prefix=nil
    prefix ||= Keys.prefix
    if prefix == 3   # If prefix is 3, do Vertical split
      $el.split_window_horizontally
      View.next
    elsif prefix == 4
      $el.split_window_vertically
      Keys.clear_prefix
      View.next
      View.enlarge
    elsif prefix == :u
      $el.split_window_vertically
    else
      $el.split_window_vertically
      View.next
    end
  end

  # Opens file (or whatever) from the path (can contain $bookmarks), just
  # moving to or exposing its view if it's already open.
  # By default it will open in 2nd view if we're in the bar view.
  def self.open path, options={}
    # Pull off line number if there
    path.sub!(/(.+?:\d+).*/, "\\1")
    line_number = path.slice!(/:\d+$/)

    # Open after bar if in bar
    if View.in_bar? && (! options[:stay_in_bar]) && path != "$0" && path != Bookmarks['$t'] && path != Bookmarks['$f']
      View.to_after_bar
    end
    # Expand $bookmark strings at beginning
    expanded = Bookmarks.expand(path)
    if expanded == ""   # If nothing there, return false
      path.sub!(/^- /, '')
      if path =~ /^\$\w+$/
        buffer = Bookmarks.buffer_bookmark path.sub(/^\$/, '')
        View.to_buffer buffer if buffer
      end
      return nil
    end

    if expanded   # Handle opening in other window
      if options[:same_view]
        $el.find_file expanded
      # If already there, do nothing
      elsif expanded == buffer_file_name
      # If already displayed, move to its window
      elsif ( ( window_list.collect {|b| window_buffer b} ).collect {|u| buffer_file_name u} ).member?(expanded)
        find_file_other_window expanded
      # If not visible, just open it
      else
        $el.find_file expanded
      end
    end
    # Jump to point if :goto_point (we assume path is just a bookmark)
    if options[:go_to_point] == true
      bookmark_jump path.sub(/^\$/, "")
    end

    if line_number
      View.to_line line_number[/\d+/]
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

  def self.list_names
    self.list.map{|v| buffer_name(window_buffer(v))}
  end

  def self.files options={}
    if options[:visible]
      return self.window_list.
        map {|b| window_buffer b}.
        collect {|u| buffer_file_name u}.
        select {|f| f}
    end

    Buffers.list.map { |b| $el.buffer_file_name(b) }.select{|path| path}
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
    buffer = window_buffer
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
      orig << [window_buffer(w), window_height(w)]
    end
    delete_other_windows
    #     split_window_horizontally 32   # Width of bar
    split_window_horizontally 54   # Width of bar
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
      View.edges[0] == 0  # Window is at left of frame
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
    return if Keys.prefix_u
    if self.bar?
      buffer = selected_window
      select_window frame_first_window
      #       enlarge_window (31 - window_width), true
      enlarge_window (48 - window_width), true
      select_window buffer
    end
  end

  # Returns true if anything is hidden
  def self.hidden?
    $el.point_min != 1 || $el.point_max != $el.buffer_size+1
  end

  def self.hide
    Keys.prefix_times.times do
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
    nil
  end

  def self.hide_others options={}
    if elvar.current_prefix_arg || self.in_bar? || options[:all]
      return $el.delete_other_windows
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

  def self.next options={}
    (Keys.prefix_times || options[:times] || 1).times do
      other_window 1
    end
    Effects.blink(:what=>:line) if options[:blink]
  end

  def self.previous options={}
    (Keys.prefix_times || options[:times] || 1).times do
      other_window -1
    end
    Effects.blink(:what=>:line) if options[:blink]
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

  def self.range_left
    region_beginning
  end

  def self.range_right
    region_end
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

  def self.to_upper options={}
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
    width_of_last = window_width(self.last)

    # Go to first window not on left margin
    self.list.each do |w|
      if self.edges(w)[0] != 0  # Window is at left of frame
        select_window(w)
        break
      end
    end
  end

  def self.left_edge view=nil
    view ||= selected_window  # Default to current view
    self.edges(view)[0]
  end

  def self.top_edge view=nil
    view ||= selected_window  # Default to current view
    self.edges(view)[1]
  end

  # Switches to a buffer
  def self.to_buffer name, options={}
    # If we're here already, do nothing
    return if buffer_name == name

    # If already displayed, move to it's window
    if ( ( window_list.collect {|b| window_buffer b} ).collect {|u| buffer_name u} ).member?(name) &&
        ! options[:in_current]
      switch_to_buffer_other_window name
    else
      switch_to_buffer name
    end
    View.clear if options[:clear]
    View.dir = options[:dir] if options[:dir]
  end

  def self.txt options={}, right=nil

    # If 2nd arg is there, we were passed right,left
    if right
      left = options
    else
      left = options[:left] || point_min
      right = options[:right] || point_max
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
    prefix ||= Keys.prefix

    prefix = prefix.abs if prefix.is_a?(Fixnum)

    case prefix
    when 0   # Do paragraph
      left, right = View.paragraph(:bounds=>true)
    when 1..6   # Should probably catch all numeric prefix?
      left = Line.left
      right = $el.point_at_bol(prefix+1)
    else   # Should this be only if no prefix?
      ignore, left, right = View.block_positions "^>"
    end

    Effects.blink(:left=>left, :right=>right) if options[:blink]
    txt = options[:just_positions] ? nil : View.txt(left, right)

    if options[:remove_heading] && txt =~ /^>/
      txt.sub! /.+\n/, ''
      # left won't be fixed, but who cares, for now
    end

    return [txt, left, right]
  end

  # Returns bounds of block in the form [left, after_header, right].
  def self.block_positions regex="^> "

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

  def self.to_highest
    prefix = Keys.prefix
    return self.to_line(prefix) if(prefix)   # If prefix, go to that line
    self.to_top
  end

  def self.to_top
    beginning_of_buffer
  end

  def self.to_bottom
    end_of_buffer
  end

  def self.at_bottom
    self.cursor == self.bottom
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

  def self.dir force_slash=nil
    # TODO: merge with .path?
    result = File.expand_path(elvar.default_directory)

    if force_slash
      return result =~ /\/$/ ? result : "#{result}/"
    end

    result
  end

  def self.dir= to
    elvar.default_directory = File.expand_path(to)+"/"
  end

  def self.file
    file = buffer_file_name
    file ? File.expand_path(file) : nil
  end

  def self.extension
    View.file[/\.(\w+)$/, 1]
  end

  def self.file_or_buffer
    self.file || self.name
  end

  def self.file_name
    buffer_file_name ?
      file_name_nondirectory(buffer_file_name) :
      nil
  end

  def self.path options={}
    # TODO: merge with .dir?
    elvar.default_directory
  end

  def self.frame
    window_frame(frame_first_window)
  end

  def self.window
    selected_window
  end

  # Returns whether a buffer is open / exists
  def self.buffer_open? name
    Buffers.list.find{|b| buffer_name(b) == name}
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

  def self.set_mark pos=nil
    pos ||= self.cursor
    $el.set_mark pos
  end

  def self.mark= pos=nil
    self.set_mark pos
  end

  def self.insert txt, options={}
    if options[:utf8]
      File.open("/tmp/tmp.txt", "w") {|f| f << txt}
      orig = $el.elvar.coding_system_for_read   # Read file as utf-8
      $el.elvar.coding_system_for_read = 'utf-8'.to_sym
      $el.insert_file_contents "/tmp/tmp.txt"
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

  def self.recenter
    $el.recenter Keys.prefix
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

  def self.line= n=nil
    self.to_line n
  end

  def self.to_line_with_prefix first=""
    line = "#{first}#{Keys.input(:prompt=>"goto line: #{first}")}"
    View.to_line line
  end

  def self.to n
    goto_char n
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
    left = point if Keys.prefix_u?
    $el.message how_many(Keys.input('pattern to count: a'), left, right).to_s
  end

  def self.line
    Line.number
  end

  def self.line_number
    Line.number
  end

  def self.column
    point - point_at_bol
  end

  def self.column= to
    Move.to_column to
  end

  def self.cursor
    point
  end
  def self.cursor= n
    goto_char n
  end


  def self.visible_line_number
    Line.number - Line.number($el.window_start) + 1
  end
  def self.visible_line_number= num
    line = Line.number($el.window_start) + num - 1
    View.line = line
  end

  def self.delete left=nil, right=nil
    return Line.delete if left == :line

    $el.delete_region left, right
  end

  def self.char
    buffer_substring(point, point+1)
  end

  def self.char_before
    buffer_substring(point-1, point)
  end


  def self.scroll_bars= on
    $el.toggle_scroll_bar on ? 1 : 0
  end

  def self.scroll_bars
    $el.frame_parameter(nil, :vertical_scroll_bars) ? true : nil
  end

  def self.name
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

  def self.beep
    $el.beep
  end

  def self.mode
    $el.elvar.major_mode.to_s.gsub('-','_').to_sym
  end

  def self.init
    @@dimension_options ||= []   # Set to empty if not set yet
    $el.winner_mode 1 rescue nil
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

    return [left, right] if options[:bounds]
    txt = View.txt(left, right)
    View.delete(left, right) if options[:delete]
    return [left, right] if options[:bounds]
    txt
  end

  def self.url txt
    $el.browse_url txt
  end

  def self.kill
    $el.kill_this_buffer
    nil
  end

  def self.kill_all
    $el.erase_buffer
  end

  def self.kill_paragraph
    left, right = View.paragraph(:bounds => true)
    Effects.blink(:left=>left, :right=>right)
    View.delete(left, right)
  end

  def self.expand_path path
    path = "#{View.dir}/#{path}" if path !~ /^[\/~]/

    # Expand ~

    had_slash = path =~ /\/$/   # Check whether / at end

    # This cleans up /./ nad /../ in paths as side-effect of above
    path = File.expand_path path

    path = "#{path}/" if had_slash && path !~ /\/$/   # Put / back at end, if it was there (and not there now)

    path
  end

  def self.dimensions_set size_x, size_y, position_x=nil, position_y=nil
    self.fullscreen_off
    set_frame_size(View.frame, size_x, size_y)
    set_frame_position(View.frame, position_x, position_y) unless position_x.nil?
    nil
  end

  # Toggle full-screen mode
  def self.dimensions_full
    if frame_parameter(nil, :fullscreen)   # If fullscreen on turn it off
      set_frame_parameter(nil, :fullscreen, nil)
    else   # Else, turn it on
      self.fullscreen_on
    end
  end

  def self.fullscreen_off
    set_frame_parameter(nil, :fullscreen, nil) if frame_parameter(nil, :fullscreen)
  end

  def self.fullscreen_on
    set_frame_parameter nil, :fullscreen, :fullboth
  end

  # Line at top of visible part of view
  def self.start
    $el.window_start
  end

  def self.insert_line
    orig_indent = Line.indent
    n = Keys.prefix   # Check for numeric prefix
    if(n)
      Line.previous unless n.nil?
      Line.next(n) if n.is_a? Fixnum   # If there, move down
      Line.to_right
      View.insert "\n"
      # Optionally indent
      $el.indent_for_tab_command unless(View.mode == :fundamental_mode && orig_indent == '')
      return
    end

    # No numeric prefix, so just grab this line's opening indent text
    indent_txt = Line[/^[ |$&%@\/\\#+!-]+/] || ""

    Deletes.delete_whitespace if ! Line.at_left && ! Line.at_right

    if Line.at_left
      Line.to_right
    end

    View.insert "\n#{indent_txt}"
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

  def self.edges view=nil
    view ||= self.current
    $el.window_edges(view).to_a
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
      name = buffer_name(b)
      next unless name != current
      next unless self.buffer_visible?(name)
      next unless self.edges(get_buffer_window(b))[0] != 0  # Window is at left of frame
      break name   # Found
    }
    self.to_buffer second_visible
    Effects.blink(:what=>:line)# if options[:blink]

  end

  def self.layout_todo options={}
    FileTree.open_in_bar
    Effects.blink(:what=>:line) unless options[:no_blink]
  end

  def self.layout_files # nth=nil
    FileTree.open_in_bar
    View.to_nth 1
    Effects.blink(:what=>:line)
  end

  def self.layout_output
    # If current line has Ol., grab line and path

    prefix = Keys.prefix

    found = prefix != :u && Line =~ /^ *Ol\b/ && OlHelper.source_to_output(View.file, Line.number)

    orig = nil
    if prefix == :-
      return View.flash("- Not found!") if ! found
      orig = Location.new
    end

    Code.open_log_view

    if found
      if found >= (Line.number(View.bottom) - 1)   # If too far back to be shown
        found = nil
        View.flash("- Not found!")
      else
        View.to_bottom
        Line.previous(found+1)
      end
    end

    Effects.blink(:what=>:line)
    if found
      Color.colorize :l
    end

    if orig
      return orig.go
    end

  end

  def self.split options={}
    options[:horizontally] ?
      $el.split_window_horizontally :
      $el.split_window_vertically
  end

  def self.to_relative
    if Keys.prefix == 0
      goto_char window_end - 1
      Line.to_left
      return
    end
    $el.goto_char window_start
    ((Keys.prefix || 1) -1).times do
      Line.next
    end
  end

  def self.gsub! from, to
    with(:save_excursion) do
      View.to_highest
      replace_regexp(from, to)
    end
  end

  def self.enter_upper
    prefix = Keys.prefix :clear=>true
    orig = Location.new

    View.layout_todo :no_blink=>true
    todo_orig = Location.new
    View.to_highest

    View.line = prefix if prefix.is_a? Fixnum

    line = Line.value
    Line.delete if prefix == :u

    todo_orig.go
    orig.go

    View.insert line
  end

  def self.scroll_position
    $el.line_number_at_pos(point) - $el.line_number_at_pos(window_start)
  end

  def self.scroll_position= pos
    $el.recenter pos
  end

  def self.under txt, options={}
    options[:escape] = '' if options[:escape].nil?
    txt = CodeTree.returned_to_s txt
    Tree.under txt, options# .merge(:escape=>'')
  end

  def a
    caller(0)[0..2]
  end

  def self.aa
    caller(0)[0..2]
  end

  def self.>> txt
    View.<< txt, :dont_move=>1
  end

  def self.enter_date
    insert elvar.current_prefix_arg ?
      Time.now.strftime("%Y-%m-%d %I:%M:%S%p").sub(' 0', ' ').downcase :
      Time.now.strftime("%Y-%m-%d")
  end

  def self.prompt message="Type something here", options={}
    ControlLock.disable

    if ! Line.blank?
      Move.to_end
      View << "/" unless Line =~ /\/$/
    end

    self.insert(message, :dont_move=>1)

    left, right = self.cursor, Line.right
    Effects.glow left, right, {:reverse=>1}.merge(options)
    self.delete left, right


    return unless options[:timed]

    View.<< message, :dont_move=>1

    $el.elvar.inhibit_quit = true

    # Wait for first, then loop until pause
    c = $el.read_char("")
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

  def self.flash message=nil, options={}

    was_modified = $el.buffer_modified_p

    File.open("/tmp/flashes.log", "a") { |f| f << "#{message}\n" } if message

    message ||= "- Success!"

    orig = self.cursor
    indent = Line.indent

    blank_line = Line[/^$/]

    Line.next unless blank_line
    message = "#{message.strip}"
    message << "\n" unless blank_line
    message.gsub! /^/, "#{indent}  "
    self.insert message, :dont_move=>1
    left, right = Line.left, Line.left+message.length
    self.cursor = orig

    Effects.glow left, right, {:reverse=>1, :times=>2}.merge(options)

    self.delete left, right

    self.cursor = orig

    $el.not_modified if ! was_modified

    nil
  end

  def self.flashes
    txt = IO.readlines File.expand_path("/tmp/flashes.log") rescue return "- No messages flashed yet!"
    txt = txt.reverse.uniq.join
  end

  def self.refresh
    $el.sit_for 0
  end


end

def View txt
  View.insert txt
end

View.init
