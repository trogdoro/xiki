require "hide"
require 'control_lock'

class Search
  extend ElMixin
  def self.insert_at_spot
    self.clear
    match = self.match
    #hidesearch_show_all_invisible
    Hide.show
    Location.go :_0
    #bookmark_jump "_"
    insert match
  end

  def self.insert_tree_at_spot
    self.clear
    # Grab symbol
    txt = TreeLs.snippet()
    Hide.show
    Location.go :_0
    #bookmark_jump "_"
    insert txt + "\n"
  end

  def self.insert_at_search_start
    self.clear
    match = self.match
    exchange_point_and_mark
    insert match
  end

  def self.insert_var_at_search_start
    self.clear
    match = self.match
    exchange_point_and_mark
    insert "\#{#{match}}"
  end

#   def self.isearch_delete_rest
#     self.isearch_pull_in_sexp
#     self.isearch_delete
#     ControlLock.disable
#   end

  def self.isearch_select_inner
    self.clear
    set_mark match_beginning(0) + 1
    goto_char match_end(0) - 1
    Effects.blink :what => :region
  end

  def self.isearch_delete
    self.clear
    delete_region match_beginning(0), match_end(0)
  end

  def self.paste_here
    self.clear
    delete_region match_beginning(0), match_end(0)
    insert Clipboard.get("0")
  end

  def self.copy_and_comment
    self.clear
    line = thing_at_point(:line).sub("\n", "")
    Line.to_left
    insert "#"
    exchange_point_and_mark
    insert line
  end

  def self.isearch_open
    self.clear
    Location.go( buffer_substring(match_beginning(0), match_end(0)) )
  end

  def self.jump_to_difflog
    self.clear
    match = buffer_substring(match_beginning(0), match_end(0))
    #  exchange_point_and_mark
    #  insert match
    DiffLog.open
    end_of_buffer
    search_backward match
    recenter
  end

  def self.copy
    self.clear
    match = self.match
    Clipboard.set("0", match)
    set_register ?X, match
    x_select_text match
  end

  def self.cut
    self.clear
    match = self.match
    Clipboard.set("0", match)
    set_register ?X, match
    delete_region match_beginning(0), match_end(0)
  end
  def self.go_to_end
    self.clear
    goto_char match_end(0)
  end

  # Clears the isearch, allowing for inserting, or whatever else
  def self.clear
    isearch_done
    isearch_clean_overlays
  end

  # Do query replace depending on what they type
  def self.query_replace
    first = Keys.input(:timed => true)
    # If they typed 'o', use clipboard 1 and clipboard 2
    if first == "o" || first == "1"
      query_replace_regexp($el.regexp_quote(Clipboard.get("1")), Clipboard.get("2"))
    # If they typed 't', use clipboard 2 and clipboard 1
    elsif first == "t" || first == "2"
      query_replace_regexp(Clipboard.get("2"), Clipboard.get("1"))
    # If 'q', prompt for both args
    elsif first == "q"
      query_replace_regexp(
        Keys.input(:prompt => "Replace: "),
        Keys.input(:prompt => "With: "))
    # If they typed 'c', use clipboard and prompt for 2nd arg
    elsif first == "c"
      query_replace_regexp(Clipboard.get("0"), Keys.input(:prompt => "Replace with (pause when done): ", :timed => true))
    # If they typed 'c', use clipboard and prompt for 2nd arg
    elsif first == "l"
      left, right = elvar.query_replace_defaults.to_a
      query_replace_regexp(left, right)
    # Otherwise, just get more input and use it
    else
      query_replace_regexp(first, Keys.input(:timed => true))
    end
  end

  def self.isearch_query_replace start_with_search_string=false
    self.clear
    match = self.match
    to = start_with_search_string ?
      Keys.input(:prompt => "Change instances of '#{match}' to: ", :initial_input => match) :
      Keys.input(:prompt => "Change instances of '#{match}' to: ")
    query_replace_regexp(match, to)
  end

  def self.grep
    cm_grep("./", read_from_minibuffer("Grep for pattern: "))
  end

  def self.tree_grep
    # Get path (from bookmark)
    dir = Keys.bookmark_as_path

    # Do search
    input = Keys.prefix_u ?
      Clipboard.get("0") :
      Keys.input
    regex = Regexp.new(input, Regexp::IGNORECASE)

    TreeLs.grep dir, regex, :bar => true
  end

  def self.isearch_tree_grep bookmark=nil
    self.clear
    match = self.match

    # Get bookmark
    dir = bookmark ?
      Bookmarks.expand(bookmark) :
      Keys.bookmark_as_path

    # Do search
    regex = Regexp.new(Regexp.quote(match), Regexp::IGNORECASE)

    TreeLs.grep dir, regex, :bar => true
  end

  def self.isearch_tree_grep_method
    self.clear
    match = self.match

#     # Get bookmark
#     dir = Keys.bookmark_as_path
    dir = Bookmarks.expand("$a")

#    insert "\\bdef .*#{match}"

    # Do search
    regex = Regexp.new("\\bdef .*#{match}\\b", Regexp::IGNORECASE)
    TreeLs.grep dir, regex, :bar => true
  end


  # Incrementeal search between cursor and end of paragraph (kills unmatching lines)
  def self.kill_search(left, right)
    pattern = ""
    lines = buffer_substring(left, right).split "\n"
    ch = char_to_string read_char
    while ch =~ /[~#-'>a-zA-Z0-9!*\_'.~#-\/]/
      if ch == "\t"
        pattern = ""
      else
        pattern = pattern + regexp_quote(ch)
        # Filter text and put back
        delete_region left, right
        # Replace out lines that don't match
        lines = lines.grep(/#{pattern}/i)
        # Put back into buffer
        insert lines.join("\n") + "\n"
        right = point
        # Go to first file
        goto_char left
      end
      message "Delete lines not matching: %s", pattern
      ch = char_to_string read_char
    end
    # Run whatever they typed last as a command (it was probably C-m or C-a, etc.)
    case ch
    when "\C-m"  # If it was C-m
      # Do nothing
    else
      command_execute ch
    end
  end

  def self.to_clipboard
    find_this = Clipboard.get(Keys.prefix || "0")

    # Move forward if already there
    if find_this == buffer_substring(point, point + find_this.size)
      forward_char
    end
    if search_forward(find_this).to_s
      goto_char match_beginning(0)
    end
  end

  def self.google
    browse_url "http://google.com/search?q=#{View.selection}"
  end

  def self.isearch_find_in_buffers options={}
    self.clear
    match = self.match
    self.find_in_buffers match, options.merge({:in_bar => true})
  end

  def self.find_in_buffers string, options={}
    options[:current_only] ?
      list = [View.buffer] :
      list = buffer_list

    View.bar if options[:in_bar]

    orig = current_buffer
    found = ""
    list.to_a.each do |b|  # Each buffer open

      file = buffer_file_name(b)
      # Skip if not actual file
      next unless file
      # Skip if a verboten file
      unless options[:current_only]
        next if file =~ /(\/difflog\.notes|\.log|\/\.emacs)$/
      end
      set_buffer b
      started = point
      beginning_of_buffer
      found_yet = nil
      while(true)
        break unless search_forward(string, nil, true)
        unless found_yet
          found << "#{file.sub(/(.+)\//, "\\1\/\n  ")}\n"

          found_yet = true
        end
        found << "    |#{Line.value}\n"
        Line.end
      end
      goto_char started
    end

    switch_to_buffer "*tree find in buffers"
    notes_mode
    erase_buffer
    # If nothing found, just insert message
    if found.size == 0
      return insert("| Note\n- ~Nothing found~\n")
    end
    insert found
    beginning_of_buffer
    highlight_regexp string, :ls_quote_highlight

    if options[:current_only]  # Goto first match
      goto_line 3
      Line.to_words
    else  # Goto first match in 2nd file
      goto_line 2
      re_search_forward "^/", nil, true
      Line.next 2
      Line.to_words
    end

    # Do search if only one file
    if list.size == 1
      TreeLs.search(:recursive => true, :left => View.top, :right => View.bottom)
    end

  end

  def self.highlight_found
    self.clear
    match = buffer_substring(match_beginning(0), match_end(0))

    #Hide.show
    highlight_regexp(match, :hi_yellow)
  end

  def self.hide
    self.clear
    match = buffer_substring(match_beginning(0), match_end(0))
    Hide.hide_unless /#{Regexp.quote(match)}/i
    recenter -3
    Hide.search
  end

  # Insert line at beginning of search
  def self.line
    self.clear
    line = thing_at_point(:line).sub("\n", "")
    exchange_point_and_mark
    insert line
  end

  # During isearch, pull next n words
  def self.isearch_pull_in_words n
    # If on the beginning of a grouping char, move back to catch the sexp
    el4r_lisp_eval "
      (isearch-yank-internal
        (lambda ()
          (forward-word #{n}) (point)))"
  end


  # During isearch, pull next sexp into the search string
  def self.isearch_pull_in_sexp
    # If on the beginning of a grouping char, move back to catch the sexp
    el4r_lisp_eval %q=
      (isearch-yank-internal
        (lambda ()
          (if (and (string-match "[{<\\\\\"'(\\\\[]" (char-to-string (char-before (point))))
            (not (string-match "[{<\\\\\"'(\\\\[]" (char-to-string (char-after (point))))))
            (backward-char))
          (forward-sexp) (point)))=
  end

  # During isearch, open most recently edited file with the search string in its name

  def self.isearch_open_last_edited
    self.clear
    match = buffer_substring(match_beginning(0), match_end(0))

    if match =~ /(.+)\.(.+)/
      # split off, and open
      match, method = $1, $2
    end

    # Convert to snake case, or nil if already in snake case
    snake = TextUtil.snake_case(match)
    snake = nil if snake == match

    match = "#{match}."
    snake = "#{snake}."

    # For each file edited
    found = elvar.editedhistory_history.to_a.find do |p|

      next if p =~ /:/  # Ignore files with colons (tramp)
      name = p[/.*\/(.*)/, 1]  # Strip off path


      # Check for match
      if name =~ /#{Regexp.quote(match)}/i ||
          (snake && name =~ /#{Regexp.quote(snake)}/i)
        p
      else
        false
      end
    end

    # Open it if it matches
    if found
      View.open found
      if method  # If method, go to it
        Move.top
        re_search_forward "^ +def self\\.#{method}"
        recenter 0
      end
    else
      message "'#{match}' not found (no recently edited file with that substring found)."
    end

  end

  def self.isearch_copy_as name
    self.clear
    Clipboard.set(name, self.match)
  end

  def self.isearch_start
    self.clear
    Line.start
  end

  def self.kill_filter
    Line.start
    left = point
    re_search_forward "^$", nil, 1
    right = point
    goto_char left
    TreeLs.search(:left => left, :right => right, :recursive => true)
  end

  def self.to_relative
    if Keys.prefix == 0
      goto_char window_end - 1
      Line.to_left
      return
    end
    goto_char window_start
    ((Keys.prefix || 1) -1).times do
      Line.next
    end
  end

  def self.stop
    Search.clear
    exchange_point_and_mark
  end

  def self.match
    buffer_substring match_beginning(0), match_end(0)
  end

  def self.forward search
    re_search_forward search, nil, true
  end

  def self.backward search
    re_search_backward search, nil, true
  end

  def self.to find
    Move.forward
    if Search.forward(find)
      Move.backward(find.size)
    else
      beep
      message "not found"
      Move.backward
    end
  end

  def self.isearch_open
    Search.clear
    View.open(self.match)
  end

  def self.isearch_google
    Search.clear
    term = self.match
    term.gsub!(' ', '%20')
#return insert term
    browse_url "http://google.com/search?q=#{term}"
  end

  def self.isearch_move_line
    isearch_done
    isearch_clean_overlays
    line = buffer_substring point_at_bol, point_at_eol + 1
    delete_region point_at_bol, point_at_eol + 1
    exchange_point_and_mark
    insert line
  end

  def self.outline_search
    if Keys.prefix_u
      History.open_current :bar => true, :all => true
    else
      History.open_current :all => true
    end
  end

  def self.upcase
    Search.clear
    upcase_region(match_beginning(0), match_end(0))
  end

end
