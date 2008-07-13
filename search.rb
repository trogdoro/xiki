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
    insert match
  end

  def self.insert_tree_at_spot
    self.clear
    txt = TreeLs.snippet()   # Grab symbol
    Hide.show
    Location.go :_0
    insert txt + "\n"
  end

  def self.insert_at_search_start
    self.clear
    match = self.match
    goto_char(elvar.isearch_opoint)  # Go back to start
    insert match
  end

  def self.move_to_search_start
    self.clear
    match = self.match
    delete_region match_beginning(0), match_end(0)
    goto_char(elvar.isearch_opoint)  # Go back to start
    insert match
  end


  def self.insert_var_at_search_start
    self.clear
    match = self.match
    goto_char(elvar.isearch_opoint)  # Go back to start
    insert "\#{#{match}}"
  end

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
    goto_char(elvar.isearch_opoint)  # Go back to start
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
      query_replace_regexp(@@query_from, @@query_to)
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
    @@query_from, @@query_to = match, to
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

  def self.google
    browse_url "http://google.com/search?q=#{View.selection}"
  end

  def self.isearch_find_in_buffers options={}
    self.clear
    match = self.match
    self.find_in_buffers match, options#.merge({:in_bar => true})
  end

  def self.find_in_buffers string, options={}
    new_args = "\"#{string}\""
    new_options = {}
    new_options[:buffer] = View.buffer_name if options[:current_only]
    new_args << ", #{new_options.inspect[1..-2]}" unless new_options.empty?

    View.bar if options[:in_bar]
    switch_to_buffer "*tree find in buffers"
    notes_mode
    erase_buffer
    View.insert "+ Buffers.search #{new_args}/"
    open_line 1
    CodeTree.launch :no_search => true
    if new_options[:buffer]   # Goto first match
      $el.goto_line 4
      Line.to_words
      TreeLs.search(:recursive => false, :left => Line.left, :right => View.bottom)
    else  # Goto first match in 2nd file
      $el.goto_line 2
      $el.re_search_forward "^  -", nil, true
      Line.next 2
      Line.to_words
    end
    # Do search if only one file
#     if list.size == 1
#       TreeLs.search(:recursive => false, :left => Line.left, :right => View.bottom)
#     end

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
    goto_char(elvar.isearch_opoint)  # Go back to start
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
    goto_char(elvar.isearch_opoint)  # Go back to start
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
    #goto_char(elvar.isearch_opoint)  # Go back to start
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
