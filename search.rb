require "hide"
require 'control_lock'
require 'line'
require 'text_util'

class Search

  SPECIAL_ORDER = "_'\"[]{}<>-+\\/!$_"

  extend ElMixin
  @@case_options = nil

  @@log = File.expand_path("~/.emacs.d/search_log.notes")

  def self.menu
    ['.log']
  end

  def self.case_options
    return @@case_options if @@case_options
    @@case_options = []   # Set to empty if not set yet
    self.add_case_option 'upper', proc {|txt| txt.upcase}
    self.add_case_option 'lower', proc {|txt| txt.downcase}
    self.add_case_option 'camel', proc {|txt| TextUtil.camel_case(txt)}
    self.add_case_option 'snake', proc {|txt| TextUtil.snake_case(txt)}
    @@case_options
  end

  # Make another option show up for View.cases
  def self.add_case_option name, the_proc
    # Delete if there already
    @@case_options.delete_if{|i| i.first == name}
    @@case_options << [name, the_proc]
  end

  def self.insert_at_spot
    self.stop
    match = self.match
    Hide.show

    Location.to_spot

    insert match
  end

  def self.insert_tree_at_spot
    self.stop
    txt = FileTree.snippet()   # Grab symbol
    Hide.show
    Location.go :_0
    insert txt + "\n"
  end

  def self.insert_at_search_start
    self.stop
    match = self.match
    self.to_start  # Go back to start
    insert match
  end

  def self.isearch_have_within
    self.stop
    match = self.match
    self.to_start  # Go back to start
    insert match[/^.(.*).$/, 1]
  end

  def self.move_to_search_start match
    was_reverse = self.was_reverse
    View.delete(Search.left, Search.right)

    deleted = View.cursor
    self.to_start  # Go back to start
    Move.backward match.length if was_reverse   # If reverse, move back width of thing deleted

    insert match

    # Save spot where it was deleted (must do after modification, for bookmark to work)
    View.cursor = deleted
    Move.forward(match.length) unless was_reverse
    Location.as_spot('killed')

    self.to_start  # Go back to start
    Move.forward(match.length) unless was_reverse

  end


  def self.insert_var_at_search_start
    self.stop
    match = self.match
    self.to_start  # Go back to start
    insert "\#{#{match}}"
  end

  def self.insert_quote_at_search_start
    self.stop
    match = self.match
    self.to_start
    insert "'#{match}'"
  end

  def self.isearch_select_inner
    self.stop
    set_mark self.left + 1
    goto_char self.right - 1
    Effects.blink :what=>:region
  end

  def self.isearch_delete
    match = self.stop
    # If nothing searched for, go to spot of last delete
    if match.nil?   # If nothing searched for yet, search difflog
      DiffLog.open
      View.to_bottom
      Search.isearch nil, :reverse=>true
    else
      View.delete(Search.left, Search.right)
      Location.as_spot('killed')
    end
  end

  def self.enter txt=nil

    match = self.stop
    txt ||= Clipboard[0]

    if match.nil?   # If nothing searched for yet, do search_edits
      bm = Keys.input :timed=>true, :prompt=>"Enter a bookmark to search edits: "
      path = bm == "." ?
        View.file :
        Bookmarks.expand("$#{bm}")

      DiffLog.open path

      Search.isearch nil, :reverse=>true

    else
      View.delete(Search.left, Search.right)
      View.insert txt
    end

  end

  def self.copy_and_comment
    self.stop
    line = Line.value(1, :include_linebreak=>true).sub("\n", "")
    Code.comment Line.left, Line.right
    self.to_start  # Go back to start
    insert "#{line}"
    Move.to_line_text_beginning
  end

  def self.isearch_just_comment
    self.stop
    Code.comment Line.left, Line.right
    Move.to_line_text_beginning
  end

  def self.isearch_open
    self.stop
    Location.go( self.match )
  end

  def self.just_increment options={}

    self.stop
    match = self.match

    View.delete(Search.left, Search.right)

    orig = View.cursor

    position = SPECIAL_ORDER.index match

    # If one of certain chars, use custom order
    result =
      if position   # Change '[' to ']', etc

        increment_or_decrement = options[:decrement] ? -1 : 1
        SPECIAL_ORDER[position+increment_or_decrement].chr

      else
        if options[:decrement]
          #           match.previous   # Doesn't decrement "10" properly
          match =~ /[a-z]/i ?
          (match[0] - 1) :
            (match.to_i - 1).to_s
        else

          match.next
        end
      end

    View.insert(result)
    View.cursor = orig
  end

  def self.jump_to_difflog
    match = self.stop
    DiffLog.open
    View.to_bottom

    Search.isearch match, :reverse=>true
  end

  def self.just_edits
    match = self.stop
    DiffLog.open View.file
    View.to_bottom

    Search.isearch match, :reverse=>true
  end

  def self.copy
    self.stop
    match = self.match
    Clipboard[0] = self.match
    set_register ?X, match
    x_select_text match
    Clipboard.save_for_yank match   # Store for retrieval with enter_yank
  end

  def self.cut
    match = self.stop

    # If nothing searched for, go to spot of last delete
    if match.nil?   # If nothing searched for yet
      Location.to_spot('killed')
    else
      Clipboard.set(0, match)
      set_register ?X, match
      View.delete self.left, self.right
      Location.as_spot('killed')
    end
  end

  def self.go_to_end
    match = self.stop

    if match.nil?   # If nothing searched for yet
      Search.isearch_restart "$f", :restart=>true
      return
    end

    goto_char self.right
  end

  # Clears the isearch, allowing for inserting, or whatever else
  def self.stop
    match = self.match

    # Make it do special clear if nothing found (to avoid weird isearch error)
    if match.nil?
      if self.not_found?
        match = :not_found
      else
        # Done so isearch_done won't error
        isearch_resume "[^`]", true, nil, true, "", true
        View.message ""
      end
    end

    $el.isearch_done
    $el.isearch_clean_overlays
    match
  end

  def self.to_start
    View.to(elvar.isearch_opoint)
  end

  # Do query replace depending on what they type
  def self.query_replace s1=nil, s2=nil
    if s1 && s2   # If manually passed in
      $el.query_replace_regexp($el.regexp_quote(s1 || ""), s2 || "")
      return
    end

    first = Keys.input(:timed=>true)
    # If they typed 'o', use clipboard 1 and clipboard 2
    if first == "o" || first == "1"
      query_replace_regexp($el.regexp_quote(Clipboard.get("1")), Clipboard.get("2"))
    # If they typed 't', use clipboard 2 and clipboard 1
    elsif first == "t" || first == "2"
      query_replace_regexp(Clipboard.get("2"), Clipboard.get("1"))
    # If 'q', prompt for both args
    elsif first == "q"
      query_replace_regexp(
        Keys.input(:prompt=>"Replace: "),
        Keys.input(:prompt=>"With: "))
    # If they typed 'c', use clipboard and prompt for 2nd arg
    elsif first == "c"
      query_replace_regexp(Clipboard.get, Keys.input(:prompt=>"Replace with (pause when done): ", :timed=>true))
    # If they typed 'c', use clipboard and prompt for 2nd arg
    elsif first == "l"
      query_replace_regexp(@@query_from, @@query_to)
    # Otherwise, just get more input and use it
    else
      query_replace_regexp(first, Keys.input(:timed=>true))
    end
  end

  def self.isearch_query_replace after=nil
    match = self.stop
    was_upper = match =~ /[A-Z]/

    match.downcase!
    left, right = Search.left, Search.right
    before = $el.regexp_quote(match)   # Always start with isearch match

    # If before not there or is :match, prompt for input
    if after.nil? || after == :match
      initial_input = after == :match ? before : ''
      after = Keys.input(:prompt=>"Change instances of '#{before}' to: ", :initial_input=>initial_input)
      @@query_from, @@query_to = before, after
    end

    View.delete left, right
    View.insert was_upper ?
      TextUtil.title_case(after) :
      after

    $el.query_replace_regexp before, after
  end

  def self.grep
    cm_grep("./", read_from_minibuffer("Grep for pattern: "))
  end

  def self.tree_grep
    dir = Keys.bookmark_as_path   # Get path (from bookmark)

    if dir == :space   # If space, search in buffers
      self.find_in_buffers Keys.input(:prompt=>"Search all open files for: ")
      return
    end

    input = case Keys.prefix
      when :u;  Clipboard.get
      when 1;  Clipboard.get("1")
      when 2;  Clipboard.get("2")
      when 3;  Clipboard.get("3")
      when 5;  "^ *(class|module) .*#{Regexp.escape(Keys.input(:prompt=>'Class/module to search for: '))}"
      when 6;  "^ *def .*#{Regexp.escape(Keys.input(:prompt=>'Method to search for: '))}"
      else;  Keys.input(:prompt=>"Text to search for: ")
      end

    input.gsub! "#", "\\#"

    self.append_log dir, "- ###{input}/"

    FileTree.grep_with_hashes dir, input
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
        View.delete left, right
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

  def self.search_in_bookmark match
    bm = Keys.bookmark_as_path

    if bm == :space   # If space, search in buffers
      self.find_in_buffers match
      return
    end

    if bm == :slash   # If space, go back to root and search
      # Make match be orange
      Overlay.face(:ls_search, :left=>self.left, :right=>self.right)
      self.search_at_root match
      return
    end

    View.to_after_bar if View.in_bar?
    match.gsub!(/([#()])/, "\\\\\\1")

    self.append_log bm, "- ###{match}/"

    # Search in bookmark
    FileTree.grep_with_hashes bm, match
  end

  def self.search_like_timer

    match = self.stop

    # Prompt for time
    minutes = Keys.input :prompt=>'How many minutes? ', :timed=>true
    times = {"1"=>"30/30", "2"=>"1/1", "3"=>"90/90", "4"=>"2/2", "5"=>"2:30/2:30", "6"=>"3/3", "9"=>"45/45"}[minutes]

    if match.nil?
      input = Keys.input :prompt=>'Add timer for what? '
      return Firefox.run("$('#timers').val('')", :tab=>1) if input == ""
      input = "#{times} #{input}" unless input =~ /^\d/
    else

      return Firefox.run("$('#timers').val('')", :tab=>1) if input == ""

      input = "#{times} #{match}"
    end

    Firefox.run "$('#timers').val(\"#{input}\n\")", :tab=>1
  end

  def self.search_log
    match = self.stop
    # If nothing searched for, go to place something was copied by name

    Search.log
    View.to_bottom
    Search.isearch match, :reverse=>true

      # Went back to original location of have_name...
      #       loc = Keys.input(:one_char=>true, :prompt=>"Enter one char to go to where it was copied from: ")
      #       Bookmarks.go "_n#{loc}", :point=>true
      #       txt = Clipboard.hash[loc.to_s]
      #       self.isearch txt
      #       return

  end

  def self.left
    match_beginning(0)
  end

  def self.right
    match_end(0)
  end

  def self.isearch_find_in_buffers options={}
    self.stop
    match = self.match
    self.find_in_buffers match, options
  end

  def self.find_in_buffers string, options={}
    string.gsub!('"', '\\"')
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
    CodeTree.launch :no_search=>true
    if new_options[:buffer]   # Goto first match
      $el.goto_line 4
      Line.to_words
      FileTree.search(:recursive=>false, :left=>Line.left, :right=>View.bottom)
    else  # Goto first match in 2nd file
      $el.goto_line 2
      $el.re_search_forward "^  -", nil, true
      Line.next 2
      Line.to_words
    end
    # Do search if only one file
  #     if list.size == 1
  #       FileTree.search(:recursive=>false, :left=>Line.left, :right=>View.bottom)
  #     end

  end


  def self.just_marker
    match = self.stop

    highlight_regexp(Regexp.quote(match), :notes_label)
  end

  def self.highlight_found
    match = self.stop

    highlight_regexp(Regexp.quote(match), :hi_yellow)
  end

  def self.hide
    self.stop
    match = self.match
    Hide.hide_unless /#{Regexp.quote(match)}/i
    recenter -3
    Hide.search
  end

  # Insert line at beginning of search
  def self.have_line
    self.stop
    line = Line.value(1, :include_linebreak=>true).sub("\n", "")
    self.to_start  # Go back to start
    insert line
  end

  # Insert line at beginning of search
  def self.have_label
    self.stop
    label = Line.label
    self.to_start  # Go back to start
    insert "- #{label}: "
  end

  # Insert line at beginning of search
  def self.have_paragraph
    self.stop
    paragraph = View.paragraph
    offset = View.cursor - View.paragraph(:bounds=>true)[0]
    self.to_start  # Go back to start
    orig = Location.new
    insert paragraph
    orig.go
    Move.forward offset
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
          (if (and (> (point) 1)
            (string-match "[{<\\\\\"'(\\\\[]" (char-to-string (char-before (point))))
            )
            (backward-char))
          (forward-sexp) (point)))=

  end

  # During isearch, open most recently edited file with the search string in its name
  def self.isearch_to
    match = self.stop

    if match.nil?   # If nothing searched for yet
      Search.isearch_restart "$t", :restart=>true

    else
      match = self.match
      dir = Keys.bookmark_as_path(:prompt=>"Enter bookmark to look in (or comma for recently edited): ")
      return self.isearch_open_last_edited(match) if dir == :comma   # If key is comma, treat as last edited
      TextUtil.snake_case! match if match =~ /[a-z][A-Z]/   # If camel case, file is probably snake
      FileTree.grep_with_hashes dir, match, '**'   # Open buffer and search
    end
  end

  def self.isearch_open_last_edited match
    if match =~ /(.+)[.#](.+)/
      match, method = $1, $2   # split off, and open
    end

    # Convert to snake case, or nil if already in snake case
    snake = TextUtil.snake_case(match)
    snake = nil if snake == match

    match = "#{match}."
    snake = "#{snake}."
    # For each file edited
    found = (Files.edited_array + Files.history_array).find do |o|

      next if o =~ /.notes$/  # Ignore notes files
      next if o =~ /:/  # Ignore files with colons (tramp)
      name = o[/.*\/(.*)/, 1]  # Strip off path
      # Check for match
      if name =~ /^#{Regexp.quote(match)}/i || (snake && name =~ /^#{Regexp.quote(snake)}/i)
        o
      else
        false
      end
    end

    if found   # Open it if it matches
      View.open found
      if method  # If method, go to it
        Move.top
        # Ol.line
        Search.forward "^ +def \\(self\\.\\)?#{method}[^_a-zA-Z0-9]", :beginning=>true
        Move.to_axis
        recenter 0
      end
    else
      message "'#{match}' not found (no recently edited file with that substring found)."
    end
  end

  def self.isearch_or_copy name
    if self.match.nil?   # If nothing searched for yet
      self.isearch Clipboard[name], :reverse=>self.was_reverse
    else   # Else, if nothing searched for
      self.stop
      Clipboard[name] = self.match
      #       Clipboard.set(name, self.match)
    end
  end

  def self.isearch_copy_as name
    self.stop
    Clipboard.set(name, self.match)
  end

  def self.isearch_start
    self.stop
    Line.start
  end

  def self.kill_filter options={}
    # TODO: Get options[:kill_matching]=>true to delete matching
    # - and map to Keys.do_kill_matching
    Line.start
    left = point
    re_search_forward "^$", nil, 1
    right = point
    goto_char left
    FileTree.search(:left=>left, :right=>right, :recursive=>true)
  end

  def self.cancel
    self.stop
    self.to_start  # Go back to start
  end

  def self.match
    left = self.left
    return nil if left == 0# || self.nil?
    result = buffer_substring(left, self.right)
    return nil if result == ""
    result
  end

  def self.not_found?
    ! elvar.isearch_success
  end

  def self.forward search, options={}
    orig = View.cursor
    found = re_search_forward search, nil, (options[:go_anyway] ? 1 : true)
    View.cursor = orig if options[:dont_move]
    View.cursor = self.left if options[:beginning] && View.cursor != View.bottom

    found
  end

  def self.backward search, options={}
    orig = View.cursor
    found = re_search_backward search, nil, (options[:go_anyway] ? 1 : true)
    View.cursor = orig if options[:dont_move]
    found
  end

  def self.to find
    Move.forward
    if Search.forward(find)
      match = $el.buffer_substring self.left, self.right
      Move.backward(match.size)
    else
      beep
      message "not found"
      Move.backward
    end
  end

  def self.isearch_open
    self.stop
    View.open(self.match)
  end

  def self.isearch_google
    self.stop
    term = self.match
    term.gsub!(' ', '%20')

    term =~ /^https?:\/\// ?   # If url, just browse
      browse_url(term) :
      browse_url("http://google.com/search?q=#{term}")

  end

  def self.isearch_move_line
    isearch_done
    isearch_clean_overlays
    line = buffer_substring point_at_bol, point_at_eol + 1
    View.delete point_at_bol, point_at_eol + 1
    #self.to_start  # Go back to start
    exchange_point_and_mark
    insert line
  end

  def self.outline
    if Keys.prefix_u?
      History.open_current :outline => true, :prompt_for_bookmark => true
    else
      History.open_current :outline => true
    end
  end

  def self.outline_search
    if Keys.prefix_u?
      History.open_current :bar=>true, :all=>true
    else
      History.open_current :all=>true
    end
  end

  def self.upcase
    self.stop
    upcase_region(self.left, self.right)
  end

  def self.downcase
    self.stop
    downcase_region(self.left, self.right)
  end

  def self.enter_search bm=nil, input=nil
    # If line already has something, assume we'll add - ##foo/ to it
    if ! Line[/^ *$/]
      input = Keys.prefix_u ? Clipboard.get : Keys.input(:prompt=>"Text to search for: ")
      indent = Line.indent
      Line.to_right
      View.insert "\n#{indent}  - ###{input}/"
      LineLauncher.launch
      return
    end

    bm ||= Keys.input(:timed=>true, :prompt=>"Enter bookmark in which to search: ")
    return unless bm
    input ||= Keys.prefix_u? ?   # Do search
      Clipboard.get :
      Keys.input(:prompt=>"Text to search for: ")

    if bm == "."   # Do tree in dir from bookmark
      if Line.blank?
        dir = $el.elvar.default_directory
      else
        dir = nil
      end
    else
      dir = Bookmarks.expand("$#{bm}")
    end

    View.insert("- #{dir}" || "")
    indent = Line.indent
    Line.to_right
    View.insert("\n#{indent}  - ###{input}/")
    FileTree.launch

    #View.insert dir
  end

  def self.isearch_log
    match = self.stop
    self.to_start
    return View.insert("Ol.line") if match.nil?
    View.insert "Ol << \"#{match}: \#{#{match}.inspect}\""
  end

  def self.isearch_log_javascript
    match = self.match
    self.stop
    self.to_start
    View.insert "p(\"#{match}: \" + #{match});"
  end

  def self.isearch_as_camel
    self.stop
    term = self.match
    self.to_start
    View.insert TextUtil.camel_case(term)
  end

  def self.isearch_as_snake
    self.stop
    term = self.match
    self.to_start
    View.insert TextUtil.snake_case(term)
  end

  def self.isearch_just_adjust
    self.stop
    Move.forward
    transpose_chars 1
    self.to_start
  end

  # Go to root of tree and do search
  def self.search_at_root txt
    Search.backward("^ *[+-] /")
    # If next line isn't ##... line (will be visually distinct) add linebreak
    unless Line.value(2) =~ /^ *[+-] ##/
      Line.next
      View.insert "\n"
      Line.previous 2
    end
    self.enter_search '.', txt
  end

  def self.just_select
    self.stop
    View.set_mark(Search.right)
    View.to(Search.left)
    Effects.blink :what=>:region
  end

  def self.isearch_just_tag
    self.stop

    left, right = Search.left, Search.right
    tag = Keys.input :timed=>true, :prompt=>"Enter tag name: "
    left_tag = "<#{tag}>"
    right_tag = "</#{tag}>"
    if tag == 'di'
      left_tag = "<div id='#{Keys.input :prompt=>"Enter id: "}'>"
      right_tag = "</div>"
    elsif tag == 'dc'
      left_tag = "<div class='#{Keys.input :prompt=>"Enter id: "}'>"
      right_tag = "</div>"
    end

    View.to(right)
    View.insert right_tag
    View.to(left)
    View.insert left_tag
    View.to right + left_tag.length
  end


  def self.isearch_just_wrap
    self.stop
    left, right = Search.left, Search.right

    wrap_with = Keys.input :timed=>true, :prompt=>"Enter string to wrap match with: "

    View.to(right)
    View.insert wrap_with
    View.to(left)
    View.insert wrap_with
    View.to right + wrap_with.length
  end


  def self.just_orange
    self.stop
    Overlay.face(:notes_label, :left=>Search.left, :right=>Search.right)
  end

  def self.just_edges
    self.stop
    left, right = Search.left+1, Search.right-1
    Effects.blink :left=>left, :right=>right
    View.delete(left, right)
    View.to(Search.left+1)
  end

  def self.isearch_just_surround_with_char left, right=nil
    term = self.match
    right ||= left
    self.stop

    if term == ""
      View.insert "()"
      Move.backward
      return
    end

    View.to(Search.right)
    View.insert right
    View.to(Search.left)
    View.insert left
    View.to Search.left
    #     View.to Search.right + left.length + right.length
  end

  # Copy match as name (like Keys.as_name)
  def self.just_name
    term = self.stop
    loc ||= Keys.input(:one_char=>true, :prompt=>"Enter one char (to store this as): ") || "0"
    Clipboard.copy loc, term
    #     Bookmarks.save("_n#{loc}")
    Effects.blink :left=>left, :right=>right
  end

  def self.just_macro
    self.stop
    Macros.run
  end

  def self.to_left
    match = self.stop
    if match.nil?   # If nothing searched for yet
      dir = Keys.bookmark_as_path :prompt=>"Enter a bookmark to show the log for: "
      CodeTree.display_menu("- Git.menu/\n  - project - #{dir}\n    - .log ''/")
      return
    end

    Line.to_left
  end

  def self.isearch_just_case
    self.stop
    txt = self.match
    lam = Keys.input(:prompt=>'convert to which case?: ', :choices=>TextUtil.case_choices)
    View.delete(Search.left, Search.right)
    View.insert lam[txt]
  end

  def self.isearch_have_case
    self.stop
    txt = self.match
    lam = Keys.input(:prompt=>'convert to which case?: ', :choices=>TextUtil.case_choices)
    self.to_start  # Go back to start
    insert lam[txt]
  end

  def self.isearch_just_underscores
    self.stop
    term = self.match
    View.delete(Search.left, Search.right)
    View.insert TextUtil.snake_case(term)
  end

  def self.zap
    match = self.stop

    if match.nil?   # If nothing searched for yet
      char = Keys.input(:one_char=>true, :prompt=>"Enter one char: ")
      if char == "m"
        CodeTree.display_menu("- $a/\n  - ## def /")
      elsif char == "f"
        CodeTree.display_menu("- $wj/\n  - ##\\bfunction.*\\(/")
      else
        View.beep
        View.message "Don't know what to do with that char."
      end
      return
    end

    right = View.point
    self.to_start   # Go back to search start
    View.delete(View.point, right)
  end

  def self.change_case
    # Prompt user to get char
    char = View.prompt(", lower, camel, snake")
  end

  def self.isearch_restart path, options={}
    term = self.stop

    if path == "$t"   # If $t, open bar
      View.layout_todo
      #       FileTree.open_in_bar; Effects.blink(:what=>:line)
    elsif path == "$f"
      View.layout_files
    elsif path == "$o"
      View.layout_output
      options[:reverse] = true
    elsif path == :top
      # Will go to highest below
    elsif path == :right
      View.layout_right 1
    elsif path == :next
      View.next
    elsif path == :previous
      View.previous
    else
      View.open Bookmarks[path]
    end

    View.wrap unless options[:restart]   # Don't change wrapping if starting search
    View.to_highest

    if options[:reverse]
      View.to_bottom
      options[:restart] ? $el.isearch_backward : self.isearch(term, :reverse=>true)
      return
    end

    options[:restart] ? $el.isearch_forward : self.isearch(term)
  end

  def self.isearch txt=nil, options={}
    txt ||= ""   # Searchig for nil causes error
    isearch_resume txt, (options[:regex] ?true:nil), nil, (! options[:reverse]), txt, true
    isearch_update
  end

  def self.isearch_stop_at_end
    # Kind of a hack - search for anything, so it won't error when we stop
    isearch_resume "[^`]", true, nil, true, "", true

    self.stop
    self.to_start  # Go back to start

    View.message ""
    nil
  end

  def self.isearch_outline
    match = self.stop

    if match.nil?   # If nothing searched for yet
      #       Search.outline_search
      Search.isearch_restart "$o", :restart=>true

    else
      if ! View.file   # If buffer, not file
        buffer_name = View.buffer_name
        txt = View.txt
        View.to_buffer "* outline of matches in: #{buffer_name}"
        Notes.mode
        View.kill_all
        View.insert txt.grep(Regexp.new(match)).join

        return
      end

      # If file
      # search in just one file!

      dir = View.dir
      file_name = View.file_name
      View.to_buffer "*tree grep";  View.dir = dir
      View.clear;  notes_mode
      View.insert "
        - #{dir}/
          - #{file_name}
            - ###{Regexp.quote(match)}/
        ".unindent

      View.to_line 3
      FileTree.launch

      #       Search.isearch_find_in_buffers(:current_only=>true)
    end
  end

  def self.isearch_previous
    match = self.stop

    if ! View.at_bottom && match.nil?   # If nothing searched for yet, search in git diff
      Git.code_tree_diff
      View.to_highest
      Search.isearch nil
      return
    end

    Search.move_to_search_start match
  end

  def self.isearch_next_or_name
    was_reverse = self.was_reverse
    match = self.stop

    if match.nil?   # If nothing searched for yet
      loc = Keys.input(:one_char=>true, :prompt=>"Enter one char to search for corresponding string: ")
      txt = Clipboard.hash[loc.to_s]
      # If there was nothing error out
      return View.message('not found!') if txt.nil?

      self.isearch txt, :reverse=>was_reverse
    else
      self.stop
      $el.next_line
    end
  end

  def self.was_reverse
    ! $el.elvar.isearch_forward
  end

  def self.isearch_clipboard
    reverse = self.was_reverse
    match = self.stop
    if match.nil?   # If nothing searched for yet
      self.isearch Clipboard[0], :reverse=>reverse
    else
      self.copy
      Location.as_spot('clipboard')
    end
  end

  def self.isearch_pause_or_resume
    match = self.stop

    if match.nil?   # If nothing searched for yet, resume search
      Location.to_spot('paused')
      Search.isearch $xiki_paused_isearch_string
    else
      # If search in progress, stop it, remembering spot
      $xiki_paused_isearch_string = self.match.downcase
      Location.as_spot('paused')
    end
  end

  def self.isearch_just_search
    self.just_orange
    match = self.match

    FileTree.to_parent   # Go to parent
    FileTree.to_parent if Line[/^ *- ##/]

    FileTree.insert_under "- \#\##{match}/", :escape=>'', :no_search=>true
    LineLauncher.launch
  end

  def self.isearch_enter_and_next
    if self.match == ""   # If nothing searched for yet, go to where last copy happened
      self.stop
      Location.to_spot('clipboard')
      Search.isearch Clipboard[0]
      return
    end

    self.stop
    match = self.match
    View.delete(Search.left, Search.right)
    View.insert Clipboard[0]
    Search.isearch match
  end

  def self.isearch_move_to path
    match = self.match
    match = Line.value if match.nil?   # Use line if nothing searched for
    Search.stop

    orig = Location.new

    if path == "$t"   # If $f, grab path also
      View.layout_todo
    elsif path == "$f"   # If $f, grab path also
      match = FileTree.snippet(match)
      match = "- #{match.sub(/^  /, '  - ')}"
      View.layout_files
    else
      View.open path
    end

    View.to_highest

    View.insert("\n", :dont_move=>true) unless Line.blank?   # Make room if line not blank

    View.insert match

    # Add line after if before heading
    unless match =~ /\n$/   # If there wasn't a linebreak at the end of the match
      Line.next
      View.insert("\n", :dont_move=>true) if Line[/^\|/]
    end

    View.to_highest
    orig.go
  end

  def self.log
    View.open @@log
  end

  def self.append_log dir, txt#, prefix=''
    txt = "- #{dir}\n  #{txt}\n"
    File.open(@@log, "a") { |f| f << txt } rescue nil
  end

  def self.bookmark
    match = self.stop

    if match.nil?   # If nothing searched for yet, resume search
      Search.tree_grep
    else
      Search.search_in_bookmark match
    end
  end

end
