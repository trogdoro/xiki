require "hide"
require 'control_lock'
require 'line'
require 'text_util'

class Search

  SPECIAL_ORDER = "X!='\"[]{}<>_-+*@#\\/!$X"

  extend ElMixin
  @@case_options = nil
  @@outline_goto_once = nil

  @@log = File.expand_path("~/.emacs.d/search_log.notes")

  def self.menu
    '
    - .history/
    - .log/
    - .launched/
    - docs/
      | > Summary
      | Some interesting keys to type while searching.
      | Note you start a search by typing Control-s.
      |
      | > What do we mean by "search_bookmark" etc.?
      | With all xiki keyboard shorcuts, you "type the acronym".
      | By the key shortcut "search_bookmark" we mean typing Control-s and then
      | Control-c.
      |
      | Of course, typing Control-s to search lets you type characters
      | to search for, so in some cases typing a search string in between makes
      | sense.  So, search_clipboard actually means you would type Control-s then
      | some characters to search for then Control-c to copy to the clipboard.
      |
      | > Examples
      - examples/
        | search_copy: Copy found to clipboard
        | search_bookmark: Search text of files in a dir
        | search_all: Show all previous searches
        | search_value: Insert found where search began
        | search_delete: Delete found
        | search_diffs (without searching): Search in diffs
        | search_todo: Search in $t bookmark
        | search_files: Search in $f bookmark
        | search_paths: Search history of menus
      - miscellaneous/
        | search_search: Re-do the last search
        | search_word: Suck the next word in
        | search_yank: Suck the rest of the line in
        | search_usurp: Suck the next expression in
      |
      | For more details about Xiki keyboard shortcuts, see:
      - @keys/docs/
      |
    '
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
    match = self.stop
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
    match = self.stop

    if match.nil?   # If nothing searched for yet, search difflog
      loc = Keys.input(:chars=>1, :prompt=>"Enter one char to search for corresponding string: ")
      loc = loc.to_s
      txt = Clipboard.hash[loc.to_s] || Clipboard.hash_by_first_letter[loc.to_s]

      txt ||= self.searches.find{|o| o =~ /^#{loc}/i}
      return View.message("Nothing to search for matching '#{loc}'.", :beep=>1) if txt.nil?

      self.isearch txt, :reverse=>was_reverse

      return
    end

    self.to_start  # Go back to start
    insert match
  end

  def self.isearch_have_within
    match = self.stop
    self.to_start  # Go back to start
    insert match[/^.(.*).$/, 1]
  end

  def self.move_to_search_start match
    was_reverse = self.was_reverse
    View.delete(Search.left, Search.right)

    deleted = View.cursor
    self.to_start  # Go back to start
    Move.backward match.length if was_reverse   # If reverse, move back width of thing deleted

    View.insert match

    # Save spot where it was deleted (must do after modification, for bookmark to work)
    View.cursor = deleted
    Move.forward(match.length) unless was_reverse
    Location.as_spot('killed')

    self.to_start  # Go back to start
    Move.forward(match.length) unless was_reverse
  end

  def self.insert_var_at_search_start
    match = self.stop
    self.to_start  # Go back to start
    insert "\#{#{match}}"
  end

  def self.insert_quote_at_search_start
    match = self.stop
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

      return Launcher.open("- DiffLog.diffs/") if bm == "8" || bm == " "

      path = bm == "." ? View.file : "$#{bm}"
      return Launcher.open("- DiffLog.diffs \"#{path}\"/")
    end

    View.delete(Search.left, Search.right)
    View.insert txt
  end

  def self.copy_and_comment
    self.stop
    line = Line.value(1, :include_linebreak=>true).sub("\n", "")
    Code.comment Line.left, Line.right
    self.to_start  # Go back to start
    insert "#{line}"
    Line.to_beginning
  end

  def self.isearch_just_comment
    self.stop
    Code.comment Line.left, Line.right
    Line.to_beginning
  end

  def self.just_increment options={}

    match = self.stop

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

  def self.copy match
    Clipboard[0] = match
    set_register ?X, match
    x_select_text match
    Clipboard.save_by_first_letter match   # Store for retrieval with enter_yank
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
  def self.stop options={}

    left, right = self.left, self.right

    txt = self.match(left, right)# if options[:match]

    # Make it do special clear if nothing found (to avoid weird isearch error)
    if txt.nil?
      if elvar.isearch_success # || Search.left == View.bottom
        # Done so isearch_done won't error
        isearch_resume "[^`]", true, nil, true, "", true
        View.message ""
      else
        txt = :not_found
      end
    end

    $el.elvar.isearch_mode = nil

    $el.isearch_clean_overlays
    $el.isearch_done

    txt == :not_found ? Search.searches[0] : txt
  end

  def self.match left=nil, right=nil
    left ||= self.left
    right ||= self.right

    return nil if left == 0# || self.nil?
    result = buffer_substring(left, right)

    return nil if result == ""
    result
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
    match = self.stop#
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

  def self.tree_grep # prefix=nil

    prefix = Keys.isearch_prefix
    path = Keys.bookmark_as_path :include_file=>1   # Get path (from bookmark)

    # If C-u, just jump to bookmark and search from the top
    if prefix == :u
      View.open path
      View.to_highest
      Search.isearch
      return
    end

    if path == :space   # If space, search in buffers
      self.find_in_buffers Keys.input(:prompt=>"Search all open files for: ")
      return
    end

    input = Keys.input(:prompt=>"Text to search for: ")

    input.gsub! "#", "\\#"

    FileTree.grep_with_hashes path, input
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

    bm = Keys.bookmark_as_path :include_file=>1

    if bm == :slash   # If space, go back to root and search
      # Make match be orange
      Overlay.face(:ls_search, :left=>self.left, :right=>self.right)
      self.search_at_root match
      return
    end

    View.to_after_bar if View.in_bar?

    if bm == :space   # If space, search in buffers
      self.find_in_buffers match
      return
    end

    match.gsub!(/([#()*+?^$\[\]|.])/, "\\\\\\1")

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
      return Firefox.run("$('#timers').val('')", :tab=>0) if input == ""
      input = "#{times} #{input}" unless input =~ /^\d/
    else
      return Firefox.run("$('#timers').val('')", :tab=>0) if input == ""
      input = "#{times} #{match}"
    end

    # Replace field
    Firefox.run "$('#timers').val(\"#{input}\")", :tab=>0
  end

  def self.subtract
    match = self.match

    if match   # If currently searching
      return $el.isearch_del_char
    end

    View.message "Unused!", :beep=>1

    self.stop
    self.search_last_launched
  end

  def self.search_last_launched
    match = self.stop

    Launcher.open Launcher.last_launched_menu
  end

  def self.launched bm=nil

    txt = File.read @@log
    txt = txt.sub(/\A- /, '').split(/^- /).reverse.uniq
    if bm && bm == "#"
      txt = txt.select{|o| o =~ /^  - ##/}
    elsif bm && bm == ":"
      txt = txt.select{|o| o =~ /^    - [^#].*: /}
    elsif bm

      path = Bookmarks[bm]

      if File.file? path   # File
        regex = /^#{Regexp.escape File.dirname path}\/\n  - #{Regexp.escape File.basename path}/
      else   # Dir
        regex = /^#{Regexp.escape path}/
        path = "#{path}/" if path !~ /\/$/
      end

      txt = txt.select{|o| o =~ regex}
    end

    result = "- #{txt.join("- ")}"
    result
  end

  def self.left
    match_beginning(0)
  end

  def self.right
    match_end(0)
  end

  def self.isearch_find_in_buffers options={}
    match = self.stop
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
      Tree.search(:recursive=>false, :left=>Line.left, :right=>View.bottom)
    else  # Goto first match in 2nd file
      $el.goto_line 2
      $el.re_search_forward "^  -", nil, true
      Line.next 2
      Line.to_words
    end
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
    match = self.stop
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
      #       match = self.match
      dir = Keys.bookmark_as_path(:prompt=>"Enter bookmark to look in (or space for recently edited): ")

      return View.message("Use space!", :beep=>1) if dir == :comma

      return self.isearch_open_last_edited(match) if dir == :space   # If key is comma, treat as last edited

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
        Search.forward "^ +def \\(self\\.\\)?#{method}[^_a-zA-Z0-9]", :beginning=>true
        Move.to_axis
        recenter 0

        dir, name = found.match(/(.+\/)(.+)/)[1..2]
        Search.append_log dir, "- #{name}\n    | #{Line.value}"

      end
    else
      message "'#{match}' not found (no recently edited file with that substring found)."
    end
  end

  def self.isearch_or_copy name
    match = self.stop

    if match.nil?   # If nothing searched for yet
      self.isearch Clipboard[name].downcase, :reverse=>self.was_reverse
    else   # Else, if nothing searched for
      self.stop
      Clipboard[name] = match
    end
  end

  def self.isearch_copy_as name
    self.stop
    Clipboard.set(name, self.match)
  end

  def self.kill_filter options={}
    # TODO: Get options[:kill_matching]=>true to delete matching
    # - and map to Keys.do_kill_matching
    Line.start
    left = point
    re_search_forward "^$", nil, 1
    right = point
    goto_char left
    Tree.search(:left=>left, :right=>right, :recursive=>true)
  end

  def self.cancel
    self.stop
    self.to_start  # Go back to start
  end

  #   def self.not_found?
  #     # Note this returns false when nothing searched for

  #     ! elvar.isearch_success
  #   end

  def self.forward search, options={}
    View.to_highest if options[:from_top]

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

  def self.isearch_google options={}
    term = self.stop
    if term
      term.gsub!(' ', '+')

      term = "\"#{term}\"" if options[:quote]

      term =~ /^https?:\/\// ?   # If url, just browse
        browse_url(term) :
        browse_url("http://google.com/search?q=#{term}")
      return
    end

    Firefox.log
    Search.isearch nil, :from_bottom=>true

  end

  def self.search_thesaurus
    term = self.stop

    url = term.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
    browse_url "http://thesaurus.reference.com/browse/#{url}"
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
      Launcher.launch
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

  end

  def self.isearch_log
    match = self.stop
    self.to_start
    return View.insert("Ol.line") if match.nil?
    View.insert "Ol << \"#{match}: \#{#{match}.inspect}\""
  end

  def self.isearch_log_javascript
    match = self.stop
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

    right ||= left
    term = self.stop

    if term == ""
      View.insert "()"
      Move.backward
      return
    end

    View.to(Search.left + term.length)
    View.insert right
    View.to(Search.left)
    View.insert left
    View.to Search.left
  end

  # Copy match as name (like Keys.as_name)
  def self.just_name
    term = self.stop
    loc ||= Keys.input(:chars=>1, :prompt=>"Enter one char (to store this as): ") || "0"
    Clipboard.copy loc, term
    Effects.blink :left=>left, :right=>right
  end

  def self.just_macro
    self.stop
    Macros.run
  end

  def self.to_left
    match = self.stop
    if match.nil?   # If nothing searched for yet
      return Launcher.open("- search/.history/")
    end

    Line.to_left
  end

  def self.just_menu
    match = self.stop
    View.open "$ml"
    View.to_bottom
    Search.isearch match, :reverse=>true
  end

  def self.isearch_just_case
    txt = self.stop

    return Search.isearch(Clipboard[0]) if txt.nil?

    choice = Keys.input(:prompt=>'convert to which case?: ', :choices=>TextUtil.case_choices)
    View.delete(Search.left, Search.right)
    View.insert choice[txt]
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
      char = Keys.input(:chars=>1, :prompt=>"Enter one char: ")
      if char == "m"
        Launcher.open("- $a/\n  - ## def /")
      elsif char == "f"
        Launcher.open("- $wj/\n  - ##\\bfunction.*\\(/")
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

  def self.xiki
    match = self.stop

    if match.nil?   # If nothing searched for yet
      char = Keys.input(:chars=>1, :prompt=>"Enter one char: ")
      if char == "m"
        Launcher.open("- $x/\n  - ##\\bdef /")
      else
        View.beep
        View.message "Don't know what to do with that char."
      end
      return
    end

    View.beep
    View.message "use 'pull'" + "!" * 600
  end

  def self.isearch_restart path, options={}
    self.stop
    term = Search.searches[0]

    if path == "$t"   # If $t, open bar
      View.layout_todo
      #       FileTree.open_in_bar; Effects.blink(:what=>:line)
    elsif path == "$f"
      View.layout_files
    elsif path == "$o"
      View.layout_output
      options[:reverse] = true
    elsif path == "$d"
      View.open "$d"
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

    if options[:from_bottom]
      View.to_bottom
      options[:reverse] = true
    end

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
      # Do isearch in Ol buffer
      Search.isearch_restart "$o", :restart=>true

    else
      if ! View.file   # If buffer, not file
        buffer_name = View.buffer_name
        txt = View.txt
        View.to_buffer "* outline of matches in #{buffer_name}"
        Notes.mode
        View.kill_all
        View.insert txt.grep(Regexp.new(match)).join

        return
      end

      # If file

      Search.outline_goto_once = Line.number

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

  def self.isearch_paths
    was_reverse = self.was_reverse
    match = self.stop
    return Line.previous if match.nil? && was_reverse   # Odd case, user might do this if at end of file
    return Git.search_repository if match.nil?

    Search.move_to_search_start match
  end

  def self.isearch_next
    was_reverse = self.was_reverse
    match = self.stop
    if match.nil? && Keys.before_last == "19"   # If nothing searched for yet
      self.stop
      self.search_last_launched
    else

      self.stop
      $el.next_line
    end
  end

  def self.was_reverse
    ! $el.elvar.isearch_forward
  end

  def self.isearch_clipboard
    txt = Search.stop

    if txt.nil?   # If nothing searched for yet
      Console.search_last_commands
    else
      self.copy txt
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

    Tree.to_parent   # Go to parent
    Tree.to_parent if Line[/^ *- ##/]

    Tree.under "- \#\##{match}/", :escape=>'', :no_search=>true
    Launcher.launch
  end

  def self.isearch_enter_and_next
    if self.match == ""   # If nothing searched for yet, go to where last copy happened
      self.stop
      Location.to_spot('clipboard')
      Search.isearch Clipboard[0]
      return
    end

    match = self.stop
    View.delete(Search.left, Search.right)
    View.insert Clipboard[0]
    Search.isearch match
  end

  def self.isearch_move_to path
    match = self.stop
    match = Line.value if match.nil?   # Use line if nothing searched for
    self.move_to path, match
  end

  def self.move_to path, match
    Search.stop
    orig = Location.new
    was_in_bar = View.in_bar?

    if path == "$t"   # If $f, grab path also
      View.layout_todo
    elsif path == "$f"   # If $f, grab path also
      match = FileTree.snippet(match)
      match = ">\n- #{match.sub(/^  /, '  - ')}\n"
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
      View.insert("\n", :dont_move=>true) if Line[/^>/]
    end

    View.to_highest

    return if path == "$t" && (was_in_bar && ! orig.buffer == "todo.notes")
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

  def self.searches
    elvar.search_ring.to_a
  end

  def self.history txt=nil
    if txt
      txt.sub! /^\| /, ''

      # Go back to where we came from, if we're in special search buffer
      ControlTab.go if View.buffer_name == "*CodeTree - search/history/"

      Search.isearch txt
      return
    end
    searches = self.searches.uniq
    searches.map{|o| "| #{o}\n"}.join("")
  end

  def self.outline_goto_once; @@outline_goto_once; end
  def self.outline_goto_once= txt; @@outline_goto_once = txt; end

  def self.deep_outline txt, line
    txt = txt.split "\n"
    target_i = line

    # Start with current line

    i, children, matched_above = target_i-1, false, 1
    result = [txt[i]]
    target_indent = txt[i][/^ */].length / 2

    # Go through each line above

    while (i -= 1) >= 0
      # Grab lines with incrementally lower indent, but only if they have lines under!

      line = txt[i]
      next if line.empty?
      indent = line[/^ */].length / 2

      if indent > target_indent   # If lower, skip, remembering children
        children = true
      elsif indent == target_indent   # If same, only grab if there were children
        if children
          matched_above += 1
          result.unshift txt[i]
        end
        children = false
      else   # Indented less

        next if indent == 0 && line =~ /^#? ?Ol\b/   # Skip if ^Ol... line

        matched_above += 1
        result.unshift txt[i]
        children = false
        target_indent = indent
      end
    end


    i, candidate = target_i-1, nil
    target_indent = txt[i][/^ */].length / 2

    # Go through each line below

    while (i += 1) < txt.length
      # Grab lins with incrementally lower indent, but only if they have lines under!

      line = txt[i]
      next if line.empty?
      indent = line[/^ */].length / 2

      if indent > target_indent   # If lower, add candidate if any
        if candidate
          result << candidate
          candidate = nil
        end

      elsif indent == target_indent   # If same, only grab if there were children
        candidate = txt[i]
      else   # Indented less

        next if indent == 0 && line =~ /^#? ?Ol\b/   # Skip if ^Ol... line

        target_indent = indent
        candidate = txt[i]
      end

    end

    [result.join("\n")+"\n", matched_above]
  end

  def self.isearch_m
    match = self.stop

    return if match   # If there was a match, just stop

    Launcher.open("- log/") if match.nil?
  end

  def self.just_bookmark
    match = self.stop
    path = Keys.bookmark_as_path :include_file=>1   # Get path (from bookmark)
    View.open path
    View.to_highest
    Search.isearch match
  end

end
