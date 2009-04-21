require "hide"
require 'control_lock'
require 'line'
require 'text_util'

class Search
  extend ElMixin
  @@case_options = nil

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
    self.clear
    match = self.match
    Hide.show

    Location.to_spot

    insert match
  end

  def self.insert_tree_at_spot
    self.clear
    txt = FileTree.snippet()   # Grab symbol
    Hide.show
    Location.go :_0
    insert txt + "\n"
  end

  def self.insert_at_search_start
    self.clear
    match = self.match
    self.to_start  # Go back to start
    insert match
  end

  def self.isearch_have_within
    self.clear
    match = self.match
    self.to_start  # Go back to start
    insert match[/^.(.*).$/, 1]
  end

  def self.move_to_search_start
    self.clear
    was_reverse = elvar.isearch_opoint > point
    match = self.match
    View.delete(Search.left, Search.right)

    Location.as_spot('deleted')
    self.to_start  # Go back to start

    # If reverse, move back width of thing deleted
    Move.backward match.length if was_reverse

    insert match

  end


  def self.insert_var_at_search_start
    self.clear
    match = self.match
    self.to_start  # Go back to start
    insert "\#{#{match}}"
  end

  def self.insert_quote_at_search_start
    self.clear
    match = self.match
    self.to_start
    insert "'#{match}'"
  end

  def self.isearch_select_inner
    self.clear
    set_mark match_beginning(0) + 1
    goto_char match_end(0) - 1
    Effects.blink :what=>:region
  end

  def self.isearch_delete
    self.clear
    View.delete(Search.left, Search.right)
  end

  def self.paste_here
    self.clear
    View.delete(Search.left, Search.right)
    insert Clipboard.get
  end

  def self.copy_and_comment
    self.clear
    line = Line.value(1, :include_linebreak=>true).sub("\n", "")
    Code.comment Line.left, Line.right
    self.to_start  # Go back to start
    insert "#{line}"
    Move.to_line_text_beginning
  end

  def self.isearch_open
    self.clear
    Location.go( self.match )
  end

  def self.just_increment
    self.clear
    match = self.match
    View.delete(Search.left, Search.right)

    orig = View.cursor
    View.insert((match.to_i + 1).to_s)
    View.cursor = orig
  end

  def self.jump_to_difflog
    self.clear
    match = self.match
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
    Keys.save_for_yank match   # Store for retrieval with enter_yank
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
    $el.isearch_done
    $el.isearch_clean_overlays
  end

  def self.to_start
    View.to(elvar.isearch_opoint)
  end

  # Do query replace depending on what they type
  def self.query_replace
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

  def self.isearch_query_replace start_with_search_string=false

    txt = $el.regexp_quote($el.elvar.isearch_string)

    self.clear
    to = start_with_search_string ?
      Keys.input(:prompt=>"Change instances of '#{txt}' to: ", :initial_input=>txt) :
      Keys.input(:prompt=>"Change instances of '#{txt}' to: ")
    @@query_from, @@query_to = txt, to
    query_replace_regexp(txt, to)
  end

  def self.grep
    cm_grep("./", read_from_minibuffer("Grep for pattern: "))
  end

  def self.tree_grep
    dir = Keys.bookmark_as_path   # Get path (from bookmark)
    input = case Keys.prefix
      when :u;  Clipboard.get
      when 1;  Clipboard.get("1")
      when 2;  Clipboard.get("2")
      else;  Keys.input(:prompt=>"Text to search for: ")
      end

    FileTree.grep_with_hashes dir, input
  end

  def self.isearch_tree_grep_method
    self.clear
    match = self.match

    dir = Bookmarks.expand("$a")

    # Do search
    regex = Regexp.new("\\bdef .*#{match}\\b", Regexp::IGNORECASE)
    FileTree.grep dir, regex, :bar=>true
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

  def self.uncover

    self.clear
    match = self.match

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

    # Search in bookmark
    FileTree.grep_with_hashes bm, match

  end

  def self.left
    match_beginning(0)
  end

  def self.right
    match_end(0)
  end

  def self.isearch_find_in_buffers options={}
    self.clear
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

  def self.highlight_found
    self.clear
    match = self.match

    #Hide.show
    highlight_regexp(match, :hi_yellow)
  end

  def self.hide
    self.clear
    match = self.match
    Hide.hide_unless /#{Regexp.quote(match)}/i
    recenter -3
    Hide.search
  end

  # Insert line at beginning of search
  def self.have_line
    self.clear
    line = Line.value(1, :include_linebreak=>true).sub("\n", "")
    self.to_start  # Go back to start
    insert line
  end

  # Insert line at beginning of search
  def self.have_label
    self.clear
    label = Line.label
    self.to_start  # Go back to start
    insert "- #{label}: "
  end

  # Insert line at beginning of search
  def self.have_paragraph
    self.clear
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

  #     el4r_lisp_eval %q=
  #       (isearch-yank-internal
  #         (lambda ()
  #           (if (and (string-match "[{<\\\\\"'(\\\\[]" (char-to-string (char-before (point))))
  #             (not (string-match "[{<\\\\\"'(\\\\[]" (char-to-string (char-after (point))))))
  #             (backward-char))
  #           (forward-sexp) (point)))=

  end

  # During isearch, open most recently edited file with the search string in its name

  def self.isearch_to
    self.clear
    match = self.match

    # Get key
    dir = Keys.bookmark_as_path(:prompt=>"Enter bookmark to look in (or comma for recently edited): ")

    # If key is comma, treat as last edited
    return self.isearch_open_last_edited(match) if dir == :comma

    # Open buffer and search
    FileTree.grep_with_hashes dir, match, '**'
  end

  def self.isearch_open_last_edited match

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
        re_search_forward "^ +def self\\.#{method}\\>"
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
    FileTree.search(:left=>left, :right=>right, :recursive=>true)
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
    self.to_start  # Go back to start
  end

  def self.match
    buffer_substring(match_beginning(0), match_end(0))
  end

  def self.forward search, options={}
    orig = View.cursor
    found = re_search_forward search, nil, (options[:go_anyway] ? 1 : true)
    View.cursor = orig if options[:dont_move]
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

  def self.isearch_url
    Search.clear
    term = self.match
    term.gsub!(' ', '%20')
    browse_url term
  end

  def self.isearch_move_line
    isearch_done
    isearch_clean_overlays
    line = buffer_substring point_at_bol, point_at_eol + 1
    delete_region point_at_bol, point_at_eol + 1
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
    Search.clear
    upcase_region(match_beginning(0), match_end(0))
  end

  def self.downcase
    Search.clear
    downcase_region(match_beginning(0), match_end(0))
  end

  def self.enter_search bm=nil, input=nil
    # If line already has something, assume we'll add - ##foo/ to it
    if ! Line.matches(/^ *$/)
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
    match = self.match
    self.clear
    self.to_start
    View.insert "Ol << \"#{match}: \#{#{match}.inspect}\""
  end

  def self.isearch_log_javascript
    match = self.match
    self.clear
    self.to_start
    View.insert "console.log(\"#{match}: \" + #{match})"
  end

  def self.isearch_as_camel
    Search.clear
    term = self.match
    self.to_start
    View.insert TextUtil.camel_case(term)
  end

  def self.isearch_as_snake
    Search.clear
    term = self.match
    self.to_start
    View.insert TextUtil.snake_case(term)
  end

  def self.isearch_just_adjust
    Search.clear
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
    Search.clear
    View.set_mark(Search.right)
    View.to(Search.left)
    Effects.blink :what=>:region
  end

  def self.isearch_just_tag
    Search.clear

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
    Search.clear
    left, right = Search.left, Search.right

    wrap_with = Keys.input :timed=>true, :prompt=>"Enter string to wrap match with: "

    View.to(right)
    View.insert wrap_with
    View.to(left)
    View.insert wrap_with
    View.to right + wrap_with.length
  end



  def self.just_orange
    Search.clear
    Overlay.face(:notes_label, :left=>Search.left, :right=>Search.right)
  end

  def self.just_edges
    Search.clear
    left, right = Search.left+1, Search.right-1
    Effects.blink :left=>left, :right=>right
    View.delete(left, right)
    View.to(Search.left+1)
  end

  def self.isearch_just_surround_with_char left, right=nil
    right ||= left
    Search.clear
    View.to(Search.right)
    View.insert right
    View.to(Search.left)
    View.insert left
    View.to Search.right + left.length + right.length
  end

  # Copy match as name (like Keys.as_name)
  def self.just_name
    Search.clear
    term = self.match
    Clipboard.copy nil, term
    Effects.blink :left=>left, :right=>right
  end

  def self.just_macro
    Search.clear
    Macros.run
  end

  def self.to_left
    Search.clear
    Line.to_left
  end

  def self.isearch_just_case
    self.clear
    txt = self.match
    lam = Keys.input(:prompt=>'convert to which case?: ', :choices=>TextUtil.case_choices)
    View.delete(Search.left, Search.right)
    View.insert lam[txt]
  end

  def self.isearch_have_case
    self.clear
    txt = self.match
    lam = Keys.input(:prompt=>'convert to which case?: ', :choices=>TextUtil.case_choices)
    self.to_start  # Go back to start
    insert lam[txt]
  end


  def self.isearch_just_underscores
    Search.clear
    term = self.match
    View.delete(Search.left, Search.right)
    View.insert TextUtil.snake_case(term)
  end

  def self.zap
    self.clear
    right = View.point
    self.to_start   # Go back to search start
    View.delete(View.point, right)
  end

  def self.change_case
    # Prompt user to get char
    char = View.prompt(", lower, camel, snake")
  end
end
