require 'xiki/core/hide'
require 'xiki/core/control_lock'
require 'xiki/core/line'
require 'xiki/core/text_util'

module Xiki
  class Search

    # Used by search+just+Plus and search+just+Minus
    SPECIAL_ORDER = "X!='\"[]{}<>?-+ +*@#\\/!$X:|"
    # These override it
    SPECIAL_ORDER_MINUS = {
      " "=>"-",
      "+"=>"-",
      "-"=>"?",
      "?"=>"-",
      }
    SPECIAL_ORDER_PLUS = {
      " "=>"+",
      "-"=>"+",
      "+"=>" ",
      "?"=>"+",
      "@"=>"=",
      }

    @@case_options = nil

    # Deprecated?  Has to do with moving cursor to line.  But why can't we just pass it in?  Try doing that.
    @@outline_goto_once = nil
    def self.outline_goto_once; @@outline_goto_once; end
    def self.outline_goto_once= txt; @@outline_goto_once = txt; end

    @@log = File.expand_path("~/xiki/misc/logs/search_log.notes")

    MENU = '
      - .history/
      - .log/
      - .launched/
      - docs/
        | > Summary
        | You start a search by typing Control-s.  Then type the search string.
        | Here are some interesting keys to type while searching, to do things
        | with the match.
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
          | search_todo: Search in :t bookmark
          | search_files: Search in :n bookmark
          | search_paths: Search history of menus
        - miscellaneous/
          | search_search: Re-do the last search
          | search_word: Suck the next word in
          | search_yank: Suck the rest of the line in
          | search_usurp: Suck the next expression in
        |
        | For more details about Xiki keyboard shortcuts, see:
        + =keys/docs/
        |
      - see/
        <= next/
      '

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

      $el.insert match
    end

    def self.insert_tree_at_spot
      self.stop
      txt = FileTree.snippet   # Grab symbol
      Hide.show
      Location.go :_0
      $el.insert txt
    end

    def self.insert_at_search_start
      was_reverse = self.was_reverse
      match = self.stop

      if match.nil?   # If nothing searched for yet, search for variable
        loc = Keys.input(:chars=>1, :prompt=>"Enter variable name (one char) to search: ")
        loc = loc.to_s

        txt = Clipboard.hash[loc.to_s] || Clipboard.hash_by_first_letter[loc.to_s]
        txt ||= self.searches.find{|o| o =~ /^#{loc}/i}

        return View.message("Nothing to search for matching '#{loc}'.", :beep=>1) if txt.nil?
        self.isearch txt, :reverse=>was_reverse

        return
      end

      self.to_start  # Go back to start
      #       View.insert match, :dont_move=>1
      View.insert match #, :dont_move=>1
      View.message ""
    end

    def self.isearch_have_wikipedia
      term = self.stop
      Wikipedia.wp term
    end

    def self.isearch_have_within
      match = self.stop
      self.to_start  # Go back to start
      $el.insert match[/^.(.*).$/, 1]
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
      $el.insert "\#{#{match}}"
    end

    def self.insert_quote_at_search_start
      match = self.stop
      self.to_start
      $el.insert "'#{match}'"
    end

    def self.isearch_select_inner
      self.stop
      $el.set_mark self.left + 1
      $el.goto_char self.right - 1
      Effects.blink :what=>:region
    end

    def self.isearch_diffs

      was_reverse = self.was_reverse
      match = self.stop

      # Nothing searched for yet, so search difflog...

      if match.nil?
        Location.as_spot
        DiffLog.open
        View.to_bottom
        Search.isearch nil, :reverse=>true
        return
      end

      # Match found, so do dropdown here...

      Launcher.dropdown

    end

    # Used by search+add and search+just+2 etc.
    def self.isearch_clear
      match = self.stop

      if match.nil?   # Nothing searched for yet, so go to spot of last delete
        Location.to_spot('killed')
      else
        View.delete Search.left, Search.right
        Location.as_spot('killed')
      end
    end

    def self.enter txt=nil
      match = self.stop
      txt ||= Clipboard[0]

      if match.nil?   # Nothing searched for yet, so show search history
        return Launcher.open "searching/history/", :bar_is_fine=>1
      end

      View.delete(Search.left, Search.right)
      View.insert txt, :dont_move=>1
    end

    def self.copy_and_comment
      self.stop
      line = Line.value(1, :include_linebreak=>true).sub("\n", "")
      Code.comment Line.left, Line.right
      self.to_start  # Go back to start
      $el.insert "#{line}"
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

      decrement = options[:decrement]

      # If one of certain chars, use custom order
      result =
        if ! decrement && found = SPECIAL_ORDER_PLUS[match]
          found
        elsif decrement && found = SPECIAL_ORDER_MINUS[match]
          found
        elsif position   # Change '[' to ']', etc

          increment_or_decrement = decrement ? -1 : 1
          SPECIAL_ORDER[position+increment_or_decrement].chr

        else
          if decrement
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
      self.to_start
      Location.as_spot

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
      $el.x_select_text(match) if Environment.gui_emacs
    end

    def self.go_to_end
      match = self.stop

      if match.nil?   # If nothing searched for yet
        Location.as_spot
        Search.isearch_restart ":n", :restart=>true
        return
      end

      $el.goto_char self.right
    end

    # Clears the isearch, allowing for inserting, or whatever else
    def self.stop options={}

      txt = self.match

      # TODO > restore or fix!

      # Maybe use (isearch-abort) instead?, and then move cursor to line?
        # might result in weird scrolling

      # Or, try one of these functions?
        # isearch-exit
        # isearch-done
        # isearch-complete1
        # isearch-complete
        # isearch-cancel

      # Make it do special clear if nothing found (to avoid weird isearch error)
      if txt.nil?
        if $el.elvar.isearch_success # || Search.left == View.bottom
          $el.isearch_resume "[^`]", true, nil, true, "", true
          View.message ""
        else
          txt = :not_found
        end
      end

      $el.elvar.isearch_mode = nil

      $el.isearch_clean_overlays
      $el.isearch_done

      # Why doing this? > self.last_search
      txt == :not_found ? self.last_search : txt
    end

    def self.match left=nil, right=nil
      left ||= self.left
      right ||= self.right

      return nil if left == 0# || self.nil?
      result = $el.buffer_substring(left, right)

      return nil if result == ""
      result
    end

    def self.to_start
      View.to($el.elvar.isearch_opoint)
    end

    # Do query replace depending on what they type
    #     def self.query_replace s1=nil, s2=nil
    def self.query_replace s1=nil, s2=nil

      # If no params, prompt for them...

      if ! s1
        s1 = Keys.input :prompt=>"Replace from: "
        s2 = Keys.input :prompt=>"Replace to: "
      else
        s1 = self.quote_elisp_regex s1

      end

      $el.query_replace_regexp s1, s2

      nil
    end

    def self.isearch_query_replace # after=nil
      match = self.stop

      # No match, so prompt for before...

      if ! match
        no_match_existed = true
        match = Keys.input(:prompt=>"Change occurrences of what?: ")
      end

      was_upper = match =~ /[A-Z]/

      match.downcase!
      left, right = Search.left, Search.right
      before = $el.regexp_quote(match)   # Always start with isearch match

      # Prompt for after...

      initial_input = ''
      after = Keys.input(:prompt=>"Change occurrences of '#{before}' to: ", :initial_input=>initial_input)
      @@query_from, @@query_to = before, after

      # We're replacing a match right here, so do it before doing query replace...

      if ! no_match_existed
        View.delete left, right
        View.insert was_upper ?
          TextUtil.title_case(after) :
          after
      end

      $el.query_replace_regexp before, after
      nil
    end

    def self.grep
      cm_grep("./", read_from_minibuffer("Grep for pattern: "))
    end

    def self.tree_grep # prefix=nil

      prefix = Keys.isearch_prefix
      path = Keys.bookmark_as_path :include_file=>1   # Get path (from bookmark)

      return if ! path

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
      lines = $el.buffer_substring(left, right).split "\n"
      ch = $el.char_to_string read_char
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
          $el.insert lines.join("\n") + "\n"
          right = $el.point
          # Go to first file
          $el.goto_char left
        end
        $el.message "Delete lines not matching: %s", pattern
        ch = $el.char_to_string read_char
      end
      # Run whatever they typed last as a command (it was probably C-m or C-a, etc.)
      case ch
      when "\C-m"  # If it was C-m
        # Do nothing
      else
        $el.command_execute ch
      end
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

      match.gsub!(/([#()*+?^$\[\]\/|.])/, "\\\\\\1")

      # Search in bookmark
      FileTree.grep_with_hashes bm, match
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

    def self.launched *arg
      arg = arg.any? ? arg.join("/") : nil

      txt = File.read @@log
      txt = txt.sub(/\A- /, '').split(/^- /).reverse.uniq
      if arg && arg == "#"
        txt = txt.select{|o| o =~ /^  - ##/}
      elsif arg && arg == ":"
        txt = txt.select{|o| o =~ /^    - [^#].*: /}
      elsif arg

        path = Bookmarks[arg]

        if File.file? path   # File
          regex = /^#{Regexp.escape File.dirname path}\/\n  - #{Regexp.escape File.basename path}/
        else   # Dir
          regex = /^#{Regexp.escape path}/
          path = "#{path}/" if path !~ /\/$/
        end

        txt = txt.select{|o| o =~ regex}
      end

      result = "=#{txt.join("=")}"
      result
    end

    def self.left
      $el.match_beginning 0
    end

    def self.right
      $el.match_end 0
    end

    def self.isearch_find_in_buffers options={}
      match = self.stop
      self.find_in_buffers match, options
    end

    def self.find_in_buffers string, options={}
      string.gsub!('"', '\\"')
      new_args = "\"#{string}\""
      new_options = {}
      new_options[:buffer] = View.name if options[:current_only]
      new_args << ", #{new_options.inspect[1..-2]}" unless new_options.empty?

      View.bar if options[:in_bar]
      $el.switch_to_buffer "find in buffers/"
      Notes.mode
      $el.erase_buffer

      View.insert "+ Buffers.search #{new_args}/"
      $el.open_line 1
      CodeTree.launch :no_search=>true
      if new_options[:buffer]   # Goto first match
        $el.goto_line 4
        Line.to_words
        Tree.filter(:recursive=>false, :left=>Line.left, :right=>View.bottom)
      else  # Goto first match in 2nd file
        $el.goto_line 2
        $el.re_search_forward "^  -", nil, true
        Line.next 2
        Line.to_words
      end
    end

    def self.just_marker
      match = self.stop

      $el.highlight_regexp(Regexp.quote(match), :notes_label)
    end

    def self.isearch_highlight_match
      match = self.stop
      View.selection = [self.right, self.left]
    end

    def self.highlight_all_found
      match = self.stop

      $el.highlight_regexp(Regexp.quote(match), :hi_yellow)
    end

    def self.hide
      match = self.stop
      Hide.hide_unless /#{Regexp.quote(match)}/i
      $el.recenter -3
      Hide.search
    end

    # Insert line at beginning of search
    def self.have_line
      self.stop
      column = View.column
      line = Line.value(1, :include_linebreak=>true).sub("\n", "")
      self.to_start  # Go back to start
      $el.insert line
      View.column = column
    end

    # Insert line at beginning of search
    def self.have_label
      self.stop
      label = Line.label
      self.to_start  # Go back to start
      $el.insert "- #{label}: "
    end

    # Insert line at beginning of search
    def self.have_paragraph
      self.stop
      paragraph = View.paragraph
      offset = View.cursor - View.paragraph(:bounds=>true)[0]
      self.to_start  # Go back to start
      orig = Location.new
      $el.insert paragraph
      orig.go
      Move.forward offset
    end


    # During isearch, pull next n words
    def self.isearch_pull_in_words n
      # If on the beginning of a grouping char, move back to catch the sexp
      $el.el4r_lisp_eval "
        (isearch-yank-internal
          (lambda ()
            (forward-word #{n}) (point)))"
    end

    # During isearch, pull next sexp into the search string
    def self.isearch_pull_in_sexp
      # If on the beginning of a grouping char, move back to catch the sexp

      $el.el4r_lisp_eval %q=
        (isearch-yank-internal
          (lambda ()
            (if (and (> (point) 1)
              (string-match "[{<\\\\\"'(\\\\[]" (char-to-string (char-before (point))))
              )
              (backward-char))
            (forward-sexp) (point)))=

    end

    # During isearch, open most recently edited file with the search string in its name
    def self.isearch_tasks
      match = self.stop

      if match.nil?   # If nothing searched for yet
        Location.as_spot
        Search.isearch_restart ":t", :restart=>true

      else

        # search+tree

        dir = Keys.bookmark_as_path :prompt=>"Enter bookmark to look in (or space for recently edited): "

        return View.message("Use space!", :beep=>1) if dir == :comma

        return self.open_file_and_method(match) if dir == :space   # If key is comma, treat as last edited

        TextUtil.snake_case! match if match =~ /[a-z][A-Z]/   # If camel case, file is probably snake
        FileTree.grep_with_hashes dir, match, '**'   # Open buffer and search
      end
    end

    #
    # Jumps to the file corresponding to a string.
    # The string has no path - the edited history (and visited history?)
    # are used to find the file.
    #
    # If the string is like "Foo.bar" it jumps to the method as well ("foo").
    #
    # > Examples
    # Search.open_file_and_method "View"
    # Search.open_file_and_method "View.path"
    #
    def self.open_file_and_method match

      match.sub!(/^[+-] /, '')   # Foo.hi("you")
      match.sub!(/[( ].+/, '')   # Foo.hi

      if match =~ /(.+)[.#](.+)/
        match, method = $1, $2   # split off, and open
      end

      # Convert to snake case, or nil if already in snake case
      snake = TextUtil.snake_case(match)
      snake = nil if snake == match

      match = "#{match}."
      snake = "#{snake}."
      # For each file edited
      found = DiffLog.file_list.find do |o|
        next if ! o.is_a? String

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
          View.to_highest
          result = Search.forward "^ +def \\(self\\.\\)?#{method}[^_a-zA-Z0-9]", :beginning=>true

          return Code.suggest_creating_method View.file, method if !result   # If not found, suggest creating

          Move.to_axis
          $el.recenter 0

          dir, name = found.match(/(.+\/)(.+)/)[1..2]
          Search.append_log dir, "- #{name}\n    : #{Line.value}"

        end
      else
        View.message "'#{match}' not found (no recently edited file with that substring found)."
      end

      return

    end

    def self.isearch_or_copy name
      was_reverse = self.was_reverse
      match = self.stop

      # Isearch for it, or remember what's already searched...

      if match.nil?   # If nothing searched for yet
        target = Clipboard.register(name).downcase
        self.isearch target, :reverse=>was_reverse

        # Make next "C-," repeat just jump to this search string...

        was_reverse ?
          Keys.remember_key_for_repeat(proc {Search.backward target, :quote=>1}, :movement=>1) :
          Keys.remember_key_for_repeat(proc {Search.forward target, :quote=>1, :beginning=>1}, :movement=>1)

      else   # If found something, just remember it
        self.stop
        target = match
        Clipboard.register(name, target)
      end


    end

    def self.isearch_copy_as name
      self.stop
      Clipboard.set(name, self.match)
    end

    def self.kill_filter options={}
      # TODO: Get options[:kill_matching]=>true to delete matching
      # - and map to Keys.just_kill_matching
      Line.start
      left = $el.point
      $el.re_search_forward "^$", nil, 1
      right = $el.point
      $el.goto_char left
      Tree.filter(:left=>left, :right=>right, :recursive=>true)
    end

    def self.cancel
      self.stop
      self.to_start  # Go back to start
      View.message ""
    end

    #   def self.not_found?
    #     # Note this returns false when nothing searched for

    #     ! elvar.isearch_success
    #   end

    # Search.forward "from"
    # Search.forward "from", :dont_move=>1   # Just return location
    # Search.forward "from", :beginning=>1   # Beginning of match
    def self.forward target, options={}
      View.to_highest if options[:from_top]

      orig = View.cursor

      # :line, so make regex to find just the line...

      if options[:line]
        target = "^#{Search.quote_elisp_regex target}$"
      end

      target = self.quote_elisp_regex target if options[:quote]

      # Do actual search

      found = $el.re_search_forward(target, nil, (options[:go_anyway] ? 1 : true))

      View.cursor = orig if options[:dont_move]

      if options[:beginning] && found && View.cursor != View.bottom
        left = self.left
        # Before moving back to left side, if left of match is where we were already, search again
        if orig == self.left
          self.forward target, options
        else
          View.cursor = left
        end

      end

      found
    end

    def self.backward target, options={}
      orig = View.cursor
      found = $el.re_search_backward target, nil, (options[:go_anyway] ? 1 : true)
      View.cursor = orig if options[:dont_move]
      found
    end

    def self.to find
      Move.forward
      if Search.forward(find)
        match = $el.buffer_substring self.left, self.right
        Move.backward(match.size)
      else
        $el.beep
        $el.message "not found"
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
          $el.browse_url(term) :
          $el.browse_url("http://google.com/search?q=#{term}")
        return
      end

      Search.isearch nil, :from_bottom=>true

    end

    def self.search_thesaurus
      term = self.stop

      url = term.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      $el.browse_url "http://thesaurus.reference.com/browse/#{url}"
    end

    def self.isearch_move_line
      $el.isearch_done
      $el.isearch_clean_overlays
      line = $el.buffer_substring $el.point_at_bol, $el.point_at_eol + 1
      View.delete $el.point_at_bol, $el.point_at_eol + 1
      #self.to_start  # Go back to start
      $el.exchange_point_and_mark
      View.insert line, :dont_move=>1
    end

    def self.outline
      if Keys.prefix_u?
        History.open_current :outline=>true, :prompt_for_bookmark=>true
      else
        History.open_current :outline=>true
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
      $el.upcase_region(self.left, self.right)
    end

    def self.downcase
      self.stop
      $el.downcase_region(self.left, self.right)
    end

    def self.enter_like_edits
      Notes.enter_junior
      View << "@edits/"
      Launcher.launch
    end

    # Inserts a "- ##foo/" string into the current view.
    def self.enter_search bm=nil, input=nil

      # If line already has something, assume we'll add - ##foo/ to it
      if ! Line[/^ *$/]

        indent = Line.indent
        Line.to_right

        # up+, so just grab from clipboard...

        if Keys.prefix_u
          input = Clipboard.get
          View.insert "\n#{indent}  - ###{input}/"
          Launcher.launch
          return
        end

        # Insert ##/ under...
        View << "\n#{indent}  - ##/"
        View.to(Line.right - 1)
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
        dir = Bookmarks.expand(":#{bm}")
      end

      View.insert("#{dir}" || "")
      indent = Line.indent
      Line.to_right
      View.insert("\n#{indent}  - ###{input}/")
      Launcher.launch

    end

    def self.isearch_have_outlog options={}

      return self.isearch_have_outlog_javascript if View.extension == "js"

      match = self.stop
      self.to_start
      return View.insert("Ol.line") if match.nil?

      if Tree.construct_path(:all=>1, :slashes=>1) =~ /<script/
        View.insert "console.log('#{match}: ' + #{match});"
        return
      end

      method = options[:method]

      txt = options[:no_label] ?
        "Ol#{method} #{match}" :
        "Ol#{method} #{match.inspect}, #{match}"

      View.insert txt
    end

    def self.isearch_have_outlog_javascript
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
      $el.transpose_chars 1
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

    def self.isearch_just_surround_with_char left=nil, right=nil

      term = self.stop

      right ||= left
      # nothing passed, so request it
      if ! left
        left = Keys.input :chars=>1
        right = left.tr "([{<", ")]}>"
      end

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


    def self.isearch_surround_with_tag

      term = self.stop
      left = Search.left

      tag = Keys.input :timed=>1, :prompt=>"Enter tag to surround: "

      View.to(left + term.length)
      View.insert "</#{tag}>"
      View.to left
      View.insert "<#{tag}>"
      View.to left
    end

    # Copy match as name (like Keys.as_name)
    def self.just_name
      term = self.stop
      loc ||= Keys.input(:chars=>1, :prompt=>"Enter one char (variable name to store this as): ") || "0"
      Clipboard.copy loc, term
      Effects.blink :left=>left, :right=>right
    end

    def self.just_macro
      self.stop
      Macros.run
    end

    def self.to_left
      match = self.stop
      if match.nil?   # C-b and nothing searched for yet
        return Launcher.open "- search/.history/", :bar_is_fine=>1
      end

      Line.to_left
    end

    def self.just_menu
      match = self.stop
      View.open ":ml"
      View.to_bottom
      Search.isearch match, :reverse=>true
    end

    def self.isearch_just_case
      was_reverse = self.was_reverse
      txt = self.stop

      return Search.isearch(Clipboard[0], :reverse=>was_reverse) if txt.nil?

      choice = Keys.input(:prompt=>'convert to which case?: ', :choices=>TextUtil.case_choices)
      View.delete(Search.left, Search.right)
      View.insert choice[txt], :dont_move=>1
    end

    def self.isearch_have_case
      self.stop
      txt = self.match
      lam = Keys.input(:prompt=>'convert to which case?: ', :choices=>TextUtil.case_choices)
      self.to_start  # Go back to start
      $el.insert lam[txt]
    end

    def self.isearch_just_underscores
      self.stop
      term = self.match
      View.delete(Search.left, Search.right)
      View.insert TextUtil.snake_case(term)
    end

    def self.zap
      # try adding this > see if it stops it!
      was_reverse = self.was_reverse
      match = self.stop

      if match.nil?   # If nothing searched for yet
        Launcher.open "ze/", :hotkey=>1
        return
      end

      right = $el.point
      self.to_start   # Go back to search start
      View.delete($el.point, right)
    end

    def self.xiki options={}

      match = self.stop

      # No string searched for, so pretend key was: search+xiki (show xiki search options)...

      if ! match
        return Launcher.open "search xiki/", :hotkey=>1
      end

      # No match, so treat like search+expand

      self.stop
      Launcher.launch

    end

    def self.isearch_restart path, options={}
      term = self.stop
      Location.as_spot if options[:as_here]

      if path == ":t"   # If :t, open bar
        View.layout_todo
      elsif path == ":n"
        View.layout_nav
      elsif path == ":o"
        Code.open_log_view
        options[:reverse] = true
      elsif path == ":d"
        View.open ":d"
        options[:reverse] = true
      elsif path == :top
        # Will go to highest below
      elsif path == :right
        Location.as_spot
        View.layout_right 1
      elsif path == :next
        Location.as_spot
        View.next
      elsif path == :previous
        Location.as_spot
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
      $el.isearch_resume txt, (options[:regex] ?true:nil), nil, (! options[:reverse]), txt, true
      $el.isearch_update
    end

    def self.isearch_forward
      prefix = Keys.prefix :clear=>1
      if prefix == :u
        return $el.isearch_backward
      elsif prefix == :-
        return $el.isearch_forward_regexp
      elsif prefix == :uu
        return $el.isearch_backward_regexp
      end
      $el.isearch_forward
    end

    def self.isearch_stop_at_end
      # Kind of a hack - search for anything, so it won't error when we stop
      $el.isearch_resume "[^`]", true, nil, true, "", true

      self.stop
      self.to_start  # Go back to start

      View.message ""
      nil
    end

    def self.isearch_outline

      match = self.stop
      current_line = Line.number

      if match.nil?   # If nothing searched for yet

        Location.as_spot
        Search.isearch_restart ":o", :restart=>true

      else
        if ! View.file   # If buffer, not file
          buffer_name = $el.buffer_name
          txt = View.txt
          View.to_buffer "* outline of matches in #{buffer_name}"
          Notes.mode
          View.kill_all
          View.insert txt.grep(Regexp.new(match)).join

          return
        end

        dir = View.dir
        file_name = View.file_name
        View.to_buffer "tree filter/";  View.dir = dir
        View.clear;  Notes.mode

        regex = Regexp.quote(match).gsub("/", '\/')

        View.insert "
          - #{dir}/
            - #{file_name}
              - ###{regex}/
          ".unindent

        View.to_line 3

        options = {:current_line=>current_line}
        txt = FileTree.filter_one_file "#{dir}/#{file_name}", /#{Regexp.quote(match)}/i, options
        txt = txt.map{|o| "#{o}\n"}.join("")

        Line.next
        left = View.cursor

        View << "#{txt.gsub(/^/, '      ')}\n"

        right = View.cursor
        View.cursor = left
        Line.next options[:line_found]
        Line.to_beginning

        Tree.filter(:left=>left, :right=>right, :recursive=>true)

      end
    end

    def self.isearch_pull
      was_reverse = self.was_reverse
      match = self.stop
      return Line.previous if match.nil? && was_reverse   # Odd case, user might do this if at end of file

      # Meant to search git diffs, when nothing matched yet
      return Git.search_repository if match.nil?

      self.move_to_search_start match
    end

    def self.isearch_nav
      match = self.stop

      if ! match   # Nothing searched for...
        Location.as_spot
        Search.isearch_restart ":n", :restart=>true
      end

      # Something searched for, so move to the right of it...

      $el.goto_char self.right
    end


    def self.was_reverse
      ! $el.elvar.isearch_forward
    end

    # During search, copy to the clipboard.
    # If no search, does "search+commands" - shows shell commands
    # recently run in a directory.
    def self.isearch_copy
      match = Search.stop

      return Launcher.open("log/") if match.nil?

      self.copy match
      Location.as_spot('clipboard')
    end

    # Search for what's in the clipboard
    def self.isearch_like_clipboard

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

      Search.backward "^ *[+-] ##"
      Move.to_axis
      indent = Line[/^ */]

      View.insert "#{indent}- \#\##{match}/\n", :dont_move=>1
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

    def self.isearch_move_to path, options={}
      match = self.stop
      match = Line.value if match.nil?   # Use line if nothing searched for
      self.move_to path, match, options
    end

    def self.move_to_files match
      match_with_path = FileTree.snippet :txt=>match
      match_with_path = ">\n#{match_with_path}"

      result = self.fit_in_snippet match
      result ? nil : match_with_path
    end

    def self.fit_in_snippet match, options={}

      target_path = options[:path] || View.file
      View.open(":n")

      View.to_highest
      Move.to_junior   # Go to first file

      path = Tree.path.last   # Grab from wiki tree

      # If doesn't fit in the tree, just return to delegate back
      return false if ! target_path

      target_path = Files.tilda_for_home target_path

      return false if ! target_path.start_with?(path)

      cursor = Line.left 2
      FileTree.enter_quote match, :leave_indent=>1
      View.cursor = cursor

      return true   # If handled
    end


    def self.move_to path, match, options={}
      orig = Location.new
      was_in_bar = View.in_bar?

      if path == ":t"   # If :n, grab path also
        View.open(":t")
      elsif path == ":n"   # If :n, grab path also
        match = self.move_to_files match
        return orig.go if ! match   # It handled it if it didn't return the match
      else
        View.open path
      end

      # Maybe extract to .insert_in_section ? ...
      View.to_highest

      line_occupied = ! Line.blank?

      if options[:append]
        Notes.to_block
        Line.previous
        line_occupied = true
      end

      View.insert match

      View.insert "\n" if line_occupied   # Make room if line not blank

      # Add line after if before heading, unless match already had one
      View.insert "\n" if Line[/^>/] && match !~ /\n$/   # If there wasn't a linebreak at the end of the match

      line = View.line

      View.to_highest

      # Which case was this handling?  Being in :n?  Why leave cursor in :t when in :n?
      #     return if path == ":t" && was_in_bar && orig.buffer != "todo.notes"

      orig.go

      if path == ":t" && orig.buffer == "tasks.notes"
        Line.next line-1
        View.column = orig.column
      end
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

      if match.nil?
        # Prompt for bookmark to search in
        self.tree_grep
      else
        self.search_in_bookmark match
      end
    end

    def self.searches
      begin
        $el.elvar.search_ring.to_a
      rescue Exception=>e
        ["error getting searches, probably because of special char :("]
      end
    end

    def self.last_search
      begin
        $el.nth 0, $el.elvar.search_ring.to_a
      rescue Exception=>e
        View.beep
        return "- weird stuff in search ring!"
      end
    end

    # Backs the search/history menu.
    def self.history txt=nil

      # If nothing selected yet, show history

      if ! txt
        searches = self.searches.uniq
        searches = searches.map do |o|
          o =~ /\n/ ?
            ": #{o.inspect}\n" :
            ": #{o}\n"
        end
        return searches.join("")
      end

      # Option selected, so search for it

      txt.sub! /^\: /, ''

      # Go back to where we came from, if we're in special search buffer
      View.kill if View.name == "searching/history/"

      Search.isearch txt
      nil
    end

    # Mapped to up+to+outline
    #
    # Outline that includes path to root.  Like...
    #
    # - /tmp/
    #   - foo.rb
    #     |   def a
    #     |   def b
    #     |     while true
    #     |       c;
    #     |       d;
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
      was_reverse = self.was_reverse
      match = self.stop
      if match   # If there was a match, just stop



        was_reverse ?
          Keys.remember_key_for_repeat(proc {Search.backward match, :quote=>1}, :movement=>1) :
          Keys.remember_key_for_repeat(proc {Search.forward match, :quote=>1, :beginning=>1}, :movement=>1)


        # Remember for C-, first
        return
      end

      Launcher.open(": search+m is available? was > log/", :no_launch=>1) if match.nil?
      #       Launcher.open("log/") if match.nil?
    end

    def self.just_bookmark
      match = self.stop
      path = Keys.bookmark_as_path :include_file=>1   # Get path (from bookmark)
      View.open path
      View.to_highest
      Search.isearch match
    end

    def self.enter_insert_search

      # If in file tree, do ##
      if FileTree.handles?
        Search.enter_search
      else
        self.insert_google
      end
    end

    def self.insert_google

      line = Line.value

      # If not harmless chars, move to next line
      harmless_chars = line[/^[ =+-]*$/]

      if ! harmless_chars && Line.at_right?
        Line << "\n"
        line = Line.value
      end

      return View << "=google/" if harmless_chars.any? && harmless_chars !~ /=$/

      View << "google/"
    end


    def self.like_delete
      match = self.stop

      line = View.line

      View.to_highest
      $el.delete_matching_lines match

      View.line = line

    end

    def self.have_right
      match = self.stop
      View.to_upper
      View.to_highest
      View << "#{match}\n\n"
    end

    # Query replaces from "1" clipboard to "2" clipboard, etc.
    # Search.query_replace_nth "1", "2"
    def self.isearch_just_after
      match = self.stop
      View.delete self.left, self.right
      View << Clipboard.register("2")
    end

    def self.query_replace_with_2

      match = self.stop

      # C-s just typed, so prompt for search strings...

      if ! match
        $el.query_replace_regexp(
          Keys.input(:prompt=>"Replace: "),
          Keys.input(:prompt=>"With: "))
        return
      end

      match.downcase!

      two = Clipboard.register("2")

      left, right = Search.left, Search.right   # Replace this one to get started
      View.delete left, right
      View << two

      self.query_replace match, two
    end

    def self.query_replace_nth n1, n2

      if Keys.up?   # If up+, grab line from last diff
        a, b = DiffLog.last_intraline_diff
        return self.query_replace a, b
      end

      self.query_replace Clipboard.get(n1), Clipboard.get(n2)
    end

    def self.quote_elisp_regex txt
      $el.regexp_quote txt
    end

    def self.isearch_just_special
      match = self.stop
      found = Search.forward "[^\t-~]"   # => 1434703
      View.flash("- no special char found", :times=>3) if ! found
      nil
    end

    def self.search_just_swap
      # Grab and delete what's selected
      txt_a = self.stop
      View.delete(Search.left, Search.right)

      # Insert from clipboard
      View << Clipboard["0"]

      # Go back to start and insert text_a
      self.to_start  # Go back to start
      View << txt_a
    end


    def self.just_kill
      was_reverse = self.was_reverse
      self.stop
      txt = Line.delete

      # Get where we would have gone
      before = $el.elvar.isearch_opoint

      # Move back to account for deleting if reverse
      before -= txt.length+1 if was_reverse

      View.cursor = before
    end

    def self.like_expanded
      match = Search.stop
      Tree.to_parent
      Tree.collapse
      Launcher.launch :no_search=>1
      Search.isearch match
    end

    def self.insert_before_and_after
      txt = Clipboard.register("1").gsub(/^/, ":-").strip+"\n"
      txt << Clipboard.register("2").gsub(/^/, ":+").strip+"\n"
      cursor = View.cursor
      View.<< txt  #, :dont_move=>1
      View.set_mark
      View.cursor = cursor
    end

  end
end
