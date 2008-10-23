class KeyBindings
  extend ElMixin

  # Define all keys
  def self.keys
    Menu.init

    self.a_keys
    self.o_keys
    self.e_keys
    self.t_keys
    self.l_keys
    self.d_keys
    self.isearch
    self.isearch_meta
    self.misc

    Keys.add_menu_items

  end

  def self.a_keys
    # A: as...
    # Use A prefix for: remembering, saving

    Keys.AA { Line.to_left }   # AA - beginning of line (A's default) **
    Keys.as_bookmark { Bookmarks.save }   # remember bookmark
    Keys.as_clipboard { Clipboard.as_clipboard }   # **
    Keys.as_directory { FileTree.copy_path }   # copy dir to clipboard from tree
    # D
    Keys.as_everything { Clipboard.copy_everything }
    Keys.as_file { DiffLog.save }   # save (or, with prefix, save as) **
    # H
    #Keys.as_indented { Clipboard.as_indented }
    # J
    Keys.as_kill { Clipboard.cut("0"); Location.as_spot('deleted') }   # cut) **
    Keys.as_line { Clipboard.as_line }
    Keys.as_macro { Macros.record }   # start recording macro *
    Keys.as_name { Clipboard.copy }   # copies using key (prompted for)
    Keys.as_object { Clipboard.as_object }   # copy object / symbol at point
    Keys.as_paragraph { Clipboard.copy_paragraph }   # copy paragraph
    Keys.as_quick { Bookmarks.save :q }   # like AB but uses different temporary namespace
    Keys.as_rest { Clipboard.copy_paragraph(:rest => true) }
    Keys.as_spot { Location.as_spot }   # remember point in file *
    Keys.as_thing { Clipboard.as_thing }  # copy sexp at point
    #Keys.as_update_remote { FileTree.save_remote }
    Keys.as_version { History.backup_file }   # creates backup
    Keys.as_windows { View.save }   # remember window configuration as tag
    Keys.as_xtract { Clipboard.cut("0") }   # cut **
    # Y
    # Z
    #Keys.A0 { Clipboard.copy("0") }   # As 0: copy as key "0"
    Keys.A1 { Clipboard.copy("1") }   # As 1
    Keys.A2 { Clipboard.copy("2") };  # As 2
    Keys.A3 { Clipboard.copy("3") };  Keys.A4 { Clipboard.copy("4") }
  end

  def self.o_keys
    # O: open...
    # Use O prefix for: opening, jumping to files

    Keys.OO { open_line elvar.current_prefix_arg || 1 }   # OO - open line (O's default)
    Keys.open_a_calendar { calendar }
    #Keys.OAD { Svn.jump_to_diff }
    Keys.open_as_lisp { find_function_at_point }   # jump to definition of lisp function
    #Keys.OAS { Search.google }   # Open As Search
    Keys.open_a_shell { Console.open }
    Keys.open_as_tail { Files.open_tail }
    Keys.open_bookmark { Bookmarks.go }
    Keys.open_current { CodeTree.display_menu("- Buffers.current/") }   # open buffer list **
    Keys.open_difflog { DiffLog.open }   # shows diffs of what you've edited *
    Keys.open_edited { Files.open_edited }   # show recently edited files *
    Keys.open_file { Files.open }
    Keys.open_as_root { Files.open_sudo }
    # G: leave unmapped for escape
    Keys.open_history { Files.open_history }   # show recently viewed files
    Keys.open_in_bar { View.open_in_bar }
    Keys.open_in_right { View.open_in_right }
    Keys.open_in_os { Files.open_in_os }
    Keys.open_just { Files.open_just }
    Keys.open_list_bookmarks { CodeTree.display_menu("- Bookmarks.tree/") }
    Keys.open_list_faces { list_faces_display }
    Keys.open_lisp_error { Code.show_el4r_error }
    Keys.open_lisp_info { info("elisp") }
    Keys.open_log_tree { Rails.tree_from_log }
    Keys.open_list_databases { CodeTree.display_menu('- CouchDb.databases/') }
    Keys.open_list_models { CodeTree.display_menu("- Merb.models/") }
    Keys.open_list_names { Clipboard.list }
    Keys.open_list_repository { Repository.open_list_repository }
    Keys.open_link_top { Links.open_first }   # open first hyperlink on page
    Keys.open_menu { CodeTree.open_menu }   # Open all menus and show them **
    Keys.open_next_error { Merb.open_next_error }
    Keys.open_not_saved { History.open_unsaved }
    # O: defined above - mapped to what C-o does by default
    Keys.open_point { Bookmarks.go(nil, :point => true) }
    Keys.open_quick { Bookmarks.go :q }   # like OB but uses different temporary namespace
    Keys.open_region_path { find_file buffer_substring(region_beginning, region_end) }
    Keys.open_rake_outline { CodeTree.display_menu("- Rake.menu/") }
    Keys.open_search { Search.outline_search }   # hide search via outline *
    Keys.open_tree { FileTree.tree }   # draw a tree, prompting for bookmark tag *
    Keys.open_up { View.show_dir }   # open enclosing dir **
    Keys.open_viewing { Buffers.open_viewing }   # show currently open files and buffers **
    Keys.open_windows { View.restore }   # open window configuration by tag
    Keys.open_xiki_docs { Help.display_docs }
    Keys.open_xiki_help { CodeTree.display_menu("- Help.menu/") }   # **
    # Y
    # Z
    Keys.O0 { View.open("$0") }   # Open 0: open bookmarked file tagged with "0"
    Keys.O1 { View.open("$1") }   # Open 1
    Keys.O2 { View.open("$2") };   Keys.O3 { View.open("$3") };   Keys.O4 { View.open("$4") };
    Keys.O5 { View.open("$5") };   Keys.O6 { View.open("$6") };   Keys.O7 { View.open("$7") };
    Keys.O8 { History.open_current :all => true, :prompt_for_bookmark => true }   # Like do_outline, but inserts all
  end

  def self.e_keys
    # E: enter...
    # Use E prefix for: inserting

    # TODO find different word?
    #   - Because "enter" can be confused with the enter key?
    #   - ideas: embed, emit, entry
    #     Keys.EAB { Code.enter_as_backslash }   # Enter As Bash: enter with \ at eol's
    Keys.EE { Line.to_right }   # EE - end of line (E's default) **
    Keys.enter_as_camelcase { insert TextUtil.camel_case(Clipboard.get(0)) }
    Keys.enter_as_debug { Code.enter_as_debug }
    Keys.enter_as_filename { insert Clipboard.get(".") }
    Keys.enter_as_trunk { Code.enter_as_trunk }
    Keys.enter_as_underscores { View.insert TextUtil.snake_case(Clipboard.get(0)) }
    Keys.enter_as_variable { insert "\#{#{Clipboard.get(0)}}" }
    Keys.enter_bullet { Notes.bullet }
    Keys.enter_clipboard { Clipboard.paste("0") }   # paste **
    Keys.enter_difflog { App.enter_from_difflog }   # Save point and go to difflog to search
    # E: defined above - mapped to what C-e does by default
    Keys.enter_file { View.insert(Keys.bookmark_as_path(:include_file=>true)); Line.to_left }   # Given a bookmark
    Keys.enter_history { History.enter_history }   # enter recently viewed files
    #Keys.EH { FileTree.enter_lines(/^\| /) }
    Keys.enter_insert_date { App.enter_date }    # insert date string (and time if C-u)
    Keys.enter_insert_command { insert("- (/): "); ControlLock.disable }    # insert date string (and time if C-u)
    Keys.enter_insert_search { View.insert("- google: ") }
    Keys.enter_in_todo { FileTree.enter_snippet }   # enter tree quote of region in $T
    Keys.enter_junior { Notes.bullet("") }
    Keys.enter_key { Keys.insert_code }
    #Keys.EK { Clipboard.paste }   # Enter Clipboard: paste
    Keys.enter_label_bullet { Notes.enter_label_bullet }
    Keys.enter_log_clipboard { Code.enter_log_clipboard }
    Keys.enter_list_databases { CodeTree.insert_menu('- CouchDb.databases/') }
    Keys.enter_log_javascript { Code.enter_log_console }
    Keys.enter_list_models { CodeTree.insert_menu("- Merb.models") }
    Keys.enter_log_statement { Code.enter_log }
    Keys.enter_log_line { Code.enter_log_line }
    Keys.enter_log_time { Code.enter_log_time }
    Keys.enter_menu { CodeTree.insert_menus }
    Keys.enter_name { Clipboard.paste }   # paste thing saved as name
    Keys.enter_outline { FileTree.enter_lines }   # in tree, enter methods or headings
    # Find new key for thisKeys.EO { DiffLog.enter_old }   # Enter Old: enter newly-deleted from last save
    Keys.enter_push { Repository.code_tree_diff(:enter=>true) }   # Commit to repos, push, etc
    Keys.enter_quoted { FileTree.enter_quoted }
    #Keys.enter_replacement { Clipboard.enter_replacement }
    Keys.enter_search { Search.enter_search }
    #Keys.enter_spot { Location.enter_at_spot }   # enter selected text at spot
    Keys.enter_tree { FileTree.tree(:here=>true) }
    Keys.enter_under { LineLauncher.launch(:blink=>true) }
    Keys.enter_viewing { History.enter_viewing }
    # W
    # X
    # Y
    # Z
    #Keys.E0 { Clipboard.paste("0") }   # Enter 0: paste from "0" tag
    Keys.E1 { Clipboard.paste("1") }   # Enter 1
    Keys.E2 { Clipboard.paste("2") };  # Enter 2
    Keys.E3 { Clipboard.paste("3") };   Keys.E4 { Clipboard.paste("4") }
    Keys.E5 { Clipboard.paste("5") };   Keys.E6 { Clipboard.paste("6") };   Keys.E7 { Clipboard.paste("7") }
    Keys.E8 { FileTree.enter_lines "." }   # Like enter_outline, but inserts all
  end

  def self.d_keys
    # D: do...
    # Use D prefix for: things that modify text or execute code

    Keys.D { insert "Apparently this is necessary to remap C-d" }
    Keys.DD { delete_char elvar.current_prefix_arg || 1 }   # DD - delete character (D's default) **
    Keys.do_as_camelcase { Clipboard.do_as_camel_case }   # change word to camel case (LikeThat)
    #Keys.DAL { Code.load_this_file }   # Do As Load: do a ruby load on the file
    Keys.do_as_javascript { Javascript.run }
    Keys.do_as_rspec { Code.do_as_rspec }
    Keys.do_as_underscores { Clipboard.do_as_snake_case }   # Change word to snake case (like_that)
    Keys.do_as_wrap { Block.do_as_wrap }
    Keys.do_backward { backward_kill_word(Keys.prefix || 1) }   # delete word backward
    Keys.do_code_align { Code.do_code_align }
    Keys.do_code_comment { Code.comment }
    Keys.do_create_directory { FileTree.create_dir }
    Keys.do_compare_file { Repository.diff_one_file }   # compare current file with subversion
    Keys.do_code_indent { Code.indent }
    Keys.do_compare_last { History.diff_with_backup }   # compare with last AV version
    Keys.do_count_matches {  View.count_matches }
    Keys.do_copy_next { Files.copy }   # copy file to next view
    Keys.do_compare_one { Repository.diff }   # compare one revision with previous revision
    Keys.do_compare_repository { Repository.diff_dir }
    Keys.do_compare_saved { DiffLog.compare_with_saved }
    Keys.do_clean_trailing {
      with(:save_excursion) do
        beginning_of_buffer;  replace_string(char_to_string(13), "")
        beginning_of_buffer;  replace_regexp(/ +$/, "")
      end
    }
    Keys.do_compare_views { ediff_buffers( window_buffer(nth(0, window_list)), window_buffer(nth(1, window_list))) }   # compare buffers in first two views
    Keys.DC1 { Clipboard.diff_1_and_2 }
    # D: defined above - mapped to what C-d does by default
    Keys.do_expand { dabbrev_expand nil }   # expand abbreviation
    Keys.do_forword { kill_word(Keys.prefix || 1) }   # delete word forward
    # H
    # G: leave unmapped for escape
    Keys.do_indent { Code.indent_to }
    Keys.do_junior { FileTree.move_dir_to_junior }   # Move a dir to next line, and indent
    #Keys.DK1 { KeyBindings.keys }   # Do Keys 1: load key bindings "1" (currently this file)
    Keys.do_kill_all { View.kill_all }   # kill all text in buffer
    Keys.do_kill_duplicates { Code.do_kill_duplicates }
    Keys.do_kill_filter { Search.kill_filter }
    Keys.do_kill_siblings { CodeTree.kill_siblings }   # kill adjacent lines at same indent as this one
    Keys.do_kill_thing { delete_region(* bounds_of_thing_at_point( :sexp )) }   # kill adjacent lines at same indent as this one
    Keys.do_load_browser { Firefox.reload }
    Keys.do_last_command { Console.do_last_command }
    Keys.do_linebreaks_dos { set_buffer_file_coding_system :dos }
    #     Keys.do_load_emacs { App.load_emacs }   # *
    Keys.do_load_file { revert_buffer(true, true, true) }
    Keys.do_lines_having {   # delete lines matching a regex
      unless elvar.current_prefix_arg
        delete_matching_lines( Keys.input(:prompt => "Delete lines having: ") )
      else
        delete_non_matching_lines( Keys.input(:prompt => "Delete lines not having: ") )
      end
    }
    Keys.do_lines_reverse { reverse_region(region_beginning, region_end) }
    Keys.do_lines_sort { sort_lines(nil, region_beginning, region_end) }
    Keys.do_linebreaks_unix { set_buffer_file_coding_system :unix }
    Keys.do_macro { Macros.run }   # do last macro *
    Keys.do_name_buffer { Buffers.rename }
    Keys.do_name_files { wdired_change_to_wdired_mode }
    Keys.do_next_paragraph { Code.do_next_paragraph }   # Move line to start of next paragraph
    Keys.do_outline { History.open_current :outline => true, :prompt_for_bookmark => true }
    Keys.do_push { Repository.code_tree_diff }   # Commit to repos, push, etc
    Keys.do_query { Search.query_replace }   # do query replace *
    Keys.do_ruby { Code.run }   # run code as ruby *
    Keys.do_search { Search.tree_grep }   # do grep search *
    #    Keys.DS { elvar.current_prefix_arg ? johns_thing : Search.grep }   # Do Search: do grep search
    Keys.do_tree { FileTree.tree(:recursive=>true) }   # draw filesystem tree for current dir or bookmark
    Keys.do_under { FileTree.kill_under }   # kill tree children (lines indented more)
    Keys.do_version { Repository.code_tree_diff_unadded }   # kill tree children (lines indented more)
    #Keys.display_up { message FileTree.construct_path( :indented => true ) }   # display path to root of file
    # V
    Keys.do_whitespace { Deletes.delete_whitespace }   # delete blank lines
    # X
    # Z
    Keys.do_zip_next { Files.zip }
    Keys.set("C-d C-.") {   # Do .:  Go to point/bookmark starting with "." and run it (like pressing C-. on that line)
      input = Keys.input(:timed => true)
      with(:save_window_excursion) do
        Bookmarks.go(".#{input}")
        LineLauncher.launch
      end
    }
    Keys.D1 { delete_char 1 };  Keys.D2 { delete_char 2 };  Keys.D3 { delete_char 3 };  Keys.D4 { delete_char 4 }
    Keys.D5 { delete_char 5 };  Keys.D6 { delete_char 6 };  Keys.D7 { delete_char 7 };  Keys.D8 { delete_char 7 };
  end

  def self.t_keys
    # T: to...
    # Use T prefix for: moving cursor, jumping to specific points

    Keys.TT { transpose_chars elvar.current_prefix_arg }   # TT - toggle character (T's default)
    Keys.to_apex { View.beep; View.to_top; View.message("#{'-'*10} Please use Keys.to_highest #{'-'*10}") }   # to beginning of file **
    Keys.to_backward { backward_word(Keys.prefix || 1) }   # move backward one word
    Keys.to_clipboard { Search.to Clipboard[0] }   # move cursor to next instance of clipboard
    Keys.to_deleted { Location.to_spot('deleted') }   # **
    Keys.to_end { View.to_bottom }   # **
    Keys.to_forward { forward_word(Keys.prefix || 1) }   # move forward one word
    Keys.to_highest { View.to_top }   # to beginning of file **
    Keys.to_indent { Move.to_indent }
    Keys.to_junior { Move.to_junior }
    Keys.to_key { Keys.jump_to_code }   # jump to ruby code of key definition *
    Keys.to_line { Move.to_line }   # move to line number *
    Keys.to_matching { Move.to_other_bracket }   # to matching bracket, etc
    Keys.to_next { Move.to_next_paragraph }   # to next paragraph *
    Keys.to_outline { History.open_current :outline => true }   # *
    Keys.to_previous { Move.to_previous_paragraph }   # to beginning of previous paragraph *
    Keys.to_quote { Move.to_quote }   # move to next ...|... quote
    Keys.to_relative { Search.to_relative }   # go to nth line, relative to top of window
    Keys.to_spot { Location.to_spot }   # *
    # T: defined above - mapped to what C-t does by default
    Keys.to_up { FileTree.to_parent }   # to parent (last line indented less)
    Keys.to_words { Move.to_line_text_beginning }   # move to start of words on line *
    Keys.to_x { Move.to_column }   # to x coordinate - ie column
    # Y
    # Z
    #Keys.T0 { Location.go("$_0") }   # To 0
    Keys.T1 { Search.to Clipboard["1"] }
    Keys.T2 { Search.to Clipboard["2"] }
    Keys.T3 { Search.to Clipboard["3"] }
    Keys.T4 { Search.to Clipboard["4"] }
  end

  def self.l_keys
    # L: layout...
    # Use L prefix for: adjusting the layout, changing what is visible

    Keys.LL { recenter(elvar.current_prefix_arg) }   # LL - recenter (L's default) *
    Keys.layout_all { View.hide_others }   # *
    Keys.layout_balance { View.balance }   # balance windows *
    Keys.layout_create { View.create }   # open new view **
    Keys.layout_dimensions_large { set_frame_size(View.frame, 145, 58);  set_frame_position(View.frame, 46, 22) }
    Keys.layout_dimensions_medium { set_frame_size(View.frame, 145, 50);  set_frame_position(View.frame, 223, 22) }
    Keys.layout_dimensions_small { set_frame_size(View.frame, 80, 28) }
    Keys.layout_expand { View.enlarge }   # *
    # F
    Keys.layout_files { FileTree.open_in_bar; View.to_nth 1; Effects.blink(:what=>:line) }
    Keys.layout_hide { View.hide }   # **
    Keys.layout_indent { Hide.hide_by_indent }   # only show lines indented less than x
    # J
    Keys.layout_kill { kill_this_buffer }   # **
    # L: defined above - mapped to what C-d does by default
    Keys.layout_mark { Color.colorize }   # colorize line, etc
    #Keys.layout_menu { CodeTree.layout_menu }   # show menu bare in current state
    Keys.layout_next { View.next }   # next view **
    Keys.layout_output { Code.open_log_view; Effects.blink(:what=>:line) }
    Keys.layout_previous { View.previous }   # previous view **
    # Q
    Keys.layout_reveal { Hide.reveal }   # reveal all hidden text
    Keys.layout_search { Keys.prefix_u? ? Search.find_in_buffers(Keys.input(:prompt=>"Search all open files for: ")) : Hide.search }   # *
    Keys.layout_todo { FileTree.open_in_bar; Effects.blink(:what=>:line) }   # show bar on left with the quick bookmark named "-t" *
    Keys.layout_upper { View.to_upper }   # go to uppermost view after bar
    # V
    Keys.layout_visibility { View.visibility }
    Keys.layout_wrap { toggle_truncate_lines }   # wrap lines **
    # X
    # Y
    Keys.layout_zoom { narrow_to_region(region_beginning, region_end) }   # show selection only
    Keys.L0 { View.recenter_top }   # Layout 0: scroll so cursor is 0 lines from top af window *
    Keys.L1 { Move.to_window(1, :blink=>true) }   # Layout 1
    Keys.L2 { Move.to_window(2, :blink=>true) }   # Layout 2
    Keys.L3 { Move.to_window(3, :blink=>true) };  Keys.L4 { Move.to_window(4, :blink=>true) }
    Keys.L5 { Move.to_window(5, :blink=>true) };  Keys.L6 { Move.to_window(6, :blink=>true) };  Keys.L7 { Move.to_window(7, :blink=>true) };  Keys.L8 { Move.to_window(8, :blink=>true) }
    Keys.L9 { Move.to_window(9, :blink=>true) }

    # Todo: if prefix passed, expand window, but leave other windows open with that much space in each
    #    Keys.LCR { Colors.highlight  }   # Layout Tree: show bar on left with the quick bookmark named "-t"

    # Todo:
    # - Make it recognize :n when opening
    #   - If you put it at end of path or as tree node, it should make it into path
    #     - So look at it when opening path and optionally jump
    #  - narrow block to region:
  end

  # Control keys during isearch
  def self.isearch
    Keys.isearch_ahead { Search.to_left }
    # B: leave unmapped for back
    Keys.isearch_clipboard { Search.copy }   # Clipboard (copy)
    Keys.isearch_delete { Search.isearch_delete }   # Delete
    Keys.isearch_enter { Search.paste_here }   # Enter: insert clipboard, replacing match
    Keys.isearch_forward { Search.go_to_end }   # Forward
    Keys.isearch_g { Search.stop }   # Stop searching
    # have_...
    define_key :isearch_mode_map, kbd("C-h"), nil
    Keys.isearch_have_camel { Search.isearch_as_camel }
    Keys.isearch_have_delete { Search.move_to_search_start }
    Keys.isearch_have_variable { Search.insert_var_at_search_start }
    Keys.isearch_have_line { Search.have_line }   # copy line back to search start
    Keys.isearch_have_output { Search.isearch_log }   # copy line back to search start
    Keys.isearch_have_paragraph { Search.have_paragraph }   # copy line back to search start
    Keys.isearch_have_spot { Search.insert_at_spot }   # Zap: move to spot (as spot)
    Keys.isearch_have_underscores { Search.isearch_as_snake }
    # I: leave unmapped - had issues using it (messes up position)
    # just_...
    define_key :isearch_mode_map, kbd("C-j"), nil
    Keys.isearch_just_difflog { Search.jump_to_difflog }   # find last string in difflog
    Keys.isearch_just_edges { Search.just_edges }   # delete everything but chars at edges of match
    Keys.isearch_just_lowercase { Search.downcase }
    Keys.isearch_just_name { Search.just_name }
    Keys.isearch_just_orange { Search.just_orange }
    Keys.isearch_just_replace { Search.isearch_query_replace }   # replace
    Keys.isearch_just_select { Search.just_select }   # select match
    Keys.isearch_just_uppercase { Search.upcase }
    #Keys.isearch_just_underline { Search.clear; Overlay.face(:underline, :left=>Search.left, :right=>Search.right) }
    Keys.isearch_kill { Search.cut; Location.as_spot('deleted') }   # cut
    Keys.isearch_look { Search.uncover }   # Look: show results for search string in all open files
    # M: leave unmapped for stop
    # N: leave unmapped for next
    Keys.isearch_outline { Search.isearch_find_in_buffers(:current_only => true) }   # Outline
    # P: leave unmapped for previous
    # Q: leave unmapped for quoting
    # R: leave unmapped for reverse
    # S: leave unmapped for search
    Keys.isearch_to { Search.isearch_open_last_edited }   # To: open file / jump to method
    Keys.isearch_usurp { Search.isearch_pull_in_sexp }   # usurp: pull sexp into search string
    Keys.isearch_value { Search.insert_at_search_start }   # Value: copy value back to search start
    # W: leave unmapped for pulling into search
    Keys.isearch_xtract { Search.move_to_search_start }   # eXtract: move back to search start
    # Y: leave unmapped for yank
    # Z: zap?

    define_key :isearch_mode_map, kbd("C-1") do
      Search.isearch_copy_as("1")
    end
    define_key :isearch_mode_map, kbd("C-2") do
      Search.isearch_copy_as("2")
    end
    define_key :isearch_mode_map, kbd("C-3") do
      Search.isearch_copy_as("3")
    end
    define_key :isearch_mode_map, kbd("C-4") do
      Search.isearch_copy_as("4")
    end

    define_key :isearch_mode_map, kbd("C-=") do   # Add one char from isearch
      $el.isearch_yank_char
    end
    define_key :isearch_mode_map, kbd("C--") do   # Remove one char from isearch
      $el.isearch_del_char
    end
    define_key :isearch_mode_map, kbd("C-/") do   # Remove last action from search results
      $el.isearch_delete_char
    end

    define_key :isearch_mode_map, kbd("C-\\") do   # Hide: hide non-matching
      Search.hide
    end

  end

  # Meta keys during isearch
  def self.isearch_meta

    Keys._A(:isearch_mode_map) { Search.isearch_query_replace :start_with_search_string }   # Alter: query-replace, using search string as initial input
    Keys._C(:isearch_mode_map) { Search.copy_and_comment }   # Comment line and copy it to starting point
    Keys._D(:isearch_mode_map) { Search.downcase }   # Downcase
    Keys._E(:isearch_mode_map) { Search.insert_tree_at_spot }   # Enter
    Keys._F(:isearch_mode_map) { Search.isearch_open }   # Find file
    Keys._G(:isearch_mode_map) { Search.isearch_google }   # Google search
    # H
    #Keys._I(:isearch_mode_map) { Search.insert_var_at_search_start }   # Interpolate: paste as interpolated variable
    # J
    # K
    #Keys._L(:isearch_mode_map) { Search.isearch_log }
    Keys._M(:isearch_mode_map) { Search.isearch_tree_grep_method }   # Method: do tree grep (prompt for dir)
    # N
    Keys._O(:isearch_mode_map) { Search.isearch_find_in_buffers(:current_only => true, :in_bar => true) }   # Outline: in side bar
    # P
    # Q
    # R
    # S
    Keys._U(:isearch_mode_map) { Search.upcase }   # Upcase
    Keys._V(:isearch_mode_map) { Search.isearch_find_in_buffers(:in_bar => true) }   # Visited: show matches in visited files
    # V
    Keys._W(:isearch_mode_map) { Search.isearch_select_inner }   # Within: select 1 char within match
    Keys._X(:isearch_mode_map) { Search.isearch_move_line }
    # Y
    # Z

    define_key :isearch_mode_map, kbd("M-1") do   # pull in 1 word
      $el.isearch_yank_char
      #Search.isearch_pull_in_words 1
    end
    define_key :isearch_mode_map, kbd("M-2") do   # pull in 2 word
      Search.isearch_pull_in_words 2
    end
    define_key :isearch_mode_map, kbd("M-3") do   # pull in 3 word
      Search.isearch_pull_in_words 3
    end
    define_key :isearch_mode_map, kbd("M-4") do   # pull in 4 word
      Search.isearch_pull_in_words 4
    end
    define_key :isearch_mode_map, kbd("M-5") do   # pull in 5 word
      Search.isearch_pull_in_words 5
    end

    define_key :isearch_mode_map, kbd("M-0") do   # Recenter
      Search.clear
      recenter 0
    end
  end

  def self.misc
    # Control-Shift combinations
    Keys.set("C-S-c") { Clipboard.copy("0") }
    Keys.set("C-S-e") { Clipboard.paste("0") }

    # Single character definitions
    Keys.B { Move.backward }
    Keys.F { Move.forward }
    Keys.Q { Keys.timed_insert }
    Keys.set("C-.") { LineLauncher.launch_or_hide(:blink=>true) }

    # Alternate key for C-. (probably easier to remember)
    Keys.set("<C-return>") { LineLauncher.launch_or_hide(:blink=>true) }

    if locate_library "ruby-mode"
      el_require :ruby_mode
      define_key :ruby_mode_map, kbd("C-\\") do
        Hide.show
        Hide.hide_unless /^ *(def|class|module|create_table|it|describe) /
        recenter -2
        Hide.search
      end
    end
    el_require :cc_mode

    # Unmap keys in modes that interfere
    el4r_lisp_eval("(require 'shell)")
    define_key :shell_mode_map, kbd("C-d"), nil
    el4r_lisp_eval("(require 'dired)")
    define_key :dired_mode_map, kbd("C-o"), nil
    define_key :java_mode_map, kbd("C-d"), nil

    # C-l in ediff mode
    defun(:ediff_disable_C_l) do
      define_key :ediff_mode_map, kbd("C-l"), nil
    end
    add_hook :ediff_keymap_setup_hook, :ediff_disable_C_l

    ControlTab.keys

    Keys.set("C-e C-\\") {
      txt, left, right = Clipboard.copy_paragraph(:just_return => true)
      View.delete left, right
      txt.gsub! /$/, ' \\'
      txt.sub! " \\\n \\", "\n"
      View.insert txt
    }

    View.sensible_defaults
  end

end
