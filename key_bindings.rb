class KeyBindings
  extend ElMixin

  # Define all keys
  def self.keys
    Menu.init

    self.as_keys
    self.open_keys
    self.enter_keys
    self.to_keys
    self.layout_keys
    self.do_keys
    self.isearch
    self.misc

    Keys.add_menu_items
  end

  def self.as_keys
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
    Keys.as_kill { Clipboard.cut(0); Location.as_spot('killed') }   # cut) **
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
    Keys.as_window { View.save }   # remember window configuration as name
    #     Keys.as_xtract { Clipboard.cut("0") }   # cut **
    # Y
    # Z
    #Keys.A0 { Clipboard.copy("0") }   # As 0: copy as key "0"
    Keys.A1 { Clipboard.copy("1") }   # As 1
    Keys.A2 { Clipboard.copy("2") };  Keys.A3 { Clipboard.copy("3") };  Keys.A4 { Clipboard.copy("4") }
    Keys.A5 { Clipboard.copy("5") };  Keys.A6 { Clipboard.copy("6") };  Keys.A7 { Clipboard.copy("7") }
  end

  def self.open_keys
    # O: open...
    # Use O prefix for: opening, jumping to files

    Keys.OO { open_line elvar.current_prefix_arg || 1 }   # OO - open line (O's default)
    Keys.open_a_calendar { calendar }
    #Keys.OAD { Svn.jump_to_diff }
    Keys.open_as_elisp { find_function_at_point }   # jump to definition of lisp function
    Keys.open_as_highest { FileTree.open_as_upper }
    Keys.open_as_lowest { FileTree.open_as_upper(:lowest) }
    Keys.open_as_utf { revert_buffer_with_coding_system('utf-8'.to_sym) }
    Keys.open_as_2 { FileTree.open_as_upper(:second) }
    Keys.open_as_root { Files.open_sudo }
    Keys.open_a_shell { Console.open }
    Keys.open_as_tail { Files.open_tail }
    Keys.open_bookmark { Bookmarks.go }
    Keys.open_current { CodeTree.display_menu("- Buffers.current/") }   # open buffer list **
    Keys.open_diffs { DiffLog.open }   # shows diffs of what you've edited *
    Keys.open_edited { Files.open_edited }   # show recently edited files *
    Keys.open_file { Files.open }
    # G: leave unmapped for escape
    Keys.open_history { Files.open_history }   # show recently viewed files
    Keys.open_in_bar { View.open_in_bar }
    Keys.open_in_right { View.open_in_right }
    Keys.open_in_os { Files.open_in_os }
    Keys.open_in_window { Files.open_in_window }   # Expose file in OS folder
    Keys.open_just { Files.open_just }
    Keys.open_key { Keys.jump_to_code }   # jump to ruby code of key definition *
    Keys.open_list_bookmarks { CodeTree.display_menu("- Bookmarks.tree/") }
    Keys.open_log_console { Console.log; View.to_bottom; Search.isearch nil, :reverse=>true }
    Keys.open_lisp_error { Code.show_el4r_error }
    Keys.open_list_faces { list_faces_display }
    Keys.open_lisp_info { info("elisp") }   # Open manual
    Keys.open_log_list { Git.show_log_one_file }   # Show git diffs o 1 file
    Keys.open_log_push { Git.show_log }   # Show git diffs for a bookmark
    Keys.open_last_screenshot { Files.open_last_screenshot }
    Keys.open_log_tree { Rails.tree_from_log }
    Keys.open_list_databases { CodeTree.display_menu('- CouchDb.databases/') }
    Keys.open_list_models { CodeTree.display_menu("- Merb.models/") }
    Keys.open_list_names { Clipboard.list }
    Keys.open_list_repository { Git.open_list_repository }
    Keys.open_link_top { Links.open_first }   # open first hyperlink on page
    Keys.open_menu { Xiki.open_menu }   # Open all menus and show them **
    Keys.open_not_saved { History.open_unsaved }
    # O: defined above - mapped to what C-o does by default
    Keys.open_point { Bookmarks.go(nil, :point => true) }
    Keys.open_quick { Bookmarks.go :q }   # like OB but uses different temporary namespace
    Keys.open_rake_outline { CodeTree.display_menu("- Rake.menu/") }
    Keys.open_region_path { find_file buffer_substring(region_beginning, region_end) }
    Keys.open_related_test { Code.open_related_rspec }
    # S
    Keys.open_search { Search.outline_search }   # hide search via outline *
    Keys.open_tree { FileTree.tree }   # draw a tree, prompting for bookmark tag *
    Keys.open_up { View.show_dir }   # open enclosing dir **
    Keys.open_viewing { Buffers.open_viewing }   # show currently open files and buffers **
    Keys.open_windows { View.restore }   # open window configuration by name
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

  def self.enter_keys
    # E: enter...
    # Use E prefix for: inserting

    # TODO find different word?
    #   - Because "enter" can be confused with the enter key?
    #   - ideas: embed, emit, entry
    #     Keys.EAB { Code.enter_as_backslash }   # Enter As Bash: enter with \ at eol's
    Keys.EE { Line.to_right }   # EE - end of line (E's default) **
    Keys.enter_as_added { Numbers.enter_as_added }   # Add dollars or numbers in clipboard
    Keys.enter_as_camelcase { View.insert TextUtil.camel_case(Clipboard.get(0)) }
    #     Keys.enter_as_debug { Code.enter_as_debug }
    Keys.enter_as_execute { Console.do_as_execute(:insert=>true) }   # change word to camel case (LikeThat)
    Keys.enter_as_filename { insert Clipboard.get(".") }
    Keys.enter_as_hyphenated { insert TextUtil.hyphen_case(Clipboard.get(0)) }
    Keys.enter_as_jquery { Javascript.enter_as_jquery }
    Keys.enter_as_snake { insert TextUtil.snake_case(Clipboard.get(0)) }
    #     Keys.enter_as_search { FileTree.enter_as_search }
    Keys.enter_as_test { Specs.enter_as_rspec }
    Keys.enter_as_url { Firefox.enter_as_url }
    Keys.enter_as_variable { insert "\#{#{Clipboard.get(0)}}" }
    Keys.enter_bullet { Notes.bullet }
    Keys.enter_clipboard { Clipboard.paste("0") }   # paste **
    Keys.enter_difflog { App.enter_from_difflog }   # Save point and go to difflog to search
    # E: defined above - mapped to what C-e does by default
    Keys.enter_file { Files.enter_file }   # Given a bookmark
    Keys.enter_history { History.enter_history }   # enter recently viewed files
    #     Keys.enter_have { Console.insert_command }
    #Keys.EH { FileTree.enter_lines(/^\| /) }
    Keys.enter_insert_1 { Notes.enter_do_bullet }    # insert date string (and time if C-u)
    Keys.enter_insert_date { App.enter_date }    # insert date string (and time if C-u)
    Keys.enter_insert_command { insert("- (/): "); ControlLock.disable }    # insert date string (and time if C-u)
    Keys.enter_insert_ruby { code = Keys.input(:prompt=>"Enter ruby code to eval and insert results: "); View.insert(eval(code).to_s)}
    Keys.enter_insert_search {View.insert(Line.matches(/^ *- $/) ? "google: " : "- google: ")}

    Keys.enter_insert_old { DiffLog.enter_old }   # Enter Old: enter newly-deleted from last save
    Keys.enter_insert_new { DiffLog.enter_new }   # Enter Old: enter newly-deleted from last save

    Keys.enter_insert_wikipedia { View.insert("- (wp): ") }
    Keys.enter_in_todo { FileTree.enter_snippet }   # enter tree quote of region in $T
    Keys.enter_junior { Notes.bullet("") }
    Keys.enter_key { Keys.insert_code }
    #Keys.EK { Clipboard.paste }   # Enter Clipboard: paste
    Keys.enter_label_bullet { Notes.enter_label_bullet }
    Keys.enter_log_clipboard { Code.enter_log_clipboard }
    Keys.enter_list_databases { CodeTree.insert_menu('- CouchDb.databases/') }
    Keys.enter_log_javascript { Firefox.enter_log_javascript_line }
    Keys.enter_log_stack { Code.enter_log_stack }
    Keys.enter_log_line { Code.enter_log_line }
    Keys.enter_log_time { Code.enter_log_time }

    Keys.enter_menu { Xiki.insert_menu }   # Redundant with C-enter on blank line
    #     Keys.enter_menu { CodeTree.insert_menus }   # Redundant with C-enter on blank line
    Keys.enter_name { Clipboard.paste }   # paste thing saved as name
    Keys.enter_outline { FileTree.enter_lines }   # in tree, enter methods or headings
    Keys.enter_push { Git.code_tree_diff(:enter=>true) }   # Commit to repos, push, etc
    Keys.enter_quote { FileTree.enter_quote }
    Keys.enter_row { View.insert_line }
    Keys.enter_search { Search.enter_search }
    #Keys.enter_spot { Location.enter_at_spot }   # enter selected text at spot
    Keys.enter_tree { FileTree.tree(:here=>true) }
    Keys.enter_upper { View.enter_upper }
    Keys.enter_viewing { History.enter_viewing }
    # W
    Keys.enter_whitespace { open_line(elvar.current_prefix_arg || 1) }
    Keys.enter_yank { Clipboard.enter_yank }
    # X
    # Y
    # Z
    #Keys.E0 { Clipboard.paste("0") }   # Enter 0: paste from "0" tag
    Keys.E1 { Clipboard.paste(1) }   # Enter 1
    Keys.E2 { Clipboard.paste(2) }   # Enter 2
    Keys.E3 { Clipboard.paste(3) }
    Keys.E4 { Clipboard.paste(4) };   Keys.E5 { Clipboard.paste(5) };   Keys.E6 { Clipboard.paste(6) }
    Keys.E7 { Clipboard.paste(7) };   Keys.E7 { Clipboard.paste(8) };   Keys.E7 { Clipboard.paste(9) }
    Keys.E8 { FileTree.enter_lines /./ }   # Like enter_outline, but inserts all
  end

  def self.do_keys
    # D: do...
    # Use D prefix for: things that modify text or execute code

    Keys.D { insert "Apparently this is necessary to remap C-d" }
    Keys.DD { delete_char elvar.current_prefix_arg || 1 }   # DD - delete character (D's default) **
    Keys.do_as_camelcase { Clipboard.do_as_camel_case }   # change word to camel case (LikeThat)
    Keys.do_as_execute { Console.do_as_execute }   # Run shell command on tree
    Keys.do_as_html { Firefox.do_as_html }
    Keys.do_as_javascript { Javascript.run }
    Keys.do_as_lowercase { Clipboard.do_as_lower_case }   # change word to camel case (LikeThat)
    Keys.do_as_php { Php.run }
    #     Keys.do_as_python { Python.run }
    Keys.do_as_snakecase { Clipboard.do_as_snake_case }   # Change word to snake case (like_that)
    Keys.do_as_test { Code.do_as_rspec }
    Keys.do_as_uppercase { Clipboard.do_as_upper_case }   # change word to camel case (LikeThat)
    #     Keys.do_as_underscores { Clipboard.do_as_snake_case }   # Change word to snake case (like_that)
    Keys.do_as_wrap { Block.do_as_wrap }
    Keys.do_backward { backward_kill_word(Keys.prefix || 1) }   # delete word backward
    Keys.do_code_align { Code.do_code_align }
    Keys.do_click_back { Firefox.back }   # compare with last AV version
    Keys.do_create_directory { FileTree.do_create_dir }
    Keys.do_compare_file { Git.diff_one_file }   # compare current file with subversion
    Keys.do_click_hyperlink { Firefox.click }   # compare with last AV version
    Keys.do_code_indent { Code.indent }
    Keys.do_compare_last { History.diff_with_backup }   # compare with last AV version
    Keys.do_count_matches {  View.count_matches }
    Keys.do_copy_next { Files.copy }   # copy file to next view
    #     Keys.do_compare_one { Git.diff }   # compare one revision with previous revision
    Keys.do_clean_quotes { Files.do_clean_quotes }   # Fix special chars
    Keys.do_compare_repository { Git.diff_dir }
    Keys.do_compare_saved { DiffLog.compare_with_saved }

    Keys.do_copy_to { FileTree.copy_to }

    Keys.do_compare_views { ediff_buffers( window_buffer(nth(0, window_list)), window_buffer(nth(1, window_list))) }   # compare buffers in first two views
    Keys.do_clean_whitespace { View.gsub!(/ +$/, "") }   # Deletes trailing whitespace
    Keys.DC1 { Clipboard.diff_1_and_2 }   # Compare contents of clipboards "1" and "2"
    # D: defined above - mapped to what C-d does by default
    Keys.do_expand { dabbrev_expand nil }   # expand abbreviation
    Keys.do_forward { kill_word(Keys.prefix || 1) }   # delete word forward
    # H
    # G: leave unmapped for escape
    Keys.do_indent { Code.indent_to }
    Keys.do_junior { FileTree.move_dir_to_junior }   # Move a dir to next line, and indent
    Keys.do_kill_all { View.kill_all }   # kill all text in buffer
    Keys.do_kill_file { FileTree.delete_file }
    Keys.do_kill_nonmatching { Search.kill_filter }
    Keys.do_kill_paragraph { View.kill_paragraph }   # kill all text in buffer
    Keys.do_kill_rest { CodeTree.kill_rest }   # kill adjacent lines at same indent as this one
    Keys.do_kill_siblings { CodeTree.kill_siblings }   # kill adjacent lines at same indent as this one
    Keys.do_kill_thing { delete_region(* bounds_of_thing_at_point( :sexp )) }   # kill adjacent lines at same indent as this one
    Keys.do_lines_arbitrary { Code.randomize_lines }   # Shuffle lines
    Keys.do_load_browser { Firefox.reload }
    Keys.do_last_command { Console.do_last_command }
    Keys.do_line_duplicate { Line.duplicate_line }
    #     Keys.do_load_emacs { App.load_emacs }   # *
    Keys.do_load_file { Files.do_load_file }   # U prefix will auto-update / auto-refresh to relflect changes
    Keys.do_lines_having {   # delete lines matching a regex
      unless elvar.current_prefix_arg
        delete_matching_lines( Keys.input(:prompt => "Delete lines having: ") )
      else
        delete_non_matching_lines( Keys.input(:prompt => "Delete lines not having: ") )
      end
    }
    Keys.do_lines_individual { Code.do_kill_duplicates }   # Uniqify, delete duplicates
    Keys.do_last_launch { View.alert("Use instead:  to_up") }
    Keys.do_line_next { Line.move(:next) }
    Keys.do_line_previous { Line.move(:previous) }
    Keys.do_lines_reverse { reverse_region(region_beginning, region_end) }

    Keys.do_lines_sort {
      old = elvar.sort_fold_case# rescue true
      elvar.sort_fold_case = true
      sort_lines(nil, region_beginning, region_end)
      elvar.sort_fold_case = old
    }

    Keys.do_linebreaks_unix { set_buffer_file_coding_system :unix }
    Keys.do_linebreaks_windows { set_buffer_file_coding_system :dos }
    Keys.do_macro { Macros.run }   # do last macro *
    Keys.do_name_buffer { Buffers.rename }
    Keys.do_notes_colors { Notes.apply_styles }
    Keys.do_number_enter { Incrementer.enter }
    Keys.do_name_files { FileTree.rename_file }
    Keys.do_number_increment { Incrementer.increment }
    Keys.do_next_paragraph { Code.do_next_paragraph }   # Move line to start of next paragraph
    Keys.do_number_start { Incrementer.start }
    Keys.do_outline { History.open_current :outline => true, :prompt_for_bookmark => true }
    Keys.do_push { Git.code_tree_diff }   # Commit to repos, push, etc
    Keys.do_query { Search.query_replace }   # do query replace *
    Keys.do_run { Code.run }   # run code as ruby *
    Keys.do_search {
      View.beep
      View.message "changed to search_bookmark" + "!" * 600
    }   # do grep search *
    #     Keys.do_search { Search.tree_grep }   # do grep search *
    #    Keys.DS { elvar.current_prefix_arg ? johns_thing : Search.grep }   # Do Search: do grep search
    Keys.do_tree { FileTree.tree(:recursive=>true) }   # draw filesystem tree for current dir or bookmark
    #     Keys.do_under { FileTree.kill_under }   # kill tree children (lines indented more)
    Keys.do_up { LineLauncher.do_last_launch }
    #Keys.display_up { message FileTree.construct_path( :indented => true ) }   # Display ancestors (by indent level)
    Keys.do_version { Git.code_tree_diff_unadded }   # Compare with repos (with what hasn't been added yet)
    Keys.do_whitespace { Deletes.delete_whitespace }   # delete blank lines
    # X
    Keys.do_you { delete_char elvar.current_prefix_arg || 1 }   # Delete character
    Keys.do_zip_next { Files.zip }
    Keys.set("C-d C-.") {   # Do .:  Go to point/bookmark starting with "." and run it (like pressing C-. on that line)
      input = Keys.input(:timed => true)
      with(:save_window_excursion) do
        Bookmarks.go(".#{input}")
        LineLauncher.launch
      end
    }
    Keys.set("C-d C-/") { Code.comment }

    Keys.D1 { Search.query_replace Clipboard.get("1"), Clipboard.get("2") }
    Keys.D2 { Search.query_replace Clipboard.get("2"), Clipboard.get("1") }
    Keys.D3 { Search.query_replace Clipboard.get("3"), Clipboard.get("4") }
    Keys.D4 { Search.query_replace Clipboard.get("4"), Clipboard.get("3") }

  end

  def self.to_keys
    # T: to...
    # Use T prefix for: moving cursor, jumping to specific points

    el4r_lisp_eval(%Q`(global-set-key (kbd "C-\'") \'repeat)`)

    #     Keys.set("C-'") { $el.repeat 1 }

    Keys.TT { transpose_chars elvar.current_prefix_arg }   # TT - toggle character (T's default)
    Keys.to_axis { Move.to_axis }   # to beginning of file *
    Keys.to_backward { backward_word(Keys.prefix || 1) }   # move backward one word
    Keys.to_column { Move.to_column }   # to x coordinate - ie column
    # D
    Keys.to_end { Move.to_end }   # To end of line **
    Keys.to_forward { forward_word(Keys.prefix || 1) }   # move forward one word
    Keys.to_highest { View.to_highest }   # to beginning of file **
    Keys.to_indent { Move.to_indent }
    Keys.to_junior { Move.to_junior }
    # K
    Keys.to_lowest { View.to_bottom }   # move to end *
    #Keys.to_line { Move.to_line }   # move to line number *
    Keys.to_matching { Move.to_other_bracket }   # to matching bracket, etc
    Keys.to_next { Move.to_next_paragraph }   # to next paragraph *
    Keys.to_outline { FileTree.to_outline }   # *
    #     Keys.to_outline { History.open_current :outline=>true }   # *
    Keys.to_previous { Move.to_previous_paragraph }   # to beginning of previous paragraph *
    Keys.to_quote { Move.to_quote }   # move to next ...|... quote
    Keys.to_row { Move.to_line }   # go to nth line, relative to top of window
    Keys.to_spot { Location.to_spot }   # *
    # T: defined above - mapped to what C-t does by default
    Keys.to_up { FileTree.to_parent }   # to parent (last line indented less)
    Keys.to_visible { View.to_relative }   # go to nth line, relative to top of window
    Keys.to_words { Move.to_line_text_beginning }   # move to start of words on line *
    # X
    #     Keys.to_yank { Clipboard.to_yank }
    # Z
    #Keys.T0 { Location.go("$_0") }   # To 0
    Keys.T1 { View.to_line_with_prefix(1) };  Keys.T2 { View.to_line_with_prefix(2) }
    Keys.T3 { View.to_line_with_prefix(3) };  Keys.T4 { View.to_line_with_prefix(4) }
    Keys.T5 { View.to_line_with_prefix(5) };  Keys.T6 { View.to_line_with_prefix(6) }
    Keys.T7 { View.to_line_with_prefix(7) };  Keys.T8 { View.to_line_with_prefix(8) }
    Keys.T9 { View.to_line_with_prefix(9) };  Keys.T0 { View.to_line_with_prefix }

    Keys.set("C-t C-/") { Code.to_comment }

  end

  def self.layout_keys
    # L: layout...
    # Use L prefix for: adjusting the layout, changing what is visible

    Keys.LL { View.recenter }   # LL - recenter (L's default) *
    Keys.layout_all { View.hide_others }   # *
    Keys.layout_balance { 3.times { View.balance } }   # balance windows *
    Keys.layout_create { View.create }   # open new view **

    Keys.layout_dimensions { View.dimensions }

    Keys.layout_expand { View.enlarge }   # *
    # F
    Keys.layout_files { View.layout_files }
    #     Keys.layout_files { FileTree.open_in_bar; View.to_nth 1; Effects.blink(:what=>:line) }
    Keys.layout_hide { View.hide }   # **
    Keys.layout_indent { Hide.hide_by_indent }   # only show lines indented less than x
    Keys.layout_jump { View.shift }
    Keys.layout_kill { $el.kill_this_buffer }   # **
    # L: defined above - mapped to what C-d does by default
    Keys.layout_marker { Color.colorize }   # colorize line, etc
    #Keys.layout_menu { CodeTree.layout_menu }   # show menu bare in current state
    Keys.layout_next { View.next(:blink=>true) }   # next view **
    Keys.layout_output { View.layout_output }
    Keys.layout_previous { View.previous(:blink=>true) }   # previous view **
    # Q
    Keys.layout_right { View.to_upper(:blink=>true) }   # Go to view to the right
    #     Keys.layout_right { View.layout_right }   # Go to view to the right
    Keys.layout_search { Keys.prefix_u? ? Search.find_in_buffers(Keys.input(:prompt=>"Search all open files for: ")) : Hide.search }   # *
    Keys.layout_todo { View.layout_todo }   # show bar on left with the quick bookmark named "-t" *
    Keys.layout_uncover { Hide.reveal }   # Reveal all hidden text
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
    Keys.L9 { Move.to_last_window(:blink=>true) }

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
    Keys.search_axis { Search.to_left }
    Keys.search_bookmark { Search.bookmark }
    # B: leave unmapped for back
    Keys.search_clipboard { Search.isearch_clipboard }   # Clipboard (copy)
    Keys.search_delete { Search.isearch_delete }   # Delete
    Keys.search_enter { Search.enter }   # Enter: insert clipboard, replacing match
    Keys.search_frontward { Search.go_to_end }   # Forward
    Keys.search_g { Search.cancel }   # Stop searching
    # have_...
    define_key :isearch_mode_map, kbd("C-h"), nil
    Keys.search_have_bullet { Search.have_label }
    Keys.search_have_case { Search.isearch_have_case }
    Keys.search_have_edges { Search.just_edges }   # Delete everything but chars at edges of match
    Keys.search_have_files { Search.isearch_move_to "$f" }
    Keys.search_have_highest { Search.isearch_restart :top }
    Keys.search_have_javascript { Search.isearch_log_javascript }
    Keys.search_have_line { Search.have_line }   # copy line back to search start
    Keys.search_have_name { Search.just_name }
    Keys.search_have_output { Search.isearch_log }
    Keys.search_have_push { Git.search_just_push }

    #     Keys.search_have_rspec { Specs.insert_in_todo }
    Keys.search_have_spot { Search.insert_at_spot }
    Keys.search_have_move { Search.isearch_move_line }
    Keys.search_have_todo { Search.isearch_move_to "$t" }
    Keys.search_have_variable { Search.insert_var_at_search_start }
    Keys.search_have_within { Search.isearch_have_within }   # Grab everything except chars on edges
    # I: leave unmapped - had issues using it (messes up position)
    # just_...
    define_key :isearch_mode_map, kbd("C-j"), nil
    Keys.search_just_adjust { Search.isearch_just_adjust }
    Keys.search_just_bold { Search.isearch_just_surround_with_char('<b>', '</b>') }
    Keys.search_just_case { Search.isearch_just_case }   # make match be camel case
    Keys.search_just_difflog { Search.jump_to_difflog }   # find last string in difflog
    Keys.search_just_edits { Search.just_edits }   # Search in diff of edits to this file
    Keys.search_just_files { Search.isearch_restart "$f" }   # isearch for this string in $f
    Keys.search_just_have { Search.just_select }   # select match
    Keys.search_just_mark { Search.just_marker }
    #     Keys.search_just_macro { Search.just_macro }
    Keys.search_just_next { Search.isearch_restart :next }
    Keys.search_just_output { Search.isearch_restart "$o" }
    Keys.search_just_previous { Search.isearch_restart :previous }
    Keys.search_just_query { Search.isearch_query_replace :match }   # replace
    Keys.search_just_right { Search.isearch_restart :right }   # Search in top-right view
    Keys.search_just_search { Search.isearch_just_search }   # Add "##search" line in tree for match
    Keys.search_just_todo { Search.isearch_restart "$t" }   # isearch for this string in $t

    Keys.search_just_variable { Search.isearch_just_surround_with_char '#{', '}' }

    Keys.search_just_web { Search.isearch_google }   # make match be snake case
    Keys.search_just_yellow { Search.just_orange }
    Keys.search_kill { Search.cut }   # cut

    define_key :isearch_mode_map, kbd("C-l"), nil
    Keys.search_like_file { Search.isearch_open }   # Log: search in search log
    Keys.search_like_timer { Search.search_like_timer }   # Log: search in search log
    Keys.search_like_log { Search.search_log }   # Log: search in search log

    #     Keys.search_log { Search.search_log }   # Log: search in search log
    # M: leave unmapped for stop
    Keys.search_next { Search.isearch_next_or_name }   # Next, or name (if nothing searched for yet)
    Keys.search_outline { Search.isearch_outline }   # Outline
    Keys.search_previous { Search.isearch_previous }   # Just go to previous line
    # P: leave unmapped for previous
    # Q: leave unmapped for quoting
    # R: leave unmapped for reverse
    # S: leave unmapped for search
    Keys.search_to { Search.isearch_to }   # To: open file / jump to method
    Keys.search_usurp { Search.isearch_pull_in_sexp }   # usurp: pull sexp into search string
    Keys.search_value { Search.insert_at_search_start }   # Value: copy value back to search start
    # W: leave unmapped for pulling into search
    Keys.search_xtract {
      View.beep
      View.message "use 'pull'" + "!" * 600
    }   # Xtract: move back to search start
    # Y: leave unmapped for yank
    Keys.search_zap { Search.zap }   # zap - delete up until search start

    # Surround with characters (quotes and brackets)

    define_key(:isearch_mode_map, kbd("C-'")) { Search.isearch_just_surround_with_char '"' }
    define_key(:isearch_mode_map, kbd("C-j C-'")) { Search.isearch_just_surround_with_char "'" }

    define_key(:isearch_mode_map, kbd("C-j C-,")) { Search.isearch_just_surround_with_char "~" }

    define_key(:isearch_mode_map, kbd("C-j C-SPC")) { Search.isearch_just_surround_with_char " " }

    define_key(:isearch_mode_map, kbd("C-`")) { Search.isearch_just_surround_with_char "~" }

    define_key(:isearch_mode_map, kbd("C-j C-/")) { Search.isearch_just_comment }
    define_key(:isearch_mode_map, kbd("C-j C-=")) { Search.just_increment }
    define_key(:isearch_mode_map, kbd("C-j C--")) { Search.just_increment(:decrement=>true) }

    define_key(:isearch_mode_map, kbd("C-9")) { Search.isearch_just_surround_with_char '(', ')' }
    define_key(:isearch_mode_map, kbd("C-j C-9")) { Search.isearch_just_surround_with_char '[', ']'}

    define_key(:isearch_mode_map, kbd("C-h C-[")) { Search.isearch_just_surround_with_char '[', ']' }
    define_key(:isearch_mode_map, kbd("C-j C-[")) { Search.isearch_just_surround_with_char '{', '}' }

    define_key(:isearch_mode_map, kbd("C-h C-'")) { Search.insert_quote_at_search_start }

    define_key(:isearch_mode_map, kbd("C-j C-1")) { Search.enter(Clipboard[1]) }   # isearch_just_1
    define_key(:isearch_mode_map, kbd("C-j C-2")) { Search.enter(Clipboard[2]) }   # isearch_just_2
    define_key(:isearch_mode_map, kbd("C-j C-3")) { Search.enter(Clipboard[3]) }   # isearch_just_3
    define_key(:isearch_mode_map, kbd("C-h C-1")) { Search.isearch_query_replace Clipboard[1] }
    define_key(:isearch_mode_map, kbd("C-h C-2")) { Search.isearch_query_replace Clipboard[2] }


    define_key(:isearch_mode_map, kbd("C-h C-0")) { Search.isearch_query_replace Clipboard[0] }

    define_key(:isearch_mode_map, kbd("C-1")) { Search.isearch_or_copy("1") }
    define_key(:isearch_mode_map, kbd("C-2")) { Search.isearch_or_copy("2") }
    define_key(:isearch_mode_map, kbd("C-3")) { Search.isearch_or_copy("3") }
    define_key(:isearch_mode_map, kbd("C-4")) { Search.isearch_or_copy("4") }
    define_key(:isearch_mode_map, kbd("C-5")) { Search.isearch_or_copy("5") }
    define_key(:isearch_mode_map, kbd("C-6")) { Search.isearch_or_copy("6") }
    define_key(:isearch_mode_map, kbd("C-7")) { Search.isearch_or_copy("7") }
    define_key(:isearch_mode_map, kbd("C-8")) { Search.isearch_or_copy("8") }
    #     define_key(:isearch_mode_map, kbd("C-9")) { Search.isearch_or_copy("9") }

    define_key(:isearch_mode_map, kbd("C-=")) { $el.isearch_yank_char }   # Add one char from isearch
    define_key(:isearch_mode_map, kbd("C--")) { $el.isearch_del_char }   # Remove one char from isearch
    define_key(:isearch_mode_map, kbd("C-/")) { $el.isearch_delete_char }   # Remove last action from search results
    define_key(:isearch_mode_map, kbd("C-,")) { Search.isearch_query_replace }   # Replace all occurrences

    define_key(:isearch_mode_map, kbd("C-\\")) { Search.hide }   # Hide: hide non-matching

    define_key(:isearch_mode_map, kbd("C-0")) { Search.isearch_pause_or_resume }   # isearch_just_0
    #     define_key(:isearch_mode_map, kbd("C-8")) { Search.isearch_query_replace Clipboard[0] }   # isearch_just_0

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
    define_key :shell_mode_map, kbd("C-d"), nil   # shell-mode etc. special C-d shortcuts over-ride xiki
    define_key :objc_mode_map, kbd("C-d"), nil
    define_key :c_mode_map, kbd("C-d"), nil
    #     el4r_lisp_eval("(require 'php)")
    el4r_lisp_eval("(require 'dired)")
    define_key :dired_mode_map, kbd("C-o"), nil
    define_key :java_mode_map, kbd("C-d"), nil
    el_require :php_mode
    define_key :php_mode_map, kbd("C-d"), nil

    # C-l in ediff mode
    defun(:ediff_disable_C_l) { define_key(:ediff_mode_map, kbd("C-l"), nil) }
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
