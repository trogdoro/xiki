module Xiki
  class KeyBindings

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

      Keys.add_menubar_items
    end

    def self.as_keys
      # A: as...
      # Use A prefix for: remembering, saving

      Keys.as_axis { Line.to_left }   # C-a C-a - beginning of line (C-a by default in emacs)
      #     $el.define_key :global_map, $el.kbd("C-a C-a"), :beginning_of_line
      Keys.as_bookmark { Bookmarks.save }   # remember bookmark
      Keys.as_clipboard { Clipboard.as_clipboard }   #
      #     Keys.as_directory { FileTree.copy_path }   # copy dir to clipboard from tree
      Keys.as_delete { Launcher.as_delete }   # copy dir to clipboard from tree
      Keys.as_everything { Clipboard.copy_everything }
      Keys.as_file { DiffLog.save }   # save (or, with prefix, save as)
      Keys.as_history { History.backup_file }   # creates backup
      Keys.as_indented { CodeTree.as_indented }
      Keys.as_job { Macros.record }   # start recording macro
      Keys.as_kill { Clipboard.cut(0); Location.as_spot('killed') }   # cut)
      Keys.as_line { Clipboard.as_line }
      Keys.as_menu { Menu.as_menu }
      Keys.as_nav { Notes.as_nav }
      Keys.as_open { Launcher.as_open }   # copy object / symbol at point
      Keys.as_paragraph { Clipboard.copy_paragraph }   # copy paragraph
      Keys.as_quick { Bookmarks.save :q }   # like AB but uses different temporary namespace
      Keys.as_rest { Clipboard.copy_paragraph(:rest => true) }
      Keys.as_spot { Location.as_spot }   # remember point in file
      Keys.as_todo { Notes.as_todo }
      Keys.as_update { Launcher.as_update }
      # U

      # TODO: make this be as_variable?
      # like:     Keys.as_name { Clipboard.copy }   # copies using key (prompted for)


      Keys.as_variable { Clipboard.copy }   # Copy to variable


      # Think of another key for backing it up?

      Keys.as_window { View.save }   # remember window configuration as name
      Keys.as_you { Clipboard.as_thing }   # copy object / symbol at point

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

      #     Keys.OO { $el.open_line $el.elvar.current_prefix_arg || 1 }   # OO - open line (O's emacs default)
      Keys.open_a_calendar { $el.calendar }
      #Keys.OAD { Svn.jump_to_diff }
      Keys.open_as_file { Code.open_as_file }
      Keys.open_as_elisp { $el.find_function_at_point }   # jump to definition of lisp function
      Keys.open_as_highest { FileTree.open_as_upper }
      Keys.open_as_lowest { FileTree.open_as_upper(:lowest) }
      Keys.open_as_utf { $el.revert_buffer_with_coding_system('utf-8'.to_sym) }
      Keys.open_as_2 { FileTree.open_as_upper(:second) }
      Keys.open_as_root { Files.open_sudo }
      Keys.open_a_shell { Console.open }
      Xiki.def("open+as+tail"){ Files.open_tail }
      Keys.open_bookmark { Bookmarks.go }


      Keys.open_current { Launcher.open("current/") }   # open buffer list


      Keys.open_diffs { DiffLog.open }   # shows diffs of what you've edited
      Keys.open_edited { Files.open_edited }   # show recently edited files
      Keys.open_file { Files.open }
      # G: leave unmapped for escape
      Xiki.def("open+history", "recent files/")
      Keys.open_in_browser { Browser.open_in_browser }
      Keys.open_in_left { View.open_in_bar }
      Keys.open_in_os { Files.open_in_os }
      Keys.open_in_right { View.open_in_right }
      Keys.open_in_window { Files.open_in_window }   # Expose file in OS folder
      Keys.open_just { Files.open_just }   # TODO: When we fix @current/ to show these (try running as-is and see how it uses the old code-tree)
      Keys.open_key { Keys.jump_to_code }   # jump to ruby code of key definition
      Keys.open_list_appointments { View.bar; Launcher.open("- Agenda.menu/") }
      Keys.open_list_bookmarks { Launcher.open("bookmarks/list/") }
      Keys.open_list_clipboard { Launcher.open("clipboard/log/") }
      #     Keys.open_last_error { Code.show_el4r_error }
      Keys.open_list_faces { Styles.list_faces }
      #     Keys.open_list_flashes { Launcher.open "- view/flashes/" }
      Keys.open_lisp_info { $el.info "elisp" }   # Open manual

      Xiki.def "open+list+log", ".@git/log/"   # Show git diffs o 1 file

      Keys.open_last_outlog { Ol.open_last_outlog }   # Show git diffs for a bookmark
      Keys.open_log_push { Git.show_log }   # Show git diffs for a bookmark
      Keys.open_last_screenshot { Files.open_last_screenshot }
      #     Keys.open_log_tree { Rails.tree_from_log }
      Keys.open_list_databases { Launcher.open('- Couch.databases/') }
      #     Keys.open_list_models { Launcher.open("- Merb.models/") }
      Keys.open_list_javascript { View.beep "- Changed to: open+menu+JN!" }
      #     Keys.open_list_names { Clipboard.list }
      Xiki.def("open+list+notes", "notes/list/")

      Keys.open_list_ruby { View.beep "- Changed to: open+menu+RN!" }

      Keys.open_list_technologies { Launcher.open("technologies/") }   # open first hyperlink on page
      Keys.open_last_urls { Launcher.open "last/urls/" }
      Keys.open_menu { Xiki.open_menu }   # Open all menus and show them
      Keys.open_new_file { View.new_file }
      Xiki.def("open+not+saved", "not saved/")
      Keys.open_over { $el.open_line $el.elvar.current_prefix_arg || 1 }   # OO - open line (O's emacs default)
      Keys.open_point { Bookmarks.go(nil, :point => true) }
      Keys.open_quick { Bookmarks.open_quick }   # like OB but uses different temporary namespace
      Keys.open_related_test { Code.open_related_rspec }
      Keys.open_related_file { Code.open_related_file }
      # S
      Keys.open_search { Search.outline_search }   # hide search via outline

      # Bring this back, for opening dir tree when bookmark is to file? - or make that up+open+bookmark?
      #     Keys.open_tree { FileTree.tree }   # draw a tree, prompting for bookmark tag

      Keys.open_up { View.show_dir }   # open enclosing dir
      Keys.open_visualize { Code.do_list_ancestors }   # show currently open files and buffers
      Keys.open_windows { View.restore }   # open window configuration by name
      Keys.open_xiki_docs { Help.display_docs }
      Keys.open_xiki_help { Launcher.open("- Help.menu/") }   #
      # Y
      # Z

      Keys.O1 { Files.open_nth 1 };  Keys.O2 { Files.open_nth 2 };  Keys.O3 { Files.open_nth 3 };  Keys.O4 { Files.open_nth 4 };  Keys.O5 { Files.open_nth 5 }
      Keys.O6 { Files.open_nth 6 };  Keys.O7 { Files.open_nth 7 };  Keys.O8 { Files.open_nth 8 };  Keys.O9 { Files.open_nth 9 }

      Keys.O0 { Files.open_nth 0 }   # Open 0: open line in $f that cursor is on

      Keys.O8 { History.open_current :all => true, :prompt_for_bookmark => true }   # Like do_outline, but inserts all
    end

    def self.enter_keys
      # E: enter...
      # Use E prefix for: inserting

      # TODO find different word?
      #   - Because "enter" can be confused with the enter key?
      #   - ideas: embed, emit, entry
      #     Keys.EE { Line.to_right }   # EE - end of line (E's emacs default)
      Keys.enter_all { Launcher.enter_all }
      Keys.enter_bookmark { FileTree.tree(:here=>true) }

      Keys.enter_clipboard { Clipboard.paste("0") }   # paste

      #     Keys.enter_docs { Line.enter_docs }   # Maybe restore this, but haven't been using it
      # Or, make this be "enter+from+difflog?"
      Keys.enter_diff { DiffLog.enter_from_difflog }
      Keys.enter_end { Line.to_right }   # C-e C-e - end of line (C-e by default in emacs)
      #     $el.define_key :global_map, $el.kbd("C-e C-e"), :end_of_line
      Keys.enter_file_path { Files.enter_file }            # Given a bookmark
      Keys.enter_firefox_tabs { Launcher.insert('browser/tabs/') }   # Given a bookmark
      Keys.enter_history { DiffLog.enter_from_difflog }         # Save point and go to difflog to search
      Keys.enter_insert_date { View.enter_date }
      Keys.enter_insert_comment { Code.enter_insert_comment }      # insert date string (and time if C-u)
      Keys.enter_insert_new { DiffLog.enter_new }           # Enter Old: enter newly-deleted from last save
      Keys.enter_insert_ruby { code = Keys.input(:prompt=>"Enter ruby code to eval and insert results: "); View.insert(eval(code).to_s)}
      Keys.enter_insert_search { Search.enter_insert_search }

      Keys.enter_insert_old { DiffLog.enter_old }   # Enter Old: enter newly-deleted from last save

      Keys.enter_insert_words { PauseMeansSpace.go }

      Keys.enter_junior { Notes.enter_junior }
      Keys.enter_key { Keys.insert_code }
      Keys.enter_log_1 { View << "Ol[1]" }
      Xiki.def("enter+log+check"){ View << "Ol[\"check!\"]\n" }
      Keys.enter_log_ancestors { Code.enter_log_ancestors }
      Keys.enter_list_databases { Launcher.insert('- Couch.databases/') }
      Keys.enter_like_edits { Search.enter_like_edits }
      Keys.enter_log_javascript { Firefox.enter_log_javascript_line }
      Keys.enter_log_stack { Code.enter_log_stack }
      Keys.enter_log_line { Code.enter_log_line }
      Keys.enter_log_out { Code.enter_log_out }
      #     Keys.enter_list_ruby { Launcher.insert("technologies/ruby/") }

      Keys.enter_log_time { Code.enter_log_time }
      Keys.enter_like_url { Firefox.enter_as_url }
      Keys.enter_like_variable { insert "\#{#{Clipboard.get(0)}}" }

      Keys.enter_menu { Xiki.insert_menu }
      Keys.enter_note { Notes.enter_note }
      Xiki.def("enter+note"){ Notes.enter_note }
      Keys.enter_outline { Launcher.enter_outline }   # in tree, enter methods or headings

      #     Keys.enter_push { Git.code_tree_diff(:enter=>true) }   # Commit to repos, push, etc
      Keys.enter_point { Notes.bullet }   # Commit to repos, push, etc
      Keys.enter_quote { FileTree.enter_quote }
      Keys.enter_row { View.insert_line }

      #     Keys.enter_search { Search.enter_search }
      Xiki.def("enter+source"){ Snippet.insert }

      Xiki.def("enter+todo"){ View.enter_upper }
      #     Keys.enter_tree { FileTree.tree(:here=>true) }
      Xiki.def("enter+upper"){ View.beep "- Changed to: enter+todo - make this be enter+under? (like enter+quote, but not a quote)!" }
      Keys.enter_value { Clipboard.paste }
      # W
      Keys.enter_whitespace { Code.enter_whitespace }
      # X
      # Y
      # Z
      Keys.E1 { Clipboard.paste(1) }   # Enter 1
      Keys.E2 { Clipboard.paste(2) }   # Enter 2
      Keys.E3 { Clipboard.paste(3) }
      Keys.E4 { Clipboard.paste(4) };   Keys.E5 { Clipboard.paste(5) };   Keys.E6 { Clipboard.paste(6) }
      Keys.E7 { Clipboard.paste(7) };   Keys.E7 { Clipboard.paste(8) };   Keys.E7 { Clipboard.paste(9) }
    end

    def self.do_keys
      # D: do...
      # Use D prefix for: things that modify text or execute code

      #     Keys.D { insert "Apparently this is necessary to remap C-d" }
      #     Keys.DD { $el.delete_char $el.elvar.current_prefix_arg || 1 }   # DD - delete character (D's emacs default)
      #     Keys.do_as_camelcase { Clipboard.do_as_camel_case }   # change word to camel case (LikeThat)
      Keys.do_as_execute { Console.do_as_execute }   # Run shell command on tree
      Keys.do_as_html { Firefox.do_as_html }
      Keys.do_as_browser { Firefox.exec_block }
      Keys.do_as_javascript { Javascript.run }
      #     Keys.do_as_launched { Launcher.do_as_launched }
      #     Keys.do_as_php { Php.run }
      Keys.do_as_menu { Menu.do_as_menu }   # Grab item after '@' and run it by itself
      Keys.do_as_python { Python.run_block }
      # Do, take numeric prefix for before and after
      Keys.do_add_space { Code.add_space }
      Keys.do_as_test { Code.do_as_rspec }
      Keys.do_as_wrap { Block.do_as_wrap }
      Keys.do_as_quote { Notes.do_as_quote }
      Keys.do_as_xul { Firefox.do_as_xul }
      # B
      Keys.do_backward { View.beep "- Changed to: up+H!" }   # delete word backward
      Keys.do_code_align { Code.do_code_align }   # Based on input character, all matches line up
      Keys.do_click_back { Firefox.back }
      Keys.do_create_directory { FileTree.do_create_dir }
      # Keys.do_click_hyperlink { Firefox.click }   # compare with last AV version

      Keys.do_current_file { Files.delete_current_file }

      Keys.do_compare_history { History.diff_with_backup }   # compare with last AV version

      Keys.do_code_indent { Code.indent }
      Keys.do_count_matches {  View.count_matches }
      Keys.do_copy_name { Clipboard.copy_name }   # Copy file name (without extension and path)
      Keys.do_colors_off { $el.font_lock_mode }   # toggles
      Keys.do_clean_quotes { Files.do_clean_quotes }   # Fix special chars

      Xiki.def("do+compare+repository"){ Git.do_compare_repository }

      Keys.do_compare_saved { DiffLog.compare_with_saved }

      Keys.do_copy_to { FileTree.copy_to }
      Keys.do_compare_views { DiffLog.do_compare_with :u }
      Keys.do_compare_with { DiffLog.do_compare_with }
      Keys.DC1 { Clipboard.diff_1_and_2 }   # Compare contents of clipboards "1" and "2"

      Keys.do_delete { $el.delete_char $el.elvar.current_prefix_arg || 1 }   # DD - delete character (D's emacs default)
      # Probably rename this to not "expand", now that that is kind of the new word for "launch"
      Keys.do_expand { $el.dabbrev_expand nil }   # expand abbreviation / autocomplete
      Keys.do_forward { $el.kill_word(Keys.prefix || 1) }   # delete word forward

      # H
      # G: leave unmapped for escape
      Keys.do_here { Launcher.do_last_launch :here=>1 }   # collapse current menu and run again
      Keys.do_indent { Code.indent_to }
      Keys.do_job { Macros.run }   # do last macro
      Keys.do_kill_all { Effects.blink :what=>:all; View.kill_all }   # kill all text in buffer
      Keys.do_kill_indented { CodeTree.do_kill_indented }  # Delete menu or file or whatever (just passes "0") prefix
      Keys.do_kill_matching { Search.kill_filter }
      Keys.do_kill_nonmatching { Search.kill_filter }
      Keys.do_kill_paragraph { View.kill_paragraph }   # kill all text in buffer
      Keys.do_kill_rest { CodeTree.kill_rest }   # kill adjacent lines at same indent as this one
      Keys.do_kill_siblings { CodeTree.kill_siblings }   # kill adjacent lines at same indent as this one
      Keys.do_kill_trailing { View.gsub!(/[ 	]+$/, "") }   # Deletes trailing whitespace
      Keys.do_list_ancestors { View.beep "- Changed to: do+visibility!" }   # Moved to do+visibility
      Keys.do_load_browser { Browser.reload }
      Keys.do_last_command { Console.do_last_command }
      Keys.do_line_duplicate { Line.duplicate_line }
      Keys.do_load_file { Files.do_load_file }   # U prefix will auto-update / auto-refresh to relflect changes
      Keys.do_lines_having {   # delete lines matching a regex
        unless $el.elvar.current_prefix_arg
          delete_matching_lines( Keys.input(:prompt => "Delete lines having: ") )
        else
          delete_non_matching_lines( Keys.input(:prompt => "Delete lines not having: ") )
        end
      }
      Keys.do_lines_jumble { Code.randomize_lines }   # Shuffle lines
      Keys.do_linebreaks_linux { $el.set_buffer_file_coding_system :unix }
      Keys.do_line_next { Line.move :next }
      Keys.do_line_previous { Line.move(:previous) }
      Keys.do_lines_reverse { $el.reverse_region($el.region_beginning, $el.region_end) }
      Keys.do_lines_sort { Line.do_lines_sort }

      Keys.do_lines_toggle { Line.do_lines_toggle }   # Swap next N lines

      Keys.do_lines_unique { Code.kill_duplicates }   # Uniqify, delete duplicates
      Keys.do_linebreaks_windows { $el.set_buffer_file_coding_system :dos }
      Keys.do_move_to { FileTree.move_to }
      Keys.do_name_buffer { Buffers.rename }
      Keys.do_notes_colors { FileTree.apply_styles; Notes.apply_styles; FileTree.apply_styles_at_end }
      Keys.do_number_enter { Incrementer.enter }
      Keys.do_name_file { FileTree.rename_file }
      Keys.do_number_increment { Incrementer.increment }
      Keys.do_number_one { Incrementer.start }
      Keys.do_next_paragraph { Code.do_next_paragraph }   # Move line to start of next paragraph
      Keys.do_name_search { Search.do_name_search }
      Keys.do_outline { History.open_current :outline=>true, :prompt_for_bookmark=>true }
      #     Keys.do_push { Git.code_tree_diff }   # Commit to repos, push, etc
      Xiki.def("do+push"){ Git.do_push }   # Commit to repos, push, etc
      Keys.do_query { Search.query_replace }   # do query replace
      Keys.do_run { Code.run }   # run code as ruby
      Keys.do_status { Git.do_status }

      Keys.do_tree { FileTree.tree(:recursive=>true) }   # draw filesystem tree for current dir or bookmark
      Keys.do_upper { Launcher.do_last_launch }
      # V
      $el.define_key :global_map, $el.kbd("C-d C-v"), :cua_set_rectangle_mark   # Keys.do_vertical, do+vertical
      Keys.do_whitespace { Deletes.delete_whitespace }   # delete blank lines
      # X
      Keys.do_you { $el.delete_char $el.elvar.current_prefix_arg || 1 }   # Delete character
      Keys.do_zip_next { Files.zip }
      Keys.set("C-d C-.") {   # Do .:  Go to point/bookmark starting with "." and run it (like pressing C-. on that line)
        input = Keys.input(:timed => true)
        $el.with(:save_window_excursion) do
          Bookmarks.go(".#{input}")
          Launcher.launch_unified
        end
      }
      Keys.set("C-d C-/") { Code.comment }

      Keys.D1 { Search.query_replace_nth "1", "2" }
      Keys.D2 { Search.query_replace_nth "2", "1" }
      Keys.D3 { Search.query_replace_nth "3", "4" }
      Keys.D4 { Search.query_replace_nth "4", "3" }

    end

    def self.to_keys
      # T: to...
      # Use T prefix for: moving cursor, jumping to specific points

      $el.el4r_lisp_eval(%Q`(global-set-key (kbd "C-\'") \'repeat)`)

      #     Keys.TT { $el.transpose_chars $el.elvar.current_prefix_arg }   # TT - toggle character (T's emacs default)
      Keys.to_axis { Move.to_axis }   # to beginning of file
      # B
      Keys.to_backward { View.beep "- Changed to: up+back!" }   # move backward one word
      Keys.to_column { Move.to_column }   # to x coordinate - ie column
      # D
      Keys.to_end { Move.to_end }   # To end of line
      # F
      Keys.to_forward { View.beep "- Changed to: up+forward!" }   # move forward one word
      Keys.to_highest { View.to_highest }   # to beginning of file
      Keys.to_indent { Move.to_indent }
      Keys.to_junior { Move.to_junior }
      Keys.to_kind { Move.to_other_bracket }   # to matching bracket, etc
      Keys.to_lowest { View.to_bottom }   # move to end
      Xiki.def("to+menu"){ Menu.to_menu }
      Keys.to_next { Move.to_next_paragraph }   # to next paragraph
      Keys.to_outline { FileTree.to_outline }
      Keys.to_previous { Move.to_previous_paragraph }   # to beginning of previous paragraph
      Keys.to_quote { Move.to_quote }   # move to next ...|... quote
      Keys.to_row { Move.to_line }   # go to nth line, relative to top of window
      Keys.to_spot { Location.to_spot }

      Keys.to_toggle { View.toggle }   # TT - toggle character (T's emacs default)

      Keys.to_up { Tree.to_parent }   # to parent (last line indented less)
      Keys.to_visible { View.to_relative }   # go to nth line, relative to top of window
      Keys.to_words { Line.to_beginning }   # move to start of words on line
      # X
      # Z

      Keys.T0 { View.to_nth_paragraph 0 }
      Keys.T1 { View.to_nth_paragraph 1 }
      Keys.T2 { View.to_nth_paragraph 2 }
      Keys.T3 { View.to_nth_paragraph 3 }
      Keys.T4 { View.to_nth_paragraph 4 }
      Keys.T5 { View.to_nth_paragraph 5 }
      Keys.T6 { View.to_nth_paragraph 6 }
      Keys.T7 { View.to_nth_paragraph 7 }
      Keys.T8 { View.to_nth_paragraph 8 }
      Keys.T9 { View.to_nth_paragraph 9 }

      Keys.set("C-t C-/") { Code.to_comment }

    end

    def self.layout_keys
      # L: layout...
      # Use L prefix for: adjusting the layout, changing what is visible

      #     Keys.LL { View.recenter }   # LL - recenter (L's emacs default)
      Keys.layout_all { View.hide_others }
      Keys.layout_balance { 3.times { View.balance } }   # balance windows
      Keys.layout_create { View.create }   # open new view
      Xiki.def "layout+dimensions", "dimensions/", :letter=>1

      Keys.layout_expand { View.enlarge }
      # F
      Keys.layout_files { View.layout_files }
      Keys.layout_hide { View.hide }   #
      Keys.layout_indent { Hide.hide_by_indent }   # only show lines indented less than x
      Keys.layout_jump { View.shift }
      Keys.layout_kill { View.kill }
      Keys.layout_look { View.recenter }   # LL - recenter (L's emacs default)
      Xiki.def "layout+mark", "mark/", :letter=>1
      Xiki.def("layout+next"){ View.next(:blink=>true) }
      Keys.layout_outlog { View.layout_outlog }
      Keys.layout_previous { View.previous(:blink=>true) }
      # Q
      Keys.layout_right { View.to_upper(:blink=>true) }   # Go to view to the right
      Keys.layout_search { Keys.prefix_u? ? Search.find_in_buffers(Keys.input(:prompt=>"Search all open files for: ")) : Hide.search }
      Keys.layout_todo { View.layout_todo }   # show bar on left with the quick bookmark named "-t"
      Keys.layout_uncover { Hide.reveal }   # Reveal all hidden text
      Xiki.def "layout+visibility", "window/visibility/", :letter=>1
      Keys.layout_wrap { $el.toggle_truncate_lines }   # wrap lines
      # X
      # Y
      Keys.layout_zoom { $el.narrow_to_region($el.region_beginning, $el.region_end) }   # show selection only
      Keys.L0 { View.recenter_top }   # Layout 0: scroll so cursor is 0 lines from top af window
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


      Xiki.def("search+bookmark"){ Search.bookmark }
      # B: leave unmapped for back

      Xiki.def("search+copy"){ Search.isearch_copy }   # Clipboard (copy) or search+commands (if no search)

      Keys.search_delete { Search.isearch_delete }   # Delete
      Keys.search_enter { Search.enter }   # Enter: insert clipboard, replacing match
      Keys.search_frontward { Search.go_to_end }   # Forward
      Keys.search_g { Search.cancel }   # Stop searching
      # have_...
      $el.define_key :isearch_mode_map, $el.kbd("C-h"), nil
      Keys.search_have_after { Search.isearch_move_to "$t", :append=>1 }
      Keys.search_have_bullet { Search.have_label }
      Keys.search_have_case { Search.isearch_have_case }
      Keys.search_have_edges { Search.just_edges }   # Delete everything but chars at edges of match
      Keys.search_have_files { View.beep "- Changed to: search+have+navigation!" }
      Keys.search_have_highest { Search.isearch_restart :top }
      Keys.search_have_javascript { Search.isearch_have_outlog_javascript }
      Keys.search_have_line { Search.have_line }   # copy line back to search start
      Keys.search_have_mock { Search.isearch_have_outlog :method=>".mock" }
      Keys.search_have_nav { Search.isearch_move_to "$f" }

      Keys.search_have_outlog { Search.isearch_have_outlog }
      Keys.search_have_push { Git.search_just_push }   # When search match

      Keys.search_have_right { Search.have_right }
      Keys.search_have_spot { Search.insert_at_spot }
      Keys.search_have_todo { Search.isearch_move_to "$t" }
      Keys.search_have_variable { Search.insert_var_at_search_start }
      Keys.search_have_within { Search.isearch_have_within }   # Grab everything except chars on edges

      # AVAILABLE: search_i ?  (when nothing searched for)

      # I: leave unmapped - had issues using it (messes up position)
      # just_...
      $el.define_key :isearch_mode_map, $el.kbd("C-j"), nil
      Keys.search_just_adjust { Search.isearch_just_adjust }
      Keys.search_just_bookmark { Search.just_bookmark }
      Keys.search_just_case { Search.isearch_just_case }   # make match be camel case
      Keys.search_just_difflog { Search.jump_to_difflog }   # find last string in difflog
      Keys.search_just_edits { Search.just_edits }   # Search in diff of edits to this file
      Keys.search_just_files { Search.isearch_restart "$f" }   # isearch for this string in $f
      Keys.search_just_have { Search.just_select }   # select match
      Keys.search_just_integer { Search.stop; Search.isearch "[0-9][0-9.]*", :regex=>1 }

      Keys.search_just_menu { Search.just_menu }
      #     Keys.search_just_mark { Search.just_marker }
      Keys.search_just_next { Search.isearch_restart :next }
      Keys.search_just_outlog { Search.isearch_restart "$o" }
      Keys.search_just_previous { Search.isearch_restart :previous }
      Keys.search_just_query { Search.isearch_query_replace :match }   # replace
      Keys.search_just_right { Search.isearch_restart :right }   # Search in top-right view
      #     Keys.search_just_search { Search.isearch_just_search }   # Add "##search" line in tree for match
      Keys.search_just_special { Search.isearch_just_special }   # Add "##search" line in tree for match
      Keys.search_just_todo { Search.isearch_restart "$t" }   # isearch for this string in $t

      Keys.search_just_variable { Search.isearch_just_surround_with_char '#{', '}' }
      Keys.search_just_wrap { Ol << 'search_just_wrap';  toggle_truncate_lines }   # make match be snake case

      Keys.search_just_yellow { Search.just_orange }
      Keys.search_kill { Search.cut }   # cut

      $el.define_key :isearch_mode_map, $el.kbd("C-l"), nil

      Keys.search_like_clipboard {
        reverse = Search.was_reverse
        match = Search.stop
        Search.isearch Clipboard[0], :reverse=>reverse
      }   # make match be camel case
      Keys.search_like_delete { Search.like_delete }   # Delete all lines that contain the match
      Keys.search_like_file { Search.isearch_open }
      Keys.search_like_menu { Launcher.search_like_menu }
      Keys.search_like_outlog { Search.isearch_have_outlog :no_label=>1 }
      Keys.search_line_pull { Search.isearch_move_line }
      Keys.search_like_quote { Search.isearch_google :quote=>true }
      Keys.search_like_repository { Git.search_repository }   # When not searching

      Keys.search_like_synonyms { Search.search_thesaurus }
      #     Keys.search_like_timer { Search.search_like_timer }

      #     Keys.search_like_thesaurus { Search.search_thesaurus }
      Keys.search_last_urls { Launcher.open("- Launcher.urls/") }
      Keys.search_like_variable { Search.just_name }
      Keys.search_like_web { Search.isearch_google }   # make match be snake case
      Keys.search_like_xiki { View.open "$x/#{Search.stop.strip}" }

      # Use search_navigated instead
      #     Keys.search_last_launched { Search.search_last_launched }
      #     Keys.search_log { Search.search_log }
      # M: leave unmapped for stop
      Keys.search_next { Search.isearch_next }   # Next, or navigated (if nothing searched for yet)
      Keys.search_outline { Search.isearch_outline }   # Outline
      Keys.search_paths { Search.isearch_paths }   # Just go to previous line
      # P: leave unmapped for previous
      # Q: leave unmapped for quoting
      # R: leave unmapped for reverse
      # S: leave unmapped for search
      Keys.search_to { Search.isearch_to }   # To: open file / jump to method
      Keys.search_usurp { Search.isearch_pull_in_sexp }   # usurp: pull sexp into search string
      Keys.search_value { Search.insert_at_search_start }   # Value: copy value back to search start
      # W: leave unmapped for pulling into search

      Keys.search_xiki { Search.xiki }   # search+xiki+__ mapped inside this method


      # Y: leave unmapped for yank
      Keys.search_zap { Search.zap }   # zap - delete up until search start

      # Surround with characters (quotes and brackets)

      $el.define_key(:isearch_mode_map, $el.kbd("C-'")) { Search.isearch_just_surround_with_char '"' }
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-'")) { Search.isearch_just_surround_with_char "'" }

      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-,")) { Search.isearch_surround_with_tag }

      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-SPC")) { Search.isearch_just_surround_with_char " " }

      $el.define_key(:isearch_mode_map, $el.kbd("C-`")) { Search.isearch_just_surround_with_char "~" }

      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-/")) { Search.isearch_just_comment }
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-=")) { Search.just_increment }   # search+just+Plus
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C--")) { Search.just_increment(:decrement=>true) }   # search+just+Minus

      $el.define_key(:isearch_mode_map, $el.kbd("C-9")) { Search.isearch_just_surround_with_char '(', ')' }
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-9")) { Search.isearch_just_surround_with_char '[', ']'}

      $el.define_key(:isearch_mode_map, $el.kbd("C-h C-[")) { Search.isearch_just_surround_with_char '[', ']' }
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-[")) { Search.isearch_just_surround_with_char '{', '}' }

      $el.define_key(:isearch_mode_map, $el.kbd("C-h C-'")) { Search.insert_quote_at_search_start }

      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-1")) { Search.enter(Clipboard[1]) }   # isearch_just_1
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-2")) { Search.enter(Clipboard[2]) }   # isearch_just_2
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-3")) { Search.enter(Clipboard[3]) }   # isearch_just_3
      $el.define_key(:isearch_mode_map, $el.kbd("C-h C-1")) { Search.isearch_query_replace Clipboard[1] }
      $el.define_key(:isearch_mode_map, $el.kbd("C-h C-2")) { Search.isearch_query_replace Clipboard[2] }


      $el.define_key(:isearch_mode_map, $el.kbd("C-h C-0")) { Search.isearch_query_replace Clipboard[0] }

      $el.define_key(:isearch_mode_map, $el.kbd("C-1")) { Search.isearch_or_copy("1") }
      $el.define_key(:isearch_mode_map, $el.kbd("C-2")) { Search.isearch_or_copy("2") }
      $el.define_key(:isearch_mode_map, $el.kbd("C-3")) { Search.isearch_or_copy("3") }
      $el.define_key(:isearch_mode_map, $el.kbd("C-4")) { Search.isearch_or_copy("4") }
      $el.define_key(:isearch_mode_map, $el.kbd("C-5")) { Search.isearch_or_copy("5") }
      $el.define_key(:isearch_mode_map, $el.kbd("C-6")) { Search.isearch_or_copy("6") }
      $el.define_key(:isearch_mode_map, $el.kbd("C-7")) { Search.isearch_or_copy("7") }
      $el.define_key(:isearch_mode_map, $el.kbd("C-8")) { Search.isearch_or_copy("8") }

      $el.define_key(:isearch_mode_map, $el.kbd("C-=")) { $el.isearch_yank_char }   # Add one char from isearch
      $el.define_key(:isearch_mode_map, $el.kbd("C--")) { Search.subtract }   # Remove one char from isearch
      $el.define_key(:isearch_mode_map, $el.kbd("C-/")) { $el.isearch_delete_char }   # Remove last action from search results
      $el.define_key(:isearch_mode_map, $el.kbd("C-,")) { Search.isearch_query_replace }   # Replace all occurrences

      $el.define_key(:isearch_mode_map, $el.kbd("C-\\")) { Search.hide }   # Hide: hide non-matching

      $el.define_key(:isearch_mode_map, $el.kbd("C-0")) { Search.isearch_pause_or_resume }   # isearch_just_0

      # Safe mapping of C-m to Search.isearch_m (works when el4r is down)
      $el.el4r_lisp_eval(%`(defun isearch-m () (interactive)
        (if (eq (process-status el4r-process) 'run) (el4r-ruby-eval "::Xiki::Search.isearch_m") (isearch-exit)))
        `.unindent)

      $el.define_key :isearch_mode_map, $el.kbd("C-m"), :isearch_m   # search+menu (done in a really safe way, so Return in isearch doesn't break when el4r goes down)

    end

    def self.map_control_return

      # Aquamacs-specific: make cua not map C-return key
      $el.define_key(:cua_global_keymap, $el.kbd("<C-return>"), nil) if $el.boundp(:cua_global_keymap)

      Keys.set("<C-return>") { Launcher.go }   # control-return, control-enter
    end

    def self.map_command_return
      return if ! $el.boundp(:osx_key_mode_map)

      $el.define_key(:osx_key_mode_map, $el.kbd("<A-return>")) { Launcher.go }
    end

    # Not called by default
    def self.map_meta_return
      Keys.set("<M-return>") { Launcher.go }   # command-return, command-enter
    end

    def self.misc

      $el.define_key :global_map, $el.kbd("C-S-v"), :scroll_down

      # Single character definitions
      Keys.B { Move.backward }
      Keys.F { Move.forward }
      Keys.Q { Keys.timed_insert }

      # These keys "launch" things (same thing as double-clicking)
      Keys.set("C-.") { Launcher.go_unified }   # control period
      self.map_command_return
      self.map_control_return

      if $el.locate_library "ruby-mode"
        $el.el_require :ruby_mode
        $el.define_key :ruby_mode_map, $el.kbd("C-\\") do
          Hide.show
          Hide.hide_unless /^ *(def|class|module|create_table|it|describe) /
          $el.recenter -2
          Hide.search
        end
      end
      $el.el_require :cc_mode

      # Unmap keys in modes that interfere
      $el.el4r_lisp_eval("(require 'shell)")
      $el.define_key :shell_mode_map, $el.kbd("C-d"), nil   # shell-mode etc. special C-d shortcuts over-ride xiki
      $el.define_key :objc_mode_map, $el.kbd("C-d"), nil
      $el.define_key :c_mode_map, $el.kbd("C-d"), nil
      $el.el4r_lisp_eval("(require 'dired)")
      $el.define_key :dired_mode_map, $el.kbd("C-o"), nil
      $el.define_key :java_mode_map, $el.kbd("C-d"), nil

      begin
        $el.el_require :php_mode
        $el.define_key :php_mode_map, $el.kbd("C-d"), nil
      rescue Exception=>e
      end

      # C-l in ediff mode
      $el.defun(:ediff_disable_C_l) { $el.define_key(:ediff_mode_map, $el.kbd("C-l"), nil) }
      $el.add_hook :ediff_keymap_setup_hook, :ediff_disable_C_l

      ControlTab.keys

      Keys.set("M-0") { Styles.font_size 120 }
      Keys.set("M-=") { Styles.zoom }
      Keys.set("M--") { Styles.zoom :out=>1 }

      View.sensible_defaults

    end

  end
end
