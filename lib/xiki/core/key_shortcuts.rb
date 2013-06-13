module Xiki
  class KeyShortcuts

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

      Xiki.def("as+axis"){ Line.to_left }   # C-a C-a - beginning of line (C-a by default in emacs)
      #     $el.define_key :global_map, $el.kbd("C-a C-a"), :beginning_of_line
      Xiki.def("as+bookmark"){ Bookmarks.save }   # remember bookmark
      Xiki.def("as+clipboard"){ Clipboard.as_clipboard }   #
      #     Xiki.def("as+directory"){ FileTree.copy_path }   # copy dir to clipboard from tree
      Xiki.def("as+delete") { Launcher.as_delete }   # copy dir to clipboard from tree
      Xiki.def("as+everything"){ Clipboard.copy_everything }
      Xiki.def("as+file"){ DiffLog.save }   # save (or, with prefix, save as)
      Xiki.def("as+history"){ History.backup_file }   # creates backup
      Xiki.def("as+indented"){ CodeTree.as_indented }
      Xiki.def("as+job"){ Macros.record }   # start recording macro
      Xiki.def("as+kill"){ Clipboard.cut(0); Location.as_spot('killed') }   # cut)
      Xiki.def("as+line"){ Clipboard.as_line }
      Xiki.def("as+menu"){ Menu.as_menu }
      Xiki.def("as+nav"){ Notes.as_nav }
      Xiki.def("as+open"){ Launcher.as_open }   # copy object / symbol at point
      Xiki.def("as+paragraph"){ Clipboard.copy_paragraph }   # copy paragraph
      Xiki.def("as+quick"){ Bookmarks.save :q }   # like AB but uses different temporary namespace
      Xiki.def("as+rest"){ Clipboard.copy_paragraph(:rest => true) }
      Xiki.def("as+spot"){ Location.as_spot }   # remember point in file
      Xiki.def("as+todo"){ Notes.as_todo }
      Xiki.def("as+update"){ Launcher.as_update }
      # U

      # TODO: make this be as_variable?
      # like:     Keys.as_name { Clipboard.copy }   # copies using key (prompted for)


      Xiki.def("as+variable"){ Clipboard.copy }   # Copy to variable


      # Think of another key for backing it up?

      Xiki.def("as+window"){ View.save }   # remember window configuration as name
      Xiki.def("as+you"){ Clipboard.as_thing }   # copy object / symbol at point

      # Y
      # Z
      #Keys.A0 { Clipboard.copy("0") }   # As 0: copy as key "0"
      Xiki.def("as+1"){ Clipboard.copy("1") }   # As 1
      Xiki.def("as+2"){ Clipboard.copy("2") };  Xiki.def("as+3"){ Clipboard.copy("3") };  Xiki.def("as+4"){ Clipboard.copy("4") }
      Xiki.def("as+5"){ Clipboard.copy("5") };  Xiki.def("as+6"){ Clipboard.copy("6") };  Xiki.def("as+7"){ Clipboard.copy("7") }
    end

    def self.open_keys
      # O: open...
      # Use O prefix for: opening, jumping to files

      #     Keys.OO { $el.open_line $el.elvar.current_prefix_arg || 1 }   # OO - open line (O's emacs default)
      Xiki.def("open+a+calendar"){ $el.calendar }
      #Xiki.def("OAD"){ Svn.jump_to_diff }
      Xiki.def("open+as+file"){ Code.open_as_file }
      Xiki.def("open+as+elisp"){ $el.find_function_at_point }   # jump to definition of lisp function
      Xiki.def("open+as+highest"){ FileTree.open_as_upper }
      Xiki.def("open+as+lowest"){ FileTree.open_as_upper(:lowest) }
      Xiki.def("open+as+utf"){ $el.revert_buffer_with_coding_system('utf-8'.to_sym) }
      Xiki.def("open+as+2"){ FileTree.open_as_upper(:second) }
      Xiki.def("open+as+root"){ Files.open_sudo }
      Xiki.def("open+a+shell"){ Console.open }
      Xiki.def("open+as+tail"){ Files.open_tail }
      Xiki.def("open+bookmark"){ Bookmarks.go }


      Xiki.def("open+current"){ Launcher.open("current/") }   # open buffer list


      Xiki.def("open+diffs"){ DiffLog.open }   # shows diffs of what you've edited
      Xiki.def("open+edited"){ Files.open_edited }   # show recently edited files
      Xiki.def("open+file"){ Files.open }
      # G: leave unmapped for escape
      Xiki.def("open+history", "recent files/")
      Xiki.def("open+in+browser"){ Browser.open_in_browser }
      Xiki.def("open+in+left"){ View.open_in_bar }
      Xiki.def("open+in+os"){ Files.open_in_os }
      Xiki.def("open+in+right"){ View.open_in_right }
      Xiki.def("open+in+window"){ Files.open_in_window }   # Expose file in OS folder
      Xiki.def("open+just"){ Files.open_just }   # TODO: When we fix @current/ to show these (try running as-is and see how it uses the old code-tree)
      Xiki.def("open+key"){ Keys.jump_to_code }   # jump to ruby code of key definition
      Xiki.def("open+list+appointments"){ View.bar; Launcher.open("- Agenda.menu/") }
      Xiki.def("open+list+bookmarks"){ Launcher.open("bookmarks/list/") }
      Xiki.def("open+list+clipboard"){ Launcher.open("clipboard/log/") }
      #     Xiki.def("open+last+error"){ Code.show_el4r_error }
      Xiki.def("open+list+faces"){ Styles.list_faces }
      #     Xiki.def("open+list+flashes"){ Launcher.open "- view/flashes/" }
      Xiki.def("open+lisp+info"){ $el.info "elisp" }   # Open manual

      Xiki.def "open+list+log", ".@git/log/"   # Show git diffs o 1 file

      Xiki.def("open+last+outlog"){ OlHelper.open_last_outlog }   # Show git diffs for a bookmark
      Xiki.def("open+log+push"){ Git.show_log }   # Show git diffs for a bookmark
      Xiki.def("open+last+screenshot"){ Files.open_last_screenshot }
      #     Xiki.def("open+log+tree"){ Rails.tree_from_log }
      Xiki.def("open+list+databases"){ Launcher.open('- Couch.databases/') }
      #     Xiki.def("open+list+models"){ Launcher.open("- Merb.models/") }
      Xiki.def("open+list+javascript"){ View.beep "- Changed to: open+menu+JN!" }
      #     Xiki.def("open+list+names"){ Clipboard.list }
      Xiki.def("open+list+notes", "notes/list/")

      Xiki.def("open+list+ruby"){ View.beep "- Changed to: open+menu+RN!" }

      Xiki.def("open+list+technologies"){ Launcher.open("technologies/") }   # open first hyperlink on page
      Xiki.def("open+last+urls"){ Launcher.open "last/urls/" }
      Xiki.def("open+menu"){ Launcher.open_menu }   # Open all menus and show them
      #       Xiki.def("open+new+file"){ View.new_file }
      Xiki.def("open+note"){ Notes.open_note }
      Xiki.def("open+over"){ $el.open_line $el.elvar.current_prefix_arg || 1 }   # OO - open line (O's emacs default)
      Xiki.def("open+point"){ Bookmarks.go(nil, :point => true) }
      Xiki.def("open+quick"){ Bookmarks.open_quick }   # like OB but uses different temporary namespace
      Xiki.def("open+related+test"){ Code.open_related_rspec }
      Xiki.def("open+related+file"){ Code.open_related_file }
      # S
      Xiki.def("open+search"){ Search.outline_search }   # hide search via outline

      # Bring this back, for opening dir tree when bookmark is to file? - or make that up+open+bookmark?
      #     Xiki.def("open+tree"){ FileTree.tree }   # draw a tree, prompting for bookmark tag

      Xiki.def("open+up"){ View.show_dir }   # open enclosing dir
      Xiki.def("open+visualize"){ Code.do_list_ancestors }   # show currently open files and buffers
      Xiki.def("open+windows"){ View.restore }   # open window configuration by name
      Xiki.def("open+xiki+docs"){ Help.display_docs }
      Xiki.def("open+xiki+help"){ Launcher.open("- Help.menu/") }   #
      # Y
      # Z

      Xiki.def("open+1"){ Files.open_nth 1 };  Xiki.def("open+2"){ Files.open_nth 2 };  Xiki.def("open+3"){ Files.open_nth 3 };  Xiki.def("open+4"){ Files.open_nth 4 };  Xiki.def("open+5"){ Files.open_nth 5 }
      Xiki.def("open+6"){ Files.open_nth 6 };  Xiki.def("open+7"){ Files.open_nth 7 };  Xiki.def("open+8"){ Files.open_nth 8 };  Xiki.def("open+9"){ Files.open_nth 9 }

      Xiki.def("open+0"){ Files.open_nth 0 }   # Open 0: open line in $f that cursor is on

      Xiki.def("open+8"){ History.open_current :all => true, :prompt_for_bookmark => true }   # Like do_outline, but inserts all
    end

    def self.enter_keys
      # E: enter...
      # Use E prefix for: inserting

      # TODO find different word?
      #   - Because "enter" can be confused with the enter key?
      #   - ideas: embed, emit, entry
      #     Keys.EE { Line.to_right }   # EE - end of line (E's emacs default)
      Xiki.def("enter+all"){ Launcher.enter_all }
      Xiki.def("enter+bookmark"){ FileTree.tree(:here=>true) }

      Xiki.def("enter+clipboard"){ Clipboard.paste("0") }   # paste

      #     Xiki.def("enter+docs"){ Line.enter_docs }   # Maybe restore this, but haven't been using it
      # Or, make this be "enter+from+difflog?"
      Xiki.def("enter+diff"){ DiffLog.enter_from_difflog }
      Xiki.def("enter+end"){ Line.to_right }   # C-e C-e - end of line (C-e by default in emacs)
      #     $el.define_key :global_map, $el.kbd("C-e C-e"), :end_of_line
      Xiki.def("enter+file+path"){ Files.enter_file }            # Given a bookmark
      Xiki.def("enter+firefox+tabs"){ Launcher.insert('browser/tabs/') }   # Given a bookmark
      # H
      Xiki.def("enter+insert+date"){ View.enter_date }
      Xiki.def("enter+insert+comment"){ Code.enter_insert_comment }      # insert date string (and time if C-u)
      Xiki.def("enter+insert+new"){ DiffLog.enter_new }           # Enter Old: enter newly-deleted from last save
      Xiki.def("enter+insert+ruby"){ code = Keys.input(:prompt=>"Enter ruby code to eval and insert results: "); View.insert(eval(code).to_s)}
      Xiki.def("enter+insert+search"){ Search.enter_insert_search }

      Xiki.def("enter+insert+old"){ DiffLog.enter_old }   # Enter Old: enter newly-deleted from last save

      Xiki.def("enter+insert+words"){ PauseMeansSpace.go }

      Xiki.def("enter+junior"){ Notes.enter_junior }
      Xiki.def("enter+key"){ Keys.insert_code }
      Xiki.def("enter+log+1"){ View << "Ol[1]" }
      Xiki.def("enter+log+check"){ View << "Ol[\"check!\"]\n" }
      Xiki.def("enter+log+ancestors"){ Code.enter_log_ancestors }
      Xiki.def("enter+list+databases"){ Launcher.insert('- Couch.databases/') }
      Xiki.def("enter+like+edits"){ Search.enter_like_edits }
      Xiki.def("enter+log+javascript"){ Firefox.enter_log_javascript_line }
      Xiki.def("enter+log+stack"){ Code.enter_log_stack }
      Xiki.def("enter+log+line"){ Code.enter_log_line }
      Xiki.def("enter+log+out"){ Code.enter_log_out }
      #     Xiki.def("enter+list+ruby"){ Launcher.insert("technologies/ruby/") }

      Xiki.def("enter+log+time"){ Code.enter_log_time }
      Xiki.def("enter+like+url"){ Firefox.enter_as_url }
      Xiki.def("enter+like+variable"){ insert "\#{#{Clipboard.get(0)}}" }

      Xiki.def("enter+menu"){ Launcher.insert_menu }
      Xiki.def("enter+note"){ Notes.enter_note }
      Xiki.def("enter+note"){ Notes.enter_note }
      Xiki.def("enter+outline"){ Launcher.enter_outline }   # in tree, enter methods or headings

      #     Xiki.def("enter+push"){ Git.code_tree_diff(:enter=>true) }   # Commit to repos, push, etc
      Xiki.def("enter+point"){ Notes.bullet }   # Commit to repos, push, etc
      Xiki.def("enter+quote"){ FileTree.enter_quote }
      Xiki.def("enter+row"){ View.insert_line }

      #     Xiki.def("enter+search"){ Search.enter_search }
      Xiki.def("enter+source"){ Snippet.insert }

      Xiki.def("enter+todo"){ View.enter_upper }
      #     Xiki.def("enter+tree"){ FileTree.tree(:here=>true) }
      Xiki.def("enter+upper"){ View.beep "- Changed to: enter+todo - make this be enter+under? (like enter+quote, but not a quote)!" }
      Xiki.def("enter+value"){ Clipboard.paste }
      # W
      Xiki.def("enter+whitespace"){ Code.enter_whitespace }
      # X
      # Y
      # Z
      Xiki.def("enter+1"){ Clipboard.paste(1) }   # Enter 1
      Xiki.def("enter+2"){ Clipboard.paste(2) }   # Enter 2
      Xiki.def("enter+3"){ Clipboard.paste(3) }
      Xiki.def("enter+4"){ Clipboard.paste(4) };   Xiki.def("enter+5"){ Clipboard.paste(5) };   Xiki.def("enter+6"){ Clipboard.paste(6) }
      Xiki.def("enter+7"){ Clipboard.paste(7) };   Xiki.def("enter+7"){ Clipboard.paste(8) };   Xiki.def("enter+7"){ Clipboard.paste(9) }
    end

    def self.do_keys
      # D: do...
      # Use D prefix for: things that modify text or execute code

      #     Keys.D { insert "Apparently this is necessary to remap C-d" }
      #     Keys.DD { $el.delete_char $el.elvar.current_prefix_arg || 1 }   # DD - delete character (D's emacs default)
      #     Keys.do_as_camelcase { Clipboard.do_as_camel_case }   # change word to camel case (LikeThat)
      Xiki.def("do+as+execute"){ Console.do_as_execute }   # Run shell command on tree
      Xiki.def("do+as+html"){ Firefox.do_as_html }
      Xiki.def("do+as+browser"){ Firefox.exec_block }
      Xiki.def("do+as+javascript"){ Javascript.run }
      #     Xiki.def("do+as+launched"){ Launcher.do_as_launched }
      #     Xiki.def("do+as+php"){ Php.run }
      Xiki.def("do+as+menu"){ Menu.do_as_menu }   # Grab item after '@' and run it by itself
      Xiki.def("do+as+python"){ Python.run_block }
      # Do, take numeric prefix for before and after
      Xiki.def("do+add+space"){ Code.add_space }
      Xiki.def("do+as+test"){ Code.do_as_rspec }
      Xiki.def("do+as+wrap"){ Block.do_as_wrap }
      Xiki.def("do+as+quote"){ Notes.do_as_quote }
      Xiki.def("do+as+xul"){ Firefox.do_as_xul }
      # B
      Xiki.def("do+bookmark"){ FileTree.tree :recursive=>true }   # delete word backward
      Xiki.def("do+code+align"){ Code.do_code_align }   # Based on input character, all matches line up
      Xiki.def("do+click+back"){ Firefox.back }
      Xiki.def("do+create+directory"){ FileTree.do_create_dir }
      # Xiki.def("do+click+hyperlink"){ Firefox.click }   # compare with last AV version

      Xiki.def("do+current+file"){ Files.delete_current_file }

      Xiki.def("do+compare+history"){ History.diff_with_backup }   # compare with last AV version

      Xiki.def("do+code+indent"){ Code.indent }
      Xiki.def("do+count+matches"){  View.count_matches }
      Xiki.def("do+copy+name"){ Clipboard.copy_name }   # Copy file name (without extension and path)
      Xiki.def("do+colors+off"){ $el.font_lock_mode }   # toggles
      Xiki.def("do+clean+quotes"){ Files.do_clean_quotes }   # Fix special chars

      Xiki.def("do+compare+repository"){ Git.do_compare_repository }

      Xiki.def("do+compare+saved"){ DiffLog.compare_with_saved }

      Xiki.def("do+copy+to"){ FileTree.copy_to }
      Xiki.def("do+compare+views"){ DiffLog.do_compare_with :u }
      Xiki.def("do+compare+with"){ DiffLog.do_compare_with }
      Xiki.def("do+compare+1"){ Clipboard.diff_1_and_2 }   # Compare contents of clipboards "1" and "2"

      Xiki.def("do+delete"){ $el.delete_char $el.elvar.current_prefix_arg || 1 }   # DD - delete character (D's emacs default)
      # Probably rename this to not "expand", now that that is kind of the new word for "launch"
      Xiki.def("do+expand"){ $el.dabbrev_expand nil }   # expand abbreviation / autocomplete
      Xiki.def("do+forward"){ $el.kill_word(Keys.prefix || 1) }   # delete word forward

      # H
      # G: leave unmapped for escape
      Xiki.def("do+here"){ Launcher.do_last_launch :here=>1 }   # collapse current menu and run again
      Xiki.def("do+indent"){ Code.indent_to }
      Xiki.def("do+job"){ Macros.run }   # do last macro
      Xiki.def("do+kill+all"){ Effects.blink :what=>:all; View.kill_all }   # kill all text in buffer
      Xiki.def("do+kill+indented"){ CodeTree.do_kill_indented }  # Delete menu or file or whatever (just passes "0") prefix
      Xiki.def("do+kill+matching"){ Search.kill_filter }
      Xiki.def("do+kill+nonmatching"){ Search.kill_filter }
      Xiki.def("do+kill+paragraph"){ View.kill_paragraph }   # kill all text in buffer
      Xiki.def("do+kill+rest"){ CodeTree.kill_rest }   # kill adjacent lines at same indent as this one
      Xiki.def("do+kill+siblings"){ CodeTree.kill_siblings }   # kill adjacent lines at same indent as this one
      Xiki.def("do+kill+trailing"){ View.gsub!(/[ 	]+$/, "") }   # Deletes trailing whitespace
      Xiki.def("do+list+ancestors"){ View.beep "- Changed to: do+visibility!" }   # Moved to do+visibility
      Xiki.def("do+load+browser"){ Browser.reload }
      Xiki.def("do+last+command"){ Console.do_last_command }
      Xiki.def("do+line+duplicate"){ Line.duplicate_line }
      Xiki.def("do+load+file"){ Files.do_load_file }   # U prefix will auto-update / auto-refresh to relflect changes
      Xiki.def("do+lines+having"){   # delete lines matching a regex
        unless $el.elvar.current_prefix_arg
          delete_matching_lines( Keys.input(:prompt => "Delete lines having: ") )
        else
          delete_non_matching_lines( Keys.input(:prompt => "Delete lines not having: ") )
        end
      }
      Xiki.def("do+lines+jumble"){ Code.randomize_lines }   # Shuffle lines
      Xiki.def("do+linebreaks+linux"){ $el.set_buffer_file_coding_system :unix }
      Xiki.def("do+line+next"){ Line.move :next }
      Xiki.def("do+line+previous"){ Line.move(:previous) }
      Xiki.def("do+lines+reverse"){ $el.reverse_region($el.region_beginning, $el.region_end) }
      Xiki.def("do+lines+sort"){ Line.do_lines_sort }

      Xiki.def("do+lines+toggle"){ Line.do_lines_toggle }   # Swap next N lines

      Xiki.def("do+lines+unique"){ Code.kill_duplicates }   # Uniqify, delete duplicates
      Xiki.def("do+linebreaks+windows"){ $el.set_buffer_file_coding_system :dos }
      Xiki.def("do+move+to"){ FileTree.move_to }
      Xiki.def("do+name+buffer"){ Buffers.rename }
      Xiki.def("do+notes+colors"){ FileTree.apply_styles; Notes.apply_styles; FileTree.apply_styles_at_end }
      Xiki.def("do+number+enter"){ Incrementer.enter }
      Xiki.def("do+name+file"){ FileTree.rename_file }
      Xiki.def("do+number+increment"){ Incrementer.increment }
      Xiki.def("do+number+one"){ Incrementer.start }
      Xiki.def("do+next+paragraph"){ Code.do_next_paragraph }   # Move line to start of next paragraph
      Xiki.def("do+not+saved", "not saved/")
      Xiki.def("do+outline"){ History.open_current :outline=>true, :prompt_for_bookmark=>true }
      #     Xiki.def("do+push"){ Git.code_tree_diff }   # Commit to repos, push, etc
      Xiki.def("do+push"){ Git.do_push }   # Commit to repos, push, etc
      Xiki.def("do+query"){ Search.query_replace }   # do query replace
      Xiki.def("do+run"){ Code.run }   # run code as ruby
      Xiki.def("do+status"){ Git.do_status }
      Xiki.def("do+todo"){ Launcher.do_last_launch }
      Xiki.def("do+up"){ Launcher.do_last_launch :here=>1 }
      # V
      $el.define_key :global_map, $el.kbd("C-d C-v"), :cua_set_rectangle_mark   # Keys.do_vertical, do+vertical
      Xiki.def("do+whitespace"){ Deletes.delete_whitespace }   # delete blank lines
      # X
      Xiki.def("do+you"){ $el.delete_char $el.elvar.current_prefix_arg || 1 }   # Delete character
      Xiki.def("do+zip+next"){ Files.zip }
      Keys.set("C-d C-.") {   # Do .:  Go to point/bookmark starting with "." and run it (like pressing C-. on that line)
        input = Keys.input(:timed => true)
        $el.with(:save_window_excursion) do
          Bookmarks.go(".#{input}")
          Launcher.launch
        end
      }
      Keys.set("C-d C-/") { Code.comment }

      Xiki.def("do+1"){ Search.query_replace_nth "1", "2" }
      Xiki.def("do+2"){ Search.query_replace_nth "2", "1" }
      Xiki.def("do+3"){ Search.query_replace_nth "3", "4" }
      Xiki.def("do+4"){ Search.query_replace_nth "4", "3" }

    end

    def self.to_keys
      # T: to...
      # Use T prefix for: moving cursor, jumping to specific points

      $el.el4r_lisp_eval(%Q`(global-set-key (kbd "C-\'") \'repeat)`)

      #     Keys.TT { $el.transpose_chars $el.elvar.current_prefix_arg }   # TT - toggle character (T's emacs default)
      Xiki.def("to+axis"){ Move.to_axis }   # to beginning of file
      # B
      Xiki.def("to+backward"){ View.beep "- Changed to: up+back!" }   # move backward one word
      Xiki.def("to+column"){ Move.to_column }   # to x coordinate - ie column
      # D
      Xiki.def("to+end"){ Move.to_end }   # To end of line
      # F
      Xiki.def("to+forward"){ View.beep "- Changed to: up+forward!" }   # move forward one word
      Xiki.def("to+highest"){ View.to_highest }   # to beginning of file
      Xiki.def("to+indent"){ Move.to_indent }
      Xiki.def("to+junior"){ Move.to_junior }
      Xiki.def("to+kind"){ Move.to_other_bracket }   # to matching bracket, etc
      Xiki.def("to+lowest"){ View.to_bottom }   # move to end
      Xiki.def("to+menu"){ Menu.to_menu }
      Xiki.def("to+next"){ Move.to_next_paragraph }   # to next paragraph
      Xiki.def("to+outline"){ FileTree.to_outline }
      Xiki.def("to+previous"){ Move.to_previous_paragraph }   # to beginning of previous paragraph
      Xiki.def("to+quote"){ Move.to_quote }   # move to next ...|... quote
      Xiki.def("to+row"){ Move.to_line }   # go to nth line, relative to top of window
      Xiki.def("to+spot"){ Location.to_spot }

      Xiki.def("to+toggle"){ View.toggle }   # TT - toggle character (T's emacs default)

      Xiki.def("to+up"){ Tree.to_parent }   # to parent (last line indented less)
      Xiki.def("to+visible"){ View.to_relative }   # go to nth line, relative to top of window
      Xiki.def("to+words"){ Line.to_beginning }   # move to start of words on line
      # X
      # Z

      Xiki.def("to+0"){ View.to_nth_paragraph 0 }
      Xiki.def("to+1"){ View.to_nth_paragraph 1 }
      Xiki.def("to+2"){ View.to_nth_paragraph 2 }
      Xiki.def("to+3"){ View.to_nth_paragraph 3 }
      Xiki.def("to+4"){ View.to_nth_paragraph 4 }
      Xiki.def("to+5"){ View.to_nth_paragraph 5 }
      Xiki.def("to+6"){ View.to_nth_paragraph 6 }
      Xiki.def("to+7"){ View.to_nth_paragraph 7 }
      Xiki.def("to+8"){ View.to_nth_paragraph 8 }
      Xiki.def("to+9"){ View.to_nth_paragraph 9 }

      Keys.set("C-t C-/") { Code.to_comment }

    end

    def self.layout_keys
      # L: layout...
      # Use L prefix for: adjusting the layout, changing what is visible

      #     Keys.LL { View.recenter }   # LL - recenter (L's emacs default)
      Xiki.def("layout+all"){ View.hide_others }
      Xiki.def("layout+balance"){ 3.times { View.balance } }   # balance windows
      Xiki.def("layout+create"){ View.create }   # open new view
      Xiki.def "layout+dimensions", "dimensions/", :letter=>1

      Xiki.def("layout+expand"){ View.enlarge }
      # F
      Xiki.def("layout+files"){ View.layout_files }
      Xiki.def("layout+hide"){ View.hide }
      Xiki.def("layout+indent"){ Hide.hide_by_indent }   # only show lines indented less than x
      Xiki.def("layout+jump"){ View.shift }
      Xiki.def("layout+kill"){ View.kill }
      Xiki.def("layout+look"){ View.recenter }   # LL - recenter (L's emacs default)
      Xiki.def "layout+mark", "mark/", :letter=>1
      Xiki.def("layout+next"){ View.next(:blink=>true) }
      Xiki.def("layout+outlog"){ View.layout_outlog }
      Xiki.def("layout+previous"){ View.previous(:blink=>true) }
      # Q
      Xiki.def("layout+right"){ View.to_upper(:blink=>true) }   # Go to view to the right
      Xiki.def("layout+search"){ Keys.prefix_u? ? Search.find_in_buffers(Keys.input(:prompt=>"Search all open files for: ")) : Hide.search }
      Xiki.def("layout+todo"){ View.layout_todo }   # show bar on left with the quick bookmark named "-t"
      Xiki.def("layout+uncover"){ Hide.reveal }   # Reveal all hidden text
      Xiki.def "layout+visibility", "window/visibility/", :letter=>1
      Xiki.def("layout+wrap"){ $el.toggle_truncate_lines }   # wrap lines
      # X
      # Y
      Xiki.def("layout+zoom"){ $el.narrow_to_region($el.region_beginning, $el.region_end) }   # show selection only

      Xiki.def("layout+0"){ View.recenter_top }   # Layout 0: scroll so cursor is 0 lines from top af window
      Xiki.def("layout+1"){ Move.to_window(1, :blink=>true) }   # Layout 1
      Xiki.def("layout+2"){ Move.to_window(2, :blink=>true) }   # Layout 2
      Xiki.def("layout+3"){ Move.to_window(3, :blink=>true) };  Xiki.def("layout+4"){ Move.to_window(4, :blink=>true) }
      Xiki.def("layout+5"){ Move.to_window(5, :blink=>true) };  Xiki.def("layout+6"){ Move.to_window(6, :blink=>true) };  Xiki.def("layout+7"){ Move.to_window(7, :blink=>true) };  Xiki.def("layout+8"){ Move.to_window(8, :blink=>true) }
      Xiki.def("layout+9"){ Move.to_last_window(:blink=>true) }

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
      Xiki.def("search+axis"){ Search.to_left }


      Xiki.def("search+bookmark"){ Search.bookmark }
      # B: leave unmapped for back

      Xiki.def("search+copy"){ Search.isearch_copy }   # Clipboard (copy) or search+commands (if no search)

      Xiki.def("search+delete"){ Search.isearch_delete }   # Delete
      Xiki.def("search+enter"){ Search.enter }   # Enter: insert clipboard, replacing match
      Xiki.def("search+frontward"){ Search.go_to_end }   # Forward
      Xiki.def("search+g"){ Search.cancel }   # Stop searching
      # have_...
      $el.define_key :isearch_mode_map, $el.kbd("C-h"), nil
      Xiki.def("search+have+after"){ Search.isearch_move_to "$t", :append=>1 }
      Xiki.def("search+have+bullet"){ Search.have_label }
      Xiki.def("search+have+case"){ Search.isearch_have_case }
      Xiki.def("search+have+edges"){ Search.just_edges }   # Delete everything but chars at edges of match
      Xiki.def("search+have+files"){ View.beep "- Changed to: search+have+navigation!" }
      Xiki.def("search+have+highest"){ Search.isearch_restart :top }
      Xiki.def("search+have+javascript"){ Search.isearch_have_outlog_javascript }
      Xiki.def("search+have+line"){ Search.have_line }   # copy line back to search start
      Xiki.def("search+have+mock"){ Search.isearch_have_outlog :method=>".mock" }
      Xiki.def("search+have+nav"){ Search.isearch_move_to "$f" }

      Xiki.def("search+have+outlog"){ Search.isearch_have_outlog }
      Xiki.def("search+have+push"){ Git.search_just_push }   # When search match

      Xiki.def("search+have+right"){ Search.have_right }
      Xiki.def("search+have+spot"){ Search.insert_at_spot }
      Xiki.def("search+have+todo"){ Search.isearch_move_to "$t" }
      Xiki.def("search+have+variable"){ Search.insert_var_at_search_start }
      Xiki.def("search+have+within"){ Search.isearch_have_within }   # Grab everything except chars on edges

      # AVAILABLE: search_i ?  (when nothing searched for)

      # I: leave unmapped - had issues using it (messes up position)
      # just_...
      $el.define_key :isearch_mode_map, $el.kbd("C-j"), nil
      Xiki.def("search+just+adjust"){ Search.isearch_just_adjust }
      Xiki.def("search+just+bookmark"){ Search.just_bookmark }
      Xiki.def("search+just+case"){ Search.isearch_just_case }   # make match be camel case
      Xiki.def("search+just+difflog"){ Search.jump_to_difflog }   # find last string in difflog
      Xiki.def("search+just+edits"){ Search.just_edits }   # Search in diff of edits to this file
      Xiki.def("search+just+files"){ Search.isearch_restart "$f" }   # isearch for this string in $f
      Xiki.def("search+just+have"){ Search.just_select }   # select match
      Xiki.def("search+just+integer"){ Search.stop; Search.isearch "[0-9][0-9.]*", :regex=>1 }

      Xiki.def("search+just+menu"){ Search.just_menu }
      #     Xiki.def("search+just+mark"){ Search.just_marker }
      Xiki.def("search+just+next"){ Search.isearch_restart :next }
      Xiki.def("search+just+outlog"){ Search.isearch_restart "$o" }
      Xiki.def("search+just+previous"){ Search.isearch_restart :previous }
      Xiki.def("search+just+query"){ Search.isearch_query_replace :match }   # replace
      Xiki.def("search+just+right"){ Search.isearch_restart :right }   # Search in top-right view
      #     Xiki.def("search+just+search"){ Search.isearch_just_search }   # Add "##search" line in tree for match
      Xiki.def("search+just+special"){ Search.isearch_just_special }   # Add "##search" line in tree for match
      Xiki.def("search+just+todo"){ Search.isearch_restart "$t" }   # isearch for this string in $t

      Xiki.def("search+just+variable"){ Search.isearch_just_surround_with_char '#{', '}' }
      Xiki.def("search+just+wrap"){ Ol << 'search_just_wrap';  toggle_truncate_lines }   # make match be snake case

      Xiki.def("search+just+yellow"){ Search.just_orange }
      Xiki.def("search+kill"){ Search.cut }   # cut

      $el.define_key :isearch_mode_map, $el.kbd("C-l"), nil

      Xiki.def("search+like+clipboard"){
        reverse = Search.was_reverse
        match = Search.stop
        Search.isearch Clipboard[0], :reverse=>reverse
      }   # make match be camel case
      Xiki.def("search+like+delete"){ Search.like_delete }   # Delete all lines that contain the match
      Xiki.def("search+like+file"){ Search.isearch_open }
      Xiki.def("search+like+menu"){ Launcher.search_like_menu }
      Xiki.def("search+like+outlog"){ Search.isearch_have_outlog :no_label=>1 }
      Xiki.def("search+line+pull"){ Search.isearch_move_line }
      Xiki.def("search+like+quote"){ Search.isearch_google :quote=>true }
      Xiki.def("search+like+repository"){ Git.search_repository }   # When not searching

      Xiki.def("search+like+synonyms"){ Search.search_thesaurus }
      #     Xiki.def("search+like+timer"){ Search.search_like_timer }

      #     Xiki.def("search+like+thesaurus"){ Search.search_thesaurus }
      Xiki.def("search+last+urls"){ Launcher.open("- Launcher.urls/") }
      Xiki.def("search+like+variable"){ Search.just_name }
      Xiki.def("search+like+web"){ Search.isearch_google }   # make match be snake case
      Xiki.def("search+like+xiki"){ View.open "$x/#{Search.stop.strip}" }

      # Use search_navigated instead
      #     Xiki.def("search+last+launched"){ Search.search_last_launched }
      #     Xiki.def("search+log"){ Search.search_log }
      # M: leave unmapped for stop
      Xiki.def("search+next"){ Search.isearch_next }   # Next, or navigated (if nothing searched for yet)
      Xiki.def("search+outline"){ Search.isearch_outline }   # Outline
      Xiki.def("search+paths"){ Search.isearch_paths }   # Just go to previous line
      # P: leave unmapped for previous
      # Q: leave unmapped for quoting
      # R: leave unmapped for reverse
      # S: leave unmapped for search
      Xiki.def("search+to"){ Search.isearch_to }   # To: open file / jump to method
      Xiki.def("search+usurp"){ Search.isearch_pull_in_sexp }   # usurp: pull sexp into search string
      Xiki.def("search+value"){ Search.insert_at_search_start }   # Value: copy value back to search start
      # W: leave unmapped for pulling into search

      Xiki.def("search+xiki"){ Search.xiki }   # search+xiki+__ mapped inside this method


      # Y: leave unmapped for yank
      Xiki.def("search+zap"){ Search.zap }   # zap - delete up until search start

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


      #       #
      #       # Temporarily make Command+Return do old launch!!!
      #       #
      #       $el.define_key(:osx_key_mode_map, $el.kbd("<A-return>")) { Launcher.go_preunified }

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
      Keys.set("C-.") { Launcher.go }   # control period
      self.map_meta_return
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
