module Xiki
  class KeyShortcuts

    # Define all keys
    def self.keys

      Menu.init

      self.window_keys
      self.open_keys

      self.as_keys
      self.enter_keys

      self.hop_keys
      self.jump_keys

      self.do_keys
      self.run_keys

      self.search_keys   # time > (0.073769)
      self.misc   # time > (0.0451)

    end

    def self.as_keys

      # This breaks > probably unset C-a first, so we can go back to this

      Xiki.def("as+axis"){ Move.hop_left_key }

      Xiki.def("as+command"){ Menu.as_menu }
      Xiki.def("as+bookmark"){ Bookmarks.save }

      Xiki.def("as+line"){ Clipboard.as_line }
      Xiki.def("as+paragraph"){ Clipboard.copy_paragraph }   # copy paragraph
      Xiki.def("as+following"){ Clipboard.copy_paragraph(:rest => true) }
      Xiki.def("as+unsaved"){ DiffLog.compare_with_saved }
      Xiki.def("as+directory"){ FileTree.copy_path }   # copy dir to clipboard from tree

      Xiki.def("as+todo"){ Notes.as_todo }
      Xiki.def("as+nav"){ Notes.as_file }   # Save in :n

      Xiki.def("as+remembered"){ Location.as_spot }   # remember point in file
      Xiki.def("as+select"){ Clipboard.select }
      Xiki.def("as+version"){ History.backup_file }   # creates backup
      Xiki.def("as+indented"){ CodeTree.as_indented }
      Xiki.def("as+macro"){ Macros.record }   # start recording macro

      # Todo > remove as+open after task items supercede it (in notes, etc.)?
      #   Wait > these may be 2 distinct cases (think about :n)
      #     as+open may still be needed, like when you want to open a file from :n, but it has an outline underneath that you don't want to close
      Xiki.def("as+open"){ Launcher.as_open }
      Xiki.def("as+everything"){ Clipboard.copy_everything }

      Xiki.def("as+grab"){ $el.save_place_kill_emacs_hook; DiffLog.save_grab_location }   # as+bookmark+g is almost as good, but doesn't save the specific place in the file

      Xiki.def("as+you"){ Clipboard.as_thing }   # copy object / symbol at point
    end

    def self.open_keys
      # O: open...
      # Use O prefix for: opening, jumping to files

      Xiki.def("open+help", :noob=>1){ Launcher.open("help/") }   # OO - open line (O's emacs default)

      Xiki.def("open+todo", :noob=>1){ Notes.open_todo }
      Xiki.def("open+nav", :noob=>1){ Notes.open_todo :bookmark=>":n" }


      Xiki.def("open+prompt", :noob=>1){ Launcher.open_prompt }
      Xiki.def("open+command"){ Launcher.open_command }
      Xiki.def("open+sessions", :noob=>1){ Launcher.open "sessions/" }

      Xiki.def("open+file", :noob=>1){ Bookmarks.go }
      Xiki.def("open+modified"){ Bookmarks.go nil, :date_sort=>true }

      Xiki.def("open+untitled", :noob=>1){ View.new_file }
      Xiki.def("open+views", :noob=>1){ Launcher.open("views/") }

      Xiki.def("open+edited"){ Files.open_edited }   # show recently edited files
      Xiki.def("open+diffs"){ DiffLog.open }   # shows diffs of what you've edited
      Xiki.def("open+recent", "recent/")
      Xiki.def("open+bookmarks"){ Launcher.open "bookmarks/" }
      Xiki.def("open+quick"){ Notes.open_todo :bookmark=>":q" }   # shows diffs of what you've edited

      Xiki.def("open+key"){ Keys.jump_to_code }   # jump to ruby code of key definition
      Xiki.def("open+whereabouts"){ Code.do_list_ancestors }

      Xiki.def("open+often"){ View.open ":o" }

      # G: leave unmapped for escape

      Xiki.def("open+list+faces"){ Styles.list_faces }   # list

      Xiki.def("open+in+browser"){ Browser.open_in_browser }
      Xiki.def("open+in+left"){ View.open_in_bar }
      Xiki.def("open+in+os"){ Files.open_in_os }
      Xiki.def("open+in+window"){ Files.open_in_window }   # Expose file in OS folder

      # open+menu > __ open xiki.
      Xiki.def("open+as+output"){ OlHelper.open_last_outlog }   # last
      Xiki.def("open+as+calendar"){ $el.calendar }
      Xiki.def("open+as+file"){ Code.open_as_file }
      Xiki.def("open+as+elisp"){ $el.find_function_at_point }   # jump to definition of lisp function
      Xiki.def("open+as+highest"){ FileTree.open_as_upper }
      Xiki.def("open+as+lowest"){ FileTree.open_as_upper(:lowest) }
      Xiki.def("open+as+utf"){ $el.revert_buffer_with_coding_system('utf-8'.to_sym) }
      Xiki.def("open+as+2"){ FileTree.open_as_upper(:second) }
      Xiki.def("open+as+root"){ Files.open_sudo }
      Xiki.def("open+as+shell"){ Shell.open }
      Xiki.def("open+as+tail"){ Files.open_tail }

      Xiki.def("open+xiki+docs"){ Help.display_docs }
      # Y
      # Z

      Xiki.def("open+1"){ Files.open_nth 1 };  Xiki.def("open+2"){ Files.open_nth 2 };  Xiki.def("open+3"){ Files.open_nth 3 };  Xiki.def("open+4"){ Files.open_nth 4 };  Xiki.def("open+5"){ Files.open_nth 5 }
      Xiki.def("open+6"){ Files.open_nth 6 };  Xiki.def("open+7"){ Files.open_nth 7 };  Xiki.def("open+8"){ Files.open_nth 8 };  Xiki.def("open+9"){ Files.open_nth 9 }

      Xiki.def("open+0"){ Files.open_nth 0 }   # Open 0: open line in :n that cursor is on

      Xiki.def("open+8"){ History.open_current :all => true, :prompt_for_bookmark => true }   # Like do_outline, but inserts all
    end


    def self.enter_keys
      # E: enter...
      # Use E prefix for: inserting

      Xiki.def("enter+end"){ Move.hop_right_key }

      Xiki.def("enter+bullet"){ Notes.bullet }   # Commit to repos, push, etc
      Xiki.def("enter+command"){ Launcher.insert_menu }
      Xiki.def("enter+space"){ Code.enter_whitespace }

      Xiki.def("enter+line"){ View.insert_line }
      Xiki.def("enter+junior"){ Notes.enter_junior }
      Xiki.def("enter+web"){ Search.enter_insert_search }

      Xiki.def("enter+note"){ Notes.enter_note }
      Xiki.def("enter+key"){ Keys.insert_code }
      Xiki.def("enter+url"){ Firefox.enter_as_url }

      Xiki.def("enter+history"){ View << "$"; Launcher.launch }
      Xiki.def("enter+todo"){ View.enter_upper }
      Xiki.def("enter+file"){ Files.enter_file }   # in tree, enter methods or headings
      Xiki.def("enter+outline"){ Launcher.enter_outline }   # in tree, enter methods or headings

      Xiki.def("enter+quote"){ FileTree.enter_quote }
      Xiki.def("enter+prompt"){ View.insert_shell_prompt }
      Xiki.def("enter+multi"){ $el.cua_set_rectangle_mark }

      Xiki.def("enter+in+time"){ Line.insert_time }
      Xiki.def("enter+in+date"){ Line.insert_date }

      Xiki.def("enter+in+replacement"){ Search.insert_before_and_after }
      Xiki.def("enter+in+new"){ DiffLog.enter_new }           # Enter Old: enter newly-deleted from last save
      Xiki.def("enter+in+old"){ DiffLog.enter_old }   # Enter Old: enter newly-deleted from last save

      Xiki.def("enter+in+http"){ View << "http://" }
      Xiki.def("enter+in+ruby"){ code = Keys.input(:prompt=>"Enter ruby code to eval and insert results: "); View.insert(eval(code).to_s)}
      Xiki.def("enter+in+javascript"){ Firefox.enter_log_javascript_line }   # Xiki.def("enter+log+javascript"){ Firefox.enter_log_javascript_line }

      Xiki.def("enter+in+log"){ Code.enter_log_line }   # Xiki.def("enter+log+line"){ Code.enter_log_line }
      Xiki.def("enter+in+stack"){ Code.enter_log_stack }   # Xiki.def("enter+log+stack"){ Code.enter_log_stack }
      Xiki.def("enter+in+exclamation"){ Code.enter_log_line :exclamation=>1 }   # Xiki.def("enter+log+line"){ Code.enter_log_line }
      Xiki.def("enter+in+performance"){ Code.enter_log_time }   # Xiki.def("enter+log+time"){ Code.enter_log_time }

    end


    def self.jump_keys

      Xiki.def("jump+todo", :noob=>1){ View.layout_todo }
      Xiki.def("jump+nav", :noob=>1){ View.layout_nav }   # jump+n
      Xiki.def("jump+ide", :noob=>1){ View.layout_todo_and_nav }

      Xiki.def("jump+higher", :noob=>1){ View.page_up }
      Xiki.def("jump+lower", :noob=>1){ View.page_down }

      Xiki.def("jump+output"){ View.layout_outlog }
      Xiki.def("jump+all"){ View.layout_outlog :all=>1 }

      Xiki.def("jump+quick"){ View.layout_quick }

      Xiki.def("jump+unsaved"){ Launcher.open("unsaved/") }
      Xiki.def("jump+repository"){ Git.do_compare_repository }

      Xiki.def("jump+expose+test"){ Code.open_related_rspec }

      Xiki.def("jump+move+file"){ Launcher.open("Todo > Find method that moves the current file...", :no_launch=>1) }
      Xiki.def("jump+move+remembered"){ FileTree.move_to }
      Xiki.def("jump+move+copy"){ FileTree.copy_to }

      Xiki.def("jump+yours"){ FileTree.copy_to }

      Xiki.def("jump+status"){ Git.do_status }
      Xiki.def("jump+diff"){ Git.do_push }   # Commit to repos, push, etc

      Xiki.def("jump+command"){ Launcher.open_nested_command }
      Xiki.def("jump+prompt"){ Shell.prompt_for_bookmark }

      # Doesn't seem to work any more?
      #      Xiki.def("jump+history"){ Shell.history_for_bookmark }

      Xiki.def("jump+xiki+methods"){ Launcher.open("#{Xiki.dir}\n  - ##^ *def /") }
      Xiki.def("jump+xiki+directory"){ FileTree.tree :recursive=>1, :bm=>"xiki" }

      # jump+N > jump to nth visible paragraph (ie jump+5 to jump to the 5th paragraph)
      (1..9).each do |n|
        Xiki.def("jump+#{n}"){ View.to_nth_fraction n }
      end

    end


    def self.run_keys
      # D: do...
      # Use D prefix for: things that modify text or execute code

      Xiki.def("run+recent"){ Shell.recent_history_external nil, :from_key_shortcut=>1 }
      Xiki.def("run+macro"){ Macros.run }   # do last macro
      Xiki.def("run+todo"){ Launcher.do_task }
      Xiki.def("run+up"){ Launcher.do_last_launch :here=>1 }

      Xiki.def("run+indent"){ Code.indent_to }
      Xiki.def("run+comment"){ Code.comment }
      Xiki.def("run+eval"){ Code.run }   # run code as ruby
      Xiki.def("run+highlight"){ Color.run_highlight }

      Xiki.def("run+query"){ Search.query_replace }   # do query replace

      Xiki.def("run+version+next"){ DiffLog.compare_views }
      Xiki.def("run+version+spot"){ DiffLog.compare_in_tree }

      Xiki.def("run+version+repository"){ Git.do_compare_repository }
      Xiki.def "run+version+log", ".=git/log/"   # Show git diffs of current file

      Xiki.def("run+version+backups"){ History.list }   # Show all backed-up versions
      Xiki.def("run+version+previous"){ History.diff_with_backup }   # compare with last AV version
      Xiki.def("run+version+edits"){ Launcher.open("#{View.file}\n  =edits/") }

      Xiki.def("run+as+align"){ Code.do_code_align }   # Based on input character, all matches line up
      Xiki.def("run+as+execute"){ Shell.do_as_execute }   # Run shell command on tree
      Xiki.def("run+as+html"){ Firefox.do_as_html }
      Xiki.def("run+as+browser"){ Firefox.exec_block }
      Xiki.def("run+as+javascript"){ Javascript.run }
      Xiki.def("run+as+command"){ Menu.do_as_menu }   # Grab item after '@' and run it by itself
      Xiki.def("run+as+outline"){ Menu.do_as_outline }   # Grab item after '@' and run it by itself
      Xiki.def("run+as+python"){ Python.run }

      Xiki.def("run+as+space"){ Code.add_space }
      Xiki.def("run+as+test"){ Code.do_as_rspec }
      Xiki.def("run+as+wrap"){ Block.do_as_wrap }
      Xiki.def("run+as+quote"){ Notes.do_as_quote }
      Xiki.def("run+as+xul"){ Firefox.do_as_xul }
      Xiki.def("run+as+deck"){ Deck.enable_arrow_keys }
      Xiki.def("run+as+remote"){ Remote.save_file }

      Xiki.def("run+delete+everything"){ Effects.blink :what=>:all; View.kill_all }   # kill all text in buffer

      Xiki.def("run+delete+indented"){ CodeTree.do_kill_indented }
      Xiki.def("run+delete+siblings"){ CodeTree.kill_siblings }   # kill adjacent lines at same indent as this one
      Xiki.def("run+delete+trailing"){ View.gsub!(/[ 	]+$/, "") }   # Deletes trailing whitespace

      Xiki.def("run+delete+current"){ Files.delete_current_file }
      Xiki.def("run+delete+unsaved"){ Files.do_load_file }

      Xiki.def("run+delete+matching"){ Search.kill_filter }
      Xiki.def("run+delete+nonmatching"){ Search.kill_filter }

      Xiki.def("run+lines+command"){ Shell.do_last_command }
      Xiki.def("run+lines+having"){   # delete lines matching a regex
        unless $el.elvar.current_prefix_arg
          $el.delete_matching_lines( Keys.input(:prompt => "Delete lines having: ") )
        else
          $el.delete_non_matching_lines( Keys.input(:prompt => "Delete lines not having: ") )
        end
      }
      Xiki.def("run+lines+jumble"){ Code.randomize_lines }   # Shuffle lines
      Xiki.def("run+lines+linux"){ $el.set_buffer_file_coding_system :unix }
      Xiki.def("run+lines+reverse"){ $el.reverse_region($el.region_beginning, $el.region_end) }
      Xiki.def("run+lines+sort"){ Line.do_lines_sort }

      Xiki.def("run+lines+toggle"){ Line.do_lines_toggle }   # Swap next N lines

      Xiki.def("run+lines+unique"){ Code.kill_duplicates }   # Uniqify, delete duplicates
      Xiki.def("run+lines+windows"){ $el.set_buffer_file_coding_system :dos }
      Xiki.def("run+jump+up"){ FileTree.move_up }

      Xiki.def("run+next"){ Line.move :next }
      Xiki.def("run+previous"){ Line.move(:previous) }

      # Probably map these to something else
      # Xiki.def("run+name+view"){ Buffers.rename }
      # Xiki.def("run+name+enter"){ Incrementer.enter }
      # Xiki.def("run+name+increment"){ Incrementer.increment }
      # Xiki.def("run+name+one"){ Incrementer.start }


      # Put here temporarily
      Xiki.def("run+output"){ View.layout_outlog }
      Xiki.def("run+browser+reload"){ Browser.reload }

      Xiki.def("run+1"){ Launcher.do_last_launch :nth=>1 }
      Xiki.def("run+2"){ Launcher.do_last_launch :nth=>2 }
      Xiki.def("run+3"){ Launcher.do_last_launch :nth=>3 }
      Xiki.def("run+4"){ Launcher.do_last_launch :nth=>4 }
      Xiki.def("run+5"){ Launcher.do_last_launch :nth=>5 }
      Xiki.def("run+6"){ Launcher.do_last_launch :nth=>6 }
      Xiki.def("run+7"){ Launcher.do_last_launch :nth=>7 }

    end


    def self.hop_keys
      # Use H prefix for: moving cursor, jumping to specific points

      Xiki.def("hop+hit", :noob=>1){ Deletes.backward }

      Xiki.def("hop+top"){ View.to_highest }
      Xiki.def("hop+bottom"){ View.to_bottom }   # move to end

      # Todo > Change hop+ancestor to hop+up? or to hop+under (like hop+junior)?
      # and change > enter+junior enter+under

      # Todo > remove these two
      Xiki.def("hop+up", :noob=>1){ View.page_up }
      Xiki.def("hop+down", :noob=>1){ View.page_down }

      Xiki.def("hop+next"){ Move.to_next_paragraph }   # to next paragraph
      Xiki.def("hop+previous"){ Move.to_previous_paragraph :skip_if_top=>1 }   # to beginning of previous paragraph
      Xiki.def("hop+line"){ Move.to_line }

      Xiki.def("hop+outline"){ FileTree.to_outline }   # OO - open line (O's emacs default)
      Xiki.def("hop+search"){ Search.outline_search }
      Xiki.def("hop+edge"){ View.to_relative :line=>1 }   # go to nth line, relative to top of window (up+ for middle, up+up+ for bottom)
      Xiki.def("hop+indent"){ Move.to_indent }
      Xiki.def("hop+junior"){ Move.to_junior }

      # hop+climb is a possible alternative
      Xiki.def("hop+ancestor"){ Tree.to_parent }   # to parent (last line indented less)

      Xiki.def("hop+remembered"){ Location.hop_remembered }
      Xiki.def("hop+file"){ Launcher.open("hop to file/") }

      Xiki.def("hop+command"){ Command.to_menu }
      Xiki.def("hop+kind"){ Move.to_other_bracket }   # to matching bracket, etc

      Xiki.def("hop+quote"){ Move.to_quote }   # move to next ...|... quote

      Xiki.def("hop+middle"){ View.to_relative }   # go to nth line, relative to top of window (up+ for middle, up+up+ for bottom)
      Xiki.def("hop+words"){ Line.to_beginning }   # move to start of words on line
      # X
      # Z

      # hop+N > jump to nth fraction of the screen (ie hop+5 to jump to middle)
      (1..9).each do |n|
        Xiki.def("hop+#{n}"){ View.to_nth_paragraph n }
      end

    end

    def self.do_keys
      # Use for: dealing with windows

      Xiki.def("do+delete"){ Deletes.forward }
      Xiki.def("do+notes"){ Notes.open_note }

      Xiki.def("do+outline"){ History.open_current :outline=>true, :prompt_for_bookmark=>true }
      Xiki.def("do+file"){ FileTree.tree :recursive=>1 }

      Xiki.def("do+comment"){ Code.enter_insert_comment }
      Xiki.def("do+tasks"){ Launcher.tasks_menu_on_bookmark }
      Xiki.def("do+edits"){ DiffLog.show_edits }   # Diffs in particular file

      Xiki.def("do+menu"){ Keys.open_project_menu }

      Xiki.def("do+repeat"){ Line.duplicate }

      Xiki.def("do+quote"){ Launcher.open("views/", :task=>["quoted"]) }

    end

    def self.window_keys
      # Use for: adjusting the layout, changing what is visible

      Xiki.def("window+save", :noob=>1){ DiffLog.save }
      Xiki.def("window+wrap", :noob=>1){ $el.toggle_truncate_lines }   # wrap lines

      Xiki.def("window+close", :noob=>1){ View.hide :kill_if_only=>1 }

      Xiki.def("window+next"){ View.next(:blink=>true) }
      Xiki.def("window+previous"){ View.previous(:blink=>true) }

      Xiki.def("window+horizontal"){ View.create }
      Xiki.def("window+vertical"){ View.create_vertical }

      Xiki.def("window+delete"){ View.kill }

      Xiki.def("window+edge"){ View.recenter_top_key }
      Xiki.def("window+middle"){ View.recenter }   # LL - recenter (L's emacs default)

      Xiki.def("window+alone"){ View.hide_others }
      Xiki.def("window+grow"){ View.enlarge }
      Xiki.def("window+uniform"){ View.balance }
      Xiki.def("window+balance"){ View.balance :narrow_bar=>1 }

      Xiki.def("window+jump"){ View.shift }

      Xiki.def("window+tasks"){ Launcher.tasks_menu_on_bookmark :bm=>"." }

      Xiki.def "window+options", "tile settings/", :hotkey=>1   # Used to be +visible

      # Q
      Xiki.def("window+indent"){ Hide.hide_by_indent }   # only show lines indented less than x
      Xiki.def("window+right"){ View.to_upper(:blink=>true) }
      Xiki.def("window+first"){ Move.to_window(1, :blink=>true) }
      Xiki.def("window+last"){ Move.to_last_window(:blink=>true) }
      Xiki.def("window+zoom"){ View.zoom }   # show selection only

      Xiki.def("window+quit", :noob=>1){ DiffLog.quit }

      Xiki.def("window+1"){ Move.to_window(1, :blink=>true) }
      Xiki.def("window+2"){ Move.to_window(2, :blink=>true) }
      Xiki.def("window+3"){ Move.to_window(3, :blink=>true) };  Xiki.def("window+4"){ Move.to_window(4, :blink=>true) }
      Xiki.def("window+5"){ Move.to_window(5, :blink=>true) };  Xiki.def("window+6"){ Move.to_window(6, :blink=>true) };  Xiki.def("window+7"){ Move.to_window(7, :blink=>true) };  Xiki.def("window+8"){ Move.to_window(8, :blink=>true) }
      Xiki.def("window+9"){ Move.to_last_window(:blink=>true) }
      Xiki.def("window+0"){ View.recenter_top }   # Layout 0: scroll so cursor is 0 lines from top of window

    end

    def self.search_keys

      Code.cache(:search_keys) do

        $el.define_key :isearch_mode_map, "\\C-h", nil
        $el.define_key :isearch_mode_map, "\\C-j", nil
        $el.define_key :isearch_mode_map, "\\C-l", nil

        Xiki.def("search+copy", :eval=>"Search.isearch_copy")
        Xiki.def("search+xpand", :eval=>"Search.xiki")

        Xiki.def("search+copy", :eval=>"Search.isearch_copy")   # Clipboard (copy) (or search+commands if no search)

        Xiki.def("search+xpand", :eval=>"Search.xiki")   # search+xiki+__ mapped inside this method

        Xiki.def("search+kill", :eval=>"Search.isearch_clear")   # cut (or search at point of last cut, if no search)

        Xiki.def("search+value", :eval=>"Search.insert_at_search_start")   # Value: copy value back to search start
        Xiki.def("search+pull", :eval=>"Search.isearch_pull")   # Pull back to search start (or search+push if nothing searched for yet)
        Xiki.def("search+zap", :eval=>"Search.zap")   # zap - delete up until search start

        Xiki.def("search+todo", :eval=>"Search.isearch_todo")   # To: open file / jump to method
        Xiki.def("search+files", :eval=>"Search.bookmark")   # when no search, prompt for input
        Xiki.def("search+outline", :eval=>"Search.isearch_outline")   # Outline (or search+outlog if nothing searched for yet)

        Xiki.def("search+exchange", :eval=>"Search.isearch_query_replace")   # Outline (or search+outlog if nothing searched for yet)
        # Search+i > available

        Xiki.def("search+nav", :eval=>"Search.isearch_nav")   # isearch for this string in :n
        Xiki.def("search+usurp", :eval=>"Search.isearch_pull_in_sexp")   # usurp: pull sexp into search string

        Xiki.def("search+before", :eval=>"Search.isearch_or_copy('1')")
        Xiki.def("search+after", :eval=>"Search.isearch_or_copy('2')")

        Xiki.def("search+diffs", :eval=>"Search.isearch_diffs")   # Delete (or search+difflog if no search)
        Xiki.def("search+good", :eval=>"Search.cancel")   # Stop searching


        Xiki.def("search+have+bullet", :eval=>"Search.have_label")
        Xiki.def("search+have+case", :eval=>"Search.isearch_have_case")
        Xiki.def("search+have+edges", :eval=>"Search.just_edges")   # Delete everything but chars at edges of match

        Xiki.def("search+have+nav", :eval=>"Search.isearch_move_to ':n'")

        Xiki.def("search+have+remembered", :eval=>"Search.insert_at_spot")

        Xiki.def("search+have+javascript", :eval=>"Search.isearch_have_outlog_javascript")
        Xiki.def("search+have+line", :eval=>"Search.have_line")   # copy line back to search start
        Xiki.def("search+have+move", :eval=>"Search.isearch_move_line")   # Move line to where search started

        Xiki.def("search+have+output", :eval=>"Search.isearch_have_outlog")
        Xiki.def("search+have+push", :eval=>"Git.search_just_push")   # When search match

        Xiki.def("search+have+todo", :eval=>"Search.isearch_move_to ':t'")
        Xiki.def("search+have+variable", :eval=>"Search.insert_var_at_search_start")

        Xiki.def("search+have+wikipedia", :eval=>"Search.isearch_have_wikipedia")   # Grab everything except chars on edges
        Xiki.def("search+have+special", :eval=>"Search.isearch_just_special")

        # Just so it's consistent with ^H^H when deleting selection
        Xiki.def("search+have+hit", :eval=>"Search.isearch_clear")

        # I: leave unmapped - had issues using it (messes up position)
        Xiki.def("search+just+after", :eval=>"Search.isearch_just_after")
        Xiki.def("search+just+case", :eval=>"Search.isearch_just_case")   # make match be camel case
        Xiki.def("search+just+delete", :eval=>"Search.like_delete")   # Delete all lines that contain the match
        Xiki.def("search+just+edited", :eval=>"Search.just_edits")   # Search in diff of edits to this file

        Xiki.def("search+just+thesaurus", :eval=>"Search.search_thesaurus")

        Xiki.def("search+just+have", :eval=>"Search.just_select")   # select match

        Xiki.def("search+just+kill", :eval=>"Search.just_kill")

        Xiki.def("search+just+integer", :eval=>"Search.stop; Search.isearch '[0-9][0-9.]*', :regex=>1")
        Xiki.def("search+just+line", :eval=>"$el.toggle_truncate_lines")   # Line wrap without exiting isearch

        Xiki.def("search+just+menu", :eval=>"Search.just_menu")
        Xiki.def("search+just+swap", :eval=>"Search.search_just_swap")   # Swap the match with what's in the clipboard (put the match where the search started)
        Xiki.def("search+just+restart", :eval=>"Search.isearch_restart :top")

        Xiki.def("search+just+order", :eval=>"Search.isearch_just_adjust")   # Swap/toggle the two characters in match
        Xiki.def("search+just+variable", :eval=>"Search.isearch_just_surround_with_char '\#{', '}'")
        Xiki.def("search+just+parens", :eval=>"Search.isearch_just_surround_with_char '(', ')'")
        Xiki.def("search+just+brackets", :eval=>"Search.isearch_just_surround_with_char '[', ']'")
        Xiki.def("search+just+quotes", :eval=>"Search.isearch_just_surround_with_char '\"'")
        Xiki.def("search+just+wrap+apostrophe", :eval=>"Search.isearch_just_surround_with_char \"'\"")

        Xiki.def("search+just+yellow", :eval=>"Search.just_orange")


        Xiki.def("search+like+after", :eval=>"Search.query_replace_with_2")
        Xiki.def("search+like+command", :eval=>"Launcher.search_like_menu")
        Xiki.def("search+like+difflog", :eval=>"Search.jump_to_difflog")   # find last string in difflog
        Xiki.def("search+like+filename", :eval=>"Search.isearch_open")   # Open match as filename

        Xiki.def("search+like+nav", :eval=>"Search.isearch_restart ':n', :as_here=>1")
        Xiki.def("search+like+previous", :eval=>"Search.isearch_restart :previous")
        Xiki.def("search+like+right", :eval=>"Search.isearch_restart :right")   # Search in top-right view

        Xiki.def("search+like+spot", :eval=>"Search.isearch_just_search")   # Add "##search" line in tree for match

        Xiki.def("search+like+layout", :eval=>"$el.recenter 1")



        # Xiki.def("search+like+middle"){ $el.recenter 20
        #   Ol["Get this to use the actual middle > borrow: window+middle!"]
        # }

        Xiki.def("search+like+output", :eval=>"Search.isearch_restart ':o'")
        Xiki.def("search+like+quote", :eval=>"Search.isearch_google :quote=>true")

        Xiki.def("search+like+expanded", :eval=>"Search.like_expanded")
        Xiki.def("search+like+todo", :eval=>"Search.isearch_restart ':t', :as_here=>1")
        Xiki.def("search+like+variable", :eval=>"Search.just_name")
        Xiki.def("search+like+web", :eval=>"Search.isearch_google")   # make match be snake case
        Xiki.def("search+like+xiki", :eval=>"View.open \":xiki/\#{Search.stop.strip}\"")

      end

      # Pre-loading for caching done, don't do full-on el4r stuff...

      return if $el.caching

      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-]")) { Search.just_increment }   # search+just+Plus > alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-j C-_")) { Search.just_increment(:decrement=>true) }   # search+just+Minus > alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-_")) { Search.subtract }   # Remove one char from isearch > alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-]")) { $el.isearch_yank_char }     # alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-\\"), :isearch_abort_really)

      $el.define_key(:isearch_mode_map, "\e"){ Search.cancel }

      $el.el4r_lisp_eval %`
        (progn

          ; Has to be defined in lisp, since passing C-@ fails (it's null, and that's what we use as a delimiter)
          ; Definition for search+Space
          (define-key isearch-mode-map (kbd "C-@") (lambda () (interactive)
            (el4r-ruby-eval "Xiki::Search.isearch_highlight_match")
          ))
          ; Like isearch-abort, but aborts even if partially matching search in progress
          (defun isearch-abort-really ()
            (interactive)
            (discard-input)
            (setq isearch-success nil)
            (isearch-cancel)
          )

        )
      `

    end


    def self.map_control_return

      # Aquamacs-specific: make cua not map C-return key
      $el.define_key(:cua_global_keymap, $el.kbd("<C-return>"), nil) if $el.boundp(:cua_global_keymap)

      Keys.set("<C-return>") { Launcher.go }   # control-return, control-enter
    end

    def self.map_command_return
      return if ! $el.boundp(:osx_key_mode_map)

      $el.define_key(:osx_key_mode_map, $el.kbd("<A-return>")) { Launcher.go }   # Command+Return
    end

    # Not called by default
    def self.map_meta_return
      Keys.set("<M-return>") { Launcher.go }   # Command+Return, Command+Enter
    end

    def self.misc

      Xiki.def("grab+", :eval=>"DiffLog.grab")
      Xiki.def("quit+", :noob=>1, :eval=>"DiffLog.quit")
      Xiki.def("yours+", :eval=>"Launcher.open('yours/')")
      Xiki.def("lock+", :eval=>"ControlLock.toggle")
      Xiki.def("tasks+", :eval=>"Launcher.tasks")   # expand+
      Xiki.def("backward+", :eval=>"Move.backward_key")
      Xiki.def("forward+", :eval=>"Move.forward_key")
      Xiki.def("previous+", :eval=>"Move.previous")
      Xiki.def("next+", :eval=>"Move.next")
      Xiki.def("xpand+", :eval=>"Launcher.go")   # expand+

      # Pre-loading for caching done, don't do full-on el4r stuff...

      $el.define_key :isearch_mode_map, "\C-q", :isearch_quote_char   # This is necessary so "C-s C-q" won't quit
      $el.define_key :minibuffer_local_map, "\C-q", :quoted_insert   # So C-q still quotes control chars in the minibuffer
      Keys.set("\e\e"){ ControlTab.go; ControlTab.go }   # Make two quick escapes switch to the last view
      Keys.set("C-k"){ Keys.expand [] }
      Keys.set("C-."){ Keys.repeat }   # Repeat last command typed (like vim "."), except for trivial ones
      Keys.set("C-,"){ Keys.repeat :movement=>1 }   # Repeat last command typed (like vim "."), except for trivial ones
      # $el.define_key(:global_map, $el.kbd("C-;")){ Bookmarks.go }
      $el.define_key(:global_map, $el.kbd("C-;")){ Launcher.open("hop to file/") }

      Keys.set("C-'") { Keys.timed_insert }
      Keys.set("C-\\"){ ControlTab.go }

      $el.define_key :global_map, $el.kbd("C-_"), :negative_argument   # For terminals
      $el.define_key :global_map, $el.kbd("C-z"), :undo

      $el.define_key :lisp_interaction_mode_map, $el.kbd("C-;"), nil

      # if $el.locate_library "ruby-mode"
      #   $el.el_require :ruby_mode
      #   $el.message ""   # Keep it from showing junk at bottom
      #   #$el.define_key :ruby_mode_map, $el.kbd("C-j"), nil
      # end

      # Not using shell mode much any more
      # $el.el4r_lisp_eval("(require 'shell)")   # Unmap keys in modes that interfere
      # $el.define_key :shell_mode_map, $el.kbd("C-j"), nil   # shell-mode etc. special C-j shortcuts over-ride xiki

      # C-l in ediff mode
      $el.defun(:ediff_disable_C_l) { $el.define_key(:ediff_mode_map, $el.kbd("C-l"), nil) }
      $el.add_hook :ediff_keymap_setup_hook, :ediff_disable_C_l

      # Find alternative, since C-x is now expand+
      # xiki+1, xiki+2, etc (meaning C-x C-2)
      #       (1..9).each do |n|
      #         $el.define_key(:global_map, $el.kbd("C-x C-#{n}")){ Launcher.do_last_launch :nth=>n, :here=>1 }
      #       end

      # Make Ctrl+X be expand, and make cua-mode play nicely with it
      # $el.define_key(:global_map, $el.kbd("C-x C-@")){ Launcher.go }   # expand+
      # C+X, C+Space > do what C-x C-x used to do

      # Add a fallback keymap with "Ctrl+X <timeout>" mapped to kill-region.
      #   added where? : emulation-mode-map-alists

      $el.el4r_lisp_eval %`
        (progn
          (setq truncate-partial-width-windows nil)
          (set 'default-truncate-lines t)

          ; Add a keymap with "Ctrl+X" mapped to kill-region, that pre-empts cua--keymap-alist when there's a selection.
          (defvar xiki-cua--keymap-alist
           '((cua--ena-region-keymap keymap
             (24 . kill-region)
             )))

          (add-to-ordered-list 'emulation-mode-map-alists 'xiki-cua--keymap-alist 200)

          (defun xiki-launch () (interactive)
            (el4r-ruby-eval "Xiki::Launcher.go")
          )
        )
      `

      $el.define_key(:cua__cua_keys_keymap, "\C-x", :xiki_launch)   # expand+

      # $el.define_key(:global_map, $el.kbd("M-C-x")){ Launcher.go }

      # Meta+T for todo.notes
      # $el.define_key(:global_map, $el.kbd("M-t")){ View.open ":t" }

    end

    # Defines right-click
    def self.right_click

      # When not gui emacs, invoke what ctrl+T does...

      if ! Environment.gui_emacs

        $el.define_key(:global_map, $el.kbd("<mouse-3>"), $el.el_lambda(:interactive=>"e") {|e|
          $el.mouse_set_point(e)
          Launcher.tasks
        })

        return
      end

      $el.defun(:xiki_right_click, :interactive=>"e") do |e|
        if ! $el.elvar.mark_active
          $el.mouse_set_point(e)
          View.refresh
        end
        Launcher.right_click
      end

      $el.define_key(:global_map, $el.kbd("<mouse-3>"), :xiki_right_click)

      # In aquamacs, undefine the normal right-click menu...

      if $el.boundp(:osx_key_mode_map)
        $el.define_key(:osx_key_mode_map, $el.kbd("<down-mouse-3>"), nil)
        $el.define_key(:osx_key_mode_map, $el.kbd("<mouse-3>"), nil)

        return
      end

    end

  end
end
