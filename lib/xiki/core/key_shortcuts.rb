module Xiki
  class KeyShortcuts

    # Define all keys
    def self.keys

      Menu.init

      self.window_keys
      self.list_keys

      self.as_keys
      self.enter_keys

      self.hop_keys
      self.content_keys

      self.do_keys
      self.run_keys

      self.jump_keys   # time > (0.073769)
      self.misc   # time > (0.0451)

    end

    def self.as_keys

      # This breaks > probably unset C-a first, so we can go back to this

      Xiki.def("as+axis"){ Move.hop_left_key }
      Xiki.def("as+idea"){ Notes.as_task }
      Xiki.def("as+line"){ Clipboard.as_line }

      Xiki.def("as+before"){ Clipboard.copy_paragraph(:before=>true) }
      Xiki.def("as+under"){ Clipboard.copy_paragraph(:rest=>1) }
      Xiki.def("as+following"){ Clipboard.copy_rest_of_line }


      Xiki.def("as+directory"){ FileTree.copy_path }   # copy dir to clipboard from tree

      Xiki.def("as+task"){ View.beep '- changed to as+note!' }
      Xiki.def("as+note"){ Notes.as_task }
      Xiki.def("as+quote"){ Notes.as_task :link=>1 }

      Xiki.def("as+remembered"){ Location.as_spot }   # remember point in file
      Xiki.def("as+select"){ Clipboard.select }
      Xiki.def("as+version"){ History.backup_file }   # creates backup
      Xiki.def("as+macro"){ Macros.record }   # start recording macro

      Xiki.def("as+open"){ Launcher.as_open }
      Xiki.def("as+everything"){ View.beep '- changed to > content+all!' }
      Xiki.def("as+you"){ Clipboard.as_thing }   # copy object / symbol at point

      # as+N > Open nth quote in :t
      # shold do this instead? do+N > execute nth label in current page?
      (0..9).each do |n|
        Xiki.def("as+#{n}"){ View.to_nth_fraction n }
      end
    end

    def self.list_keys
      # O: open...
      # Use O prefix for: opening, jumping to files

      Xiki.def("list+xiki", :noob=>1){ Launcher.open "xiki/" }
      Xiki.def("list+search", :noob=>1){ Ol "Todo > Perform sync first!"; Launcher.open "xiki/" }

      Xiki.def("list+history"){ Launcher.open("history/") }

      Xiki.def("list+diffs", :noob=>1){ DiffLog.open }   # shows diffs of what you've edited
      Xiki.def("list+changes"){ DiffLog.show_edits }   # Diffs in particular file

      Xiki.def("list+interactions", :noob=>1){ Launcher.open "interactions/" }

      Xiki.def("list+views", :noob=>1){ Launcher.open("views/") }
      Xiki.def("list+recent", "recent/")
      Xiki.def("list+edited"){ Files.open_edited }   # show recently edited files
      Xiki.def("list+bookmarks", :noob=>1){ Launcher.open "bookmarks/" }

      Xiki.def("list+trail"){ Code.do_list_ancestors }

      Xiki.def("list+unsaved"){ DiffLog.compare_with_saved }

      Xiki.def("list+notes", :noob=>1){ Launcher.open("notes") }
      Xiki.def("list+links", :noob=>1){ Launcher.open("links/") }
      Xiki.def("list+key"){ FileTree.hop_file }
      Xiki.def("list+files", :noob=>1){ Launcher.open("files/") }

      Xiki.def("list+quick"){ Launcher.open "quick topics/" }
      Xiki.def("list+markers", :noob=>1){ Launcher.open("#{View.file}\n  = markers/", :bar_is_fine=>1) }
      Xiki.def("list+outline", :noob=>1){ FileTree.to_outline }
      Xiki.def("list+appointments", :noob=>1){ Launcher.open("~/xiki/notes.xiki\n  - ##^> 201/", :bar_is_fine=>1) }   # Appointments and other important tasks

      # G: leave unmapped for escape

      Xiki.def("list+preferences+bar"){
        $el.elvar.xiki_bar_hidden = $el.elvar.xiki_bar_hidden ? nil : true
      }
      Xiki.def("list+preferences+name"){ Buffers.rename }
      Xiki.def("list+preferences+coloring"){ Styles.toggle }


      Xiki.def("list+wrap"){ Block.do_as_wrap }   # Wrap lines in paragraph

      Xiki.def("list+go"){ Launcher.open("jump to dir/") }

      Xiki.def("list+just+methods"){ Launcher.open("links/methods/") }
      Xiki.def("list+just+routes"){ Launcher.open("links/routes/") }
      Xiki.def("list+just+outlog"){ Launcher.open("links/outlog/") }
      Xiki.def("list+just+comments"){ Launcher.open("links/comments/") }
      Xiki.def("list+just+xiki"){ Launcher.open("links/xiki/") }

      # list+N > Open nth quote in :n
      (0..9).each do |n|
        Xiki.def("list+#{n}"){ Links.to_nth_file n }
      end
    end


    def self.enter_keys
      # E: enter...
      # Use E prefix for: inserting

      Xiki.def("enter+end"){ Move.hop_right_key }

      Xiki.def("enter+bullet"){ Notes.bullet }
      Xiki.def("enter+comment"){ Code.enter_insert_comment }

      Xiki.def("enter+space"){ Code.enter_whitespace }

      Xiki.def("enter+line"){ View.insert_line }
      Xiki.def("enter+under"){ Notes.enter_junior }
      Xiki.def("enter+web"){ Search.enter_insert_search }
      Xiki.def("enter+marker"){ Notes.enter_note }


      Xiki.def("enter+key"){ Keys.insert_code }

      Xiki.def("enter+heading"){ Notes.insert_heading }

      Xiki.def("enter+note"){ Tasks.enter_upper }
      Xiki.def("enter+file"){ Files.enter_file }   # in tree, enter methods or headings
      Xiki.def("enter+directory"){ Files.enter_file :expand=>1 }
      Xiki.def("enter+outline"){ Launcher.enter_outline }   # in tree, enter methods or headings

      Xiki.def("enter+xiki"){ Launcher.open_topic :insert=>1 }

      Xiki.def("enter+quote"){ FileTree.enter_quote }
      Xiki.def("enter+value"){ FileTree.enter_quote nil, :char=>":" }

      Xiki.def("enter+prompt"){ View.insert_shell_prompt }
      Xiki.def("enter+group"){ $el.cua_set_rectangle_mark }

      Xiki.def("enter+after"){ Tasks.enter_after }

      Xiki.def("enter+item+time"){ Line.insert_time }
      Xiki.def("enter+item+date"){ Line.insert_date }

      Xiki.def("enter+item+replacement"){ Search.insert_before_and_after }
      Xiki.def("enter+item+new"){ DiffLog.enter_new }           # Enter Old: enter newly-deleted from last save > diff
      Xiki.def("enter+item+old"){ DiffLog.enter_old }   # Enter Old: enter newly-deleted from last save > diff

      # Todo > restore elsewhere
      Xiki.def("enter+item+eval"){ code = Keys.input(:prompt=>"Enter ruby code to eval and insert results: "); View.insert(eval(code).to_s)}
      Xiki.def("enter+item+javascript"){ Firefox.enter_log_javascript_line }   # Xiki.def("enter+log+javascript"){ Firefox.enter_log_javascript_line }

      Xiki.def("enter+item+log"){ Code.enter_log_line }   # Xiki.def("enter+log+line"){ Code.enter_log_line }
      Xiki.def("enter+item+important"){ Code.enter_log_line :exclamation=>1 }   # Xiki.def("enter+log+line"){ Code.enter_log_line }
      Xiki.def("enter+item+stack"){ Code.enter_log_stack }   # Xiki.def("enter+log+stack"){ Code.enter_log_stack }

      Xiki.def("enter+item+continue"){ Code.enter_log_line :txt=>"continue here!!" }   # Xiki.def("enter+log+line"){ Code.enter_log_line }
      Xiki.def("enter+item+performance"){ Code.enter_log_time }   # Xiki.def("enter+log+time"){ Code.enter_log_time }

      # enter+N > Go to nth heading
      (1..9).each do |n|
        Xiki.def("enter+#{n}"){
          View.to_highest
          (n-1).times do
            Notes.to_block :key=>1
          end
          View.recenter_top_key
          ""
        }
      end

    end


    def self.content_keys

      Xiki.def("content+save", :noob=>1){ DiffLog.save }
      Xiki.def("content+web", :noob=>1){ XikihubClient.web }
      Xiki.def("content+close", :noob=>1){ View.kill }

      Xiki.def("content+untitled", :noob=>1){ View.new_file }
      Xiki.def("content+open", :noob=>1){ View.open Keys.input(:prompt=>"File to open: ") }
      Xiki.def("content+directory"){ Bookmarks.go nil, :date_sort=>1 }
      Xiki.def("content+prompt", :noob=>1){ Launcher.open_prompt }
      Xiki.def("content+revert"){ Files.revert :from_key_shortcut=>1 }   # Revert file

      Xiki.def("content+filter", :noob=>1){ Search.outline_search }
      Xiki.def("content+jump"){ Move.to_line }   # Goto nth line in file

      Xiki.def("content+notes", :noob=>1){ Notes.open_todo }   # Open notes.xiki
      Xiki.def("content+links", :noob=>1){ Notes.open_todo :bookmark=>"%links" }   # Open links.xiki

      Xiki.def("content+highlight"){ View.highlight }
      Xiki.def("content+all"){ Clipboard.copy_everything }
      Xiki.def("content+group"){ Clipboard.copy_paragraph }   # copy paragraph
      Xiki.def("content+items"){ CodeTree.as_indented }

      Xiki.def("content+tasks", :noob=>1){ Launcher.tasks_menu_on_bookmark :bm=>"." }

      Xiki.def("content+bookmark", :noob=>1){ Bookmarks.save }
      Xiki.def("content+keys"){ Xiki::View.toggle_bar_mode }
      Xiki.def("content+zoom", :noob=>1){ CodeTree.kill_siblings :cross_blank_lines=>1 }   # kill adjacent lines at same indent as this one

      Xiki.def("content+editor"){ Grab.content_editor }

      Xiki.def("content+yours"){ View.open "%y", :stay_in_bar=>1 }
      Xiki.def("content+memorize", :noob=>1){ View.open "~/xiki/memorize.xiki" }
      Xiki.def("content+quick", :noob=>1){ View.open "%q" }

      Xiki.def("content+xiki"){ Launcher.open_topic }

      Xiki.def("content+views"){ View.link_views }


      # content+N > Go to nth fraction of the screen (ie content+5 to jump to middle)
      (1..9).each do |n|
        Xiki.def("content+#{n}"){ Files.open_nth n }
      end

    end


    def self.run_keys
      # D: do...
      # Use D prefix for: things that modify text or execute code

      Xiki.def("run+recent"){ Shell.recent_history_external nil, :from_key_shortcut=>1 }
      Xiki.def("run+macro"){ Macros.run }   # do last macro
      Xiki.def("run+note"){ Launcher.do_task }
      Xiki.def("run+up"){ Launcher.do_last_launch :here=>1 }

      Xiki.def("run+indent"){ Code.indent_to }
      # Xiki.def("run+comment"){ Code.comment }
      Xiki.def("run+eval"){ Code.run }   # run code as ruby
      Xiki.def("run+selected"){ Clipboard.select; View.deselect }


      Xiki.def("run+key"){ Keys.jump_to_code }

      Xiki.def("run+highlight+red"){ Color.mark "red" }
      Xiki.def("run+highlight+orange"){ Color.mark "orange" }
      Xiki.def("run+highlight+yellow"){ Color.mark "yellow" }
      Xiki.def("run+highlight+green"){ Color.mark "green" }
      Xiki.def("run+highlight+blue"){ Color.mark "blue" }
      Xiki.def("run+highlight+light"){ Color.mark "light" }
      Xiki.def("run+highlight+white"){ Color.mark "white" }

      Xiki.def("run+highlight+show"){ Color.show }
      Xiki.def("run+highlight+all", "highlight/all/")

      Xiki.def("run+highlight+next"){ Color.next_key }
      Xiki.def("run+highlight+previous"){ Color.previous_key }

      Xiki.def("run+highlight+delete"){ Color.delete }
      Xiki.def("run+highlight+clear"){ Color.clear; Color.clear_light }

      # Restore? > shows options on a bookmarked file

      Xiki.def("run+outline"){ History.open_current :outline=>true, :prompt_for_bookmark=>true }

      Xiki.def("run+query"){ Search.query_replace }   # do query replace


      Xiki.def("run+as+align"){ Code.do_code_align }   # Based on input character, all matches line up
      Xiki.def("run+as+execute"){ Shell.do_as_execute }   # Run shell command on tree
      Xiki.def("run+as+html"){ Firefox.do_as_html }
      Xiki.def("run+as+browser"){ Firefox.exec_block }
      Xiki.def("run+as+javascript"){ Javascript.run }
      Xiki.def("run+as+command"){ Command.do_as_menu }   # Grab item after '@' and run it by itself
      Xiki.def("run+as+outline"){ Command.do_as_outline }   # Grab item after '@' and run it by itself
      Xiki.def("run+as+python"){ Python.run }

      Xiki.def("run+as+space"){ Code.add_space }
      Xiki.def("run+as+test"){ Code.do_as_rspec }
      Xiki.def("run+as+wrap"){ View.beep 'Moved to > list+wrap' }
      Xiki.def("run+as+quote"){ Notes.do_as_quote }
      Xiki.def("run+as+xul"){ Firefox.do_as_xul }
      Xiki.def("run+as+deck"){ Deck.enable_arrow_keys }
      Xiki.def("run+as+remote"){ Remote.save_file }

      Xiki.def("run+delete+all"){ Effects.blink :what=>:all; View.kill_all }   # kill all text in buffer

      Xiki.def("run+delete+indented"){ CodeTree.do_kill_indented }
      Xiki.def("run+delete+siblings"){ View.beep 'Moved to > list+zoom' }   # List notes
      Xiki.def("run+delete+trailing"){ View.gsub!(/[ 	]+$/, "") }   # Deletes trailing whitespace

      Xiki.def("run+delete+unsaved"){ View.beep '- changed to content+zap!' }

      Xiki.def("run+delete+matching"){ Search.kill_matching }
      Xiki.def("run+delete+nonmatching"){ Search.kill_filter }

      Xiki.def("run+list+faces"){ Styles.list_faces }   # list
      Xiki.def("run+list+unsaved"){ Launcher.open("unsaved/") }

      Xiki.def("run+jump+up"){ FileTree.move_up }

      Xiki.def("run+forward"){ Line.move_word :forward }
      Xiki.def("run+back"){ Line.move_word :backward }


      Xiki.def("run+version+repository"){ Git.do_push }
      Xiki.def("run+version+log"){ Git.do_log }
      Xiki.def("run+version+diff"){ Git.do_compare_repository }
      Xiki.def("run+version+status"){ Git.do_status }

      Xiki.def("run+version+backups"){ History.list }   # Show all backed-up versions
      Xiki.def("run+version+changes"){ History.diff_with_backup }   # compare with last AV version

      Xiki.def("run+version+next"){ DiffLog.compare_views }

      # run+N > Run nth label in :t
      (0..9).each do |n|
        Xiki.def("run+#{n}"){ Launcher.do_last_launch :nth=>n }
      end

    end


    def self.hop_keys

      # Use H prefix for: moving cursor, jumping to specific points

      Xiki.def("hop+next"){ Move.to_next_paragraph }   # to next paragraph
      Xiki.def("hop+previous"){ Move.to_previous_paragraph :skip_if_top=>1 }   # to beginning of previous paragraph

      Xiki.def("hop+forward"){ Notes.to_block :key=>1 }
      Xiki.def("hop+back"){ Notes.to_block :up=>1, :key=>1 }


      Xiki.def("hop+to"){ Search.hop_to }

      Xiki.def("hop+edge"){ View.to_relative :line=>1 }   # go to nth line, relative to top of window (up+ for middle, up+up+ for bottom)
      Xiki.def("hop+aligned"){ View.to_relative }   # go to nth line, relative to top of window (up+ for middle, up+up+ for bottom)

      Xiki.def("hop+indent"){ Move.to_indent }
      Xiki.def("hop+side"){ View.content_right }
      Xiki.def("hop+last"){ Move.to_last_window(:blink=>true) }

      Xiki.def("hop+gap"){ Search.forward "\n\n\n+" }

      # hop+climb is a possible alternative
      Xiki.def("hop+up"){ Tree.to_parent :key=>1 }   # to parent (last line indented less)

      Xiki.def("hop+hack", :noob=>1){ Deletes.backward }
      Xiki.def("hop+down"){ Move.to_next_paragraph :prefix=>:uu }

      Xiki.def("hop+output"){ View.layout_outlog }
      Xiki.def("hop+marker"){ Notes.next_marker }   # go to nth line, relative to top of window (up+ for middle, up+up+ for bottom)
      Xiki.def("hop+clear"){ View.layout_outlog :prefix=>:u }

      Xiki.def("hop+remembered"){ Location.hop_remembered }

      Xiki.def("hop+kind"){ Move.to_other_bracket }   # to matching bracket, etc

      Xiki.def("hop+quote"){ Move.to_quote }   # move to next |... quote

      Xiki.def("hop+junior"){ Move.to_junior }   # go to nth line, relative to top of window (up+ for middle, up+up+ for bottom)
      Xiki.def("hop+words"){ Line.to_beginning }   # move to start of words on line
      # X
      # Z

      # hop+N > Go to nth visible paragraph (ie jump+5 to jump to the 5th paragraph)
      (1..9).each do |n|
        Xiki.def("hop+#{n}"){ View.to_nth_paragraph n }
      end

    end

    def self.do_keys
      # Use for: dealing with windows

      Xiki.def("do+delete"){ Deletes.forward }

      Xiki.def("do+file"){ FileTree.tree :recursive=>1 }
      Xiki.def("do+comment"){ Code.comment }
      Xiki.def("do+indent"){ Hide.hide_by_indent }   # only show lines indented less than x

      Xiki.def("do+next"){ Line.move :next }
      Xiki.def("do+previous"){ Line.move :previous }

      Xiki.def("do+kill"){ Line.delete_line_key }

      Xiki.def("do+selection+sort"){ Line.do_lines_sort }
      Xiki.def("do+selection+reverse"){ $el.reverse_region($el.region_beginning, $el.region_end) }
      Xiki.def("do+selection+unique"){ Code.kill_duplicates }   # Uniqify, delete duplicates
      Xiki.def("do+selection+jumble"){ Code.randomize_lines }   # Shuffle lines

      Xiki.def("do+selection+having"){   # delete lines matching a regex
        unless $el.elvar.current_prefix_arg
          $el.delete_matching_lines( Keys.input(:prompt => "Delete lines having: ") )
        else
          $el.delete_non_matching_lines( Keys.input(:prompt => "Delete lines not having: ") )
        end
      }
      Xiki.def("do+selection+linux"){ $el.set_buffer_file_coding_system :unix }   # Use unix linebreaks
      Xiki.def("do+selection+windows"){ $el.set_buffer_file_coding_system :dos }

      Xiki.def("do+open+browser"){ Browser.open_in_browser }
      Xiki.def("do+open+left"){ View.open_in_bar }
      Xiki.def("do+open+os"){ Files.open_in_os }
      Xiki.def("do+open+window"){ Files.open_in_window }   # Expose file in OS folder

      # Do > restore? > prompts for bookmark to show edits for

      Xiki.def("do+marker"){ Notes.open_todo; Notes.next_marker :nth=>1 }
      Xiki.def("do+repeat"){ Line.duplicate }

      Xiki.def("do+view"){
        View.open Keys.input(:prompt=>"name of the view: ", :timed=>1)
        # , :txt=>"\n\n\n\n"
        Notes.mode
      }   # Make view with specific name

      Xiki.def("do+quote"){ Launcher.open("views/", :task=>["quoted"]) }
      Xiki.def("do+update+numbers"){ View.beep "changed to > tasks+reoder" }
      Xiki.def("do+highlights"){ OlHelper.highlight_executed_lines }

      Xiki.def("do+word"){ View.selection = $el.bounds_of_thing_at_point(:word).to_a }

      Xiki.def("do+align+left"){ X "iterm/position/left" }
      Xiki.def("do+align+right"){ X "iterm/position/right" }
      Xiki.def("do+align+top"){ X "iterm/position/top" }
      Xiki.def("do+align+bottom"){ X "iterm/position/bottom" }

      Xiki.def("do+align+full"){ X "iterm/position/full" }

      Xiki.def("do+align+west"){ X "iterm/position/left edge" }
      Xiki.def("do+align+east"){ X "iterm/position/right edge" }
      Xiki.def("do+align+north"){ X "iterm/position/top edge" }
      Xiki.def("do+align+south"){ X "iterm/position/bottom edge" }


      Xiki.def("do+align+detached"){ X "iterm/position/square" }

      Xiki.def("do+align+margin"){ X "iterm/position/margin" }

      Xiki.def("do+align+1"){ X "iterm/position/corner 1" }
      Xiki.def("do+align+2"){ X "iterm/position/corner 2" }
      Xiki.def("do+align+3"){ X "iterm/position/corner 3" }
      Xiki.def("do+align+4"){ X "iterm/position/corner 4" }



      Xiki.def("do+browser+reload"){ Browser.reload }
      Xiki.def("do+browser+url"){ View.<< Browser.url }


      Xiki.def("do+zoom"){ View.zoom }   # show selection only

      Xiki.def("do+xiki+methods"){ Launcher.open("#{Xiki.dir}\n  - ##^ *def /") }
      Xiki.def("do+xiki+javascript"){ Launcher.open("#{Xiki.dir}\n  - ##^ *function /") }
      Xiki.def("do+xiki+files"){ FileTree.tree :recursive=>1, :bm=>"xiki" }
      Xiki.def("do+xiki+keys"){ Launcher.open("#{Xiki.dir}lib/xiki/core/key_shortcuts.rb\n  - ##^ *Xiki.def..\\w+\\+/") }
      Xiki.def("do+xiki+tests"){ Launcher.open("#{Xiki.dir}spec/\n  - ##^ *(describe|it) /") }
      Xiki.def("do+xiki+comments"){ Launcher.open("#{Xiki.dir}\n  - ##^ *[#;] /") }
      Xiki.def("do+xiki+here"){ Launcher.open("#{Xiki.dir}\n  - ##^ *Ol \"(continue|test) here/") }
      Xiki.def("do+xiki+later"){ Launcher.open("#{Xiki.dir}\n  - ##^ *Ol \"continue later/") }
      Xiki.def("do+xiki+notes"){ Launcher.open("xiki/") }

      # Todo > do+1, do+2 > like open+N, but take from :t

      # do+N > Run nth label in current file
      (1..9).each do |n|
        Xiki.def("do+#{n}"){ Launcher.do_last_launch :nth=>n, :here=>1 }
      end

    end


    def self.window_keys
      # Use for: adjusting the layout, changing what is visible

      Xiki.def("window+close", :noob=>1){ View.hide :kill_if_only=>1 }

      Xiki.def("window+top", :noob=>1){ View.to_highest }
      Xiki.def("window+bottom", :noob=>1){ View.to_bottom }
      Xiki.def("window+up", :noob=>1){ View.page_up }
      Xiki.def("window+down", :noob=>1){ View.page_down }

      Xiki.def("window+link", :noob=>1){ Notes.as_file }   # Save in :n
      Xiki.def("window+quote"){ Notes.as_file :prompt_for_label=>1 }   # Save in :n, with note

      Xiki.def("window+horizontal"){ View.create_horizontal }
      Xiki.def("window+vertical"){ View.create_vertical }
      Xiki.def("window+next"){ View.next }
      Xiki.def("window+previous"){ View.previous }

      Xiki.def("window+align"){ View.recenter }   # LL - recenter (L's emacs default)
      Xiki.def("window+edge"){ View.recenter_top_key }
      Xiki.def("window+wrap", :noob=>1){ $el.toggle_truncate_lines; View.message "" }   # wrap lines

      Xiki.def("window+ide", :noob=>1){ View.layout_todo_and_nav }

      Xiki.def("window+justify"){ View.balance :narrow_bar=>1 }
      Xiki.def("window+rebalance"){ View.balance }
      Xiki.def("window+full"){ View.hide_others }
      Xiki.def("window+zoom"){ View.enlarge }

      Xiki.def("window+go"){ $el.save_place_kill_emacs_hook; DiffLog.save_go_location }   # Used to be > content+go

      Xiki.def("window+open"){ Launcher.as_open }

      Xiki.def("window+markers"){ Move.to_window(1); Launcher.open('note markers/', :bar_is_fine=>1) }
      Xiki.def("window+search", :noob=>1){ View.open :txt=>"Todo ^S on the current file > to show shared search results" }

      Xiki.def("window+xiki"){ View.open :txt=>"Todo ^X on the current file > to show notes > Borrow > content+tasks" }

      Xiki.def("window+1"){ Move.to_window(1) }
      Xiki.def("window+2"){ Move.to_window(2) }
      Xiki.def("window+3"){ Move.to_window(3) };  Xiki.def("window+4"){ Move.to_window(4) }
      Xiki.def("window+5"){ Move.to_window(5) };  Xiki.def("window+6"){ Move.to_window(6) };  Xiki.def("window+7"){ Move.to_window(7) };  Xiki.def("window+8"){ Move.to_window(8) }
      Xiki.def("window+9"){ Move.to_last_window(:blink=>1) }
      Xiki.def("window+0"){ View.recenter_top }   # Layout 0: scroll so cursor is 0 lines from top of window

    end


    def self.jump_keys

      Code.cache(:search_keys) do


        # jump+ keys > doing things while searching...

        $el.define_key :isearch_mode_map, "\\C-h", nil
        $el.define_key :isearch_mode_map, "\\C-t", nil
        $el.define_key :isearch_mode_map, "\\C-s", nil

        Xiki.def("jump+copy", :eval=>"Search.isearch_copy")

        Xiki.def("jump+kill", :eval=>"Search.isearch_clear")   # cut (or search at point of last cut, if no search)

        Xiki.def("jump+go", :eval=>"Search.isearch_go")   # cut (or search at point of last cut, if no search)

        Xiki.def("jump+value", :eval=>"Search.insert_at_search_start")   # Value: copy value back to search start
        Xiki.def("jump+pull", :eval=>"Search.isearch_pull")   # Pull back to search start (or jump+push if nothing searched for yet)
        Xiki.def("jump+zap", :eval=>"Search.zap")   # zap - delete up until search start

        Xiki.def("jump+open", :eval=>"Search.xiki")   # To: open file / jump to method
        Xiki.def("jump+notes", :eval=>"Search.isearch_tasks")
        Xiki.def("jump+links", :eval=>"Search.isearch_links")   # isearch for this string in :n

        Xiki.def("jump+files", :eval=>"Search.bookmark")   # when no search, prompt for input

        Xiki.def("jump+exchange", :eval=>"Search.isearch_query_replace")   # Outline (or jump+outlog if nothing searched for yet)
        # Jump+i > available

        Xiki.def("jump+usurp", :eval=>"Search.isearch_pull_in_sexp")   # usurp: pull sexp into search string

        Xiki.def("jump+before", :eval=>"Search.isearch_or_copy('1')")
        Xiki.def("jump+after", :eval=>"Search.isearch_or_copy('2')")

        Xiki.def("jump+diffs", :eval=>"Search.isearch_diffs")   # Delete (or jump+difflog if no search)

        # Todo > define > make it do what isearch-yank-line
        #   - but > define here so it'll show up in menu

        Xiki.def("jump+have+case", :eval=>"Search.isearch_have_case")
        Xiki.def("jump+have+edges", :eval=>"Search.just_edges")   # Delete everything but chars at edges of match

        Xiki.def("jump+have+line", :eval=>"Search.have_line")   # Line wrap without exiting isearch
        Xiki.def("jump+have+remembered", :eval=>"Search.insert_at_spot")
        Xiki.def("jump+have+javascript", :eval=>"Search.isearch_have_outlog_javascript")

        # Todo > shuffle to new key

        Xiki.def("jump+have+move", :eval=>"Search.isearch_move_line")   # Move line to where search started
        Xiki.def("jump+have+output", :eval=>"Search.isearch_have_outlog")

        Xiki.def("jump+have+text", :eval=>"Search.insert_at_search_start :prepend=>'Ol.a '")
        Xiki.def("jump+have+next", :eval=>"Search.isearch_move_to '%n', :insert_after=>1")

        Xiki.def("jump+have+path", :eval=>"Search.isearch_move_to '%n', :include_file_context=>1")
        Xiki.def("jump+have+variable", :eval=>"Search.insert_var_at_search_start")

        Xiki.def("jump+have+web", :eval=>"Search.isearch_move_to '%n', :prepend=>\"google/\n  \"")
        Xiki.def("jump+have+go", :eval=>"Search.have_go")

        Xiki.def("jump+have+filter", :eval=>"Search.isearch_just_search")   # Add "##match/" line in current tree

        Xiki.def("jump+have+search", :eval=>"Search.insert_at_search_start :prepend=>'##'")
        Xiki.def("jump+have+diffs", :eval=>"Search.isearch_move_to '%n', :diffs=>1")
        Xiki.def("jump+have+after", :eval=>"Search.isearch_move_to '%n', :prepend=>':+'")
        Xiki.def("jump+have+before", :eval=>"Search.isearch_move_to '%n', :prepend=>':-'")
        Xiki.def("jump+have+yellow", :eval=>"Search.isearch_move_to '%n', :prepend=>':?'")

        # Just so it's consistent with ^H^H when deleting selection
        Xiki.def("jump+have+hit", :eval=>"Search.isearch_clear")
        Xiki.def("jump+have+quote", :eval=>"Search.isearch_move_to '%links', :prompt_label=>1")

        # I: leave unmapped - had issues using it (messes up position)
        Xiki.def("jump+to+after", :eval=>"Search.query_replace_with_2")
        Xiki.def("jump+to+case", :eval=>"Search.isearch_just_case")   # make match be camel case
        Xiki.def("jump+to+delete", :eval=>"Search.like_delete")   # Filter > Delete lines that contain the match, between here and end of paragraph

        Xiki.def("jump+to+top", :eval=>"Search.isearch_restart :top")   # Restart search at top
        Xiki.def("jump+to+edge", :eval=>"Search.isearch_restart :edge")   # Restart search at top


        Xiki.def("jump+to+kill", :eval=>"Search.just_kill")

        Xiki.def("jump+to+integer", :eval=>"Search.stop; Search.isearch '[0-9][0-9.]*', :regex=>1")
        Xiki.def("jump+to+links", :eval=>"Search.isearch_move_to '%links'")
        Xiki.def("jump+to+notes", :eval=>"Search.isearch_move_to '%n'")

        Xiki.def("jump+to+search", :eval=>"Search.isearch_move_to '%n', :prepend=>'##', :as_regex=>1")   # Swap the match with what's in the clipboard (put the match where the search started)        Xiki.def("jump+to+search", :eval=>"Search.search_just_swap")   # Swap the match with what's in the clipboard (put the match where the search started)

        Xiki.def("jump+to+reverse", :eval=>"Search.search_just_swap")   # Swap the match with what's in the clipboard (put the match where the search started)

        Xiki.def("jump+to+order", :eval=>"Search.isearch_just_adjust")   # Swap/toggle the two characters in match
        Xiki.def("jump+to+variable", :eval=>"Search.isearch_just_surround_with_char '\#{', '}'")
        Xiki.def("jump+to+push", :eval=>"Git.search_just_push")
        Xiki.def("jump+to+bullet", :eval=>"Search.have_label")

        Xiki.def("jump+to+quote", :eval=>"Search.isearch_google :quote=>true")

        Xiki.def("jump+to+web", :eval=>"Search.isearch_google :quote=>true")

        Xiki.def("jump+to+highlight+this", :eval=>"Search.just_orange")   # Highlight just match as orange
        # Todo > implement these
        Xiki.def("jump+to+highlight+all", :eval=>"Search.highlight_all_found")   # Highlight all matches
        Xiki.def("jump+to+highlight+lines", :eval=>"Search.highlight_matching_lines")   # Highlight all lines that match
        # jump+to+y < available


        # jump+show+ keys...
        # Alternative words considered > see send set show save share snatch seek


        Xiki.def("jump+show+command", :eval=>"Launcher.search_like_menu")
        Xiki.def("jump+show+difflog", :eval=>"Search.jump_to_difflog")   # find last string in difflog
        Xiki.def("jump+show+filename", :eval=>"Search.isearch_open")   # Open match as filename

        Xiki.def("jump+show+links", :eval=>"Search.isearch_restart '%links', :as_here=>1")
        Xiki.def("jump+show+parens", :eval=>"Search.isearch_just_surround_with_char '(', ')'")   # When search match
        Xiki.def("jump+show+right", :eval=>"Search.isearch_restart :right")   # Search in top-right view
        Xiki.def("jump+show+brackets", :eval=>"Search.isearch_just_surround_with_char '[', ']'")
        Xiki.def("jump+show+special", :eval=>"Search.isearch_just_special")   # Jump to next special character



        Xiki.def("jump+show+more", :eval=>"Search.recenter_to_top")
        Xiki.def("jump+show+output", :eval=>"Search.isearch_restart '%o'")
        Xiki.def("jump+show+quote", :eval=>"Search.isearch_just_surround_with_char '\"'")

        Xiki.def("jump+show+after", :eval=>"Search.isearch_just_after")

        Xiki.def("jump+show+expanded", :eval=>"Search.like_expanded")
        Xiki.def("jump+show+notes", :eval=>"Search.isearch_restart '%n', :as_here=>1")

        # Todo > restore?
        # Xiki.def("jump+show+variable", :eval=>"Search.just_name")
        # Xiki.def("jump+show+variable", :eval=>"Search.isearch_just_surround_with_char '\#{', '}'")

        Xiki.def("jump+show+web", :eval=>"Search.isearch_google")   # make match be snake case
        Xiki.def("jump+show+xiki", :eval=>"View.open \"%xiki/\#{Search.stop.strip}\"")

      end

      # Pre-loading for caching done, don't do full-on el4r stuff...

      return if $el.caching

      $el.define_key(:isearch_mode_map, $el.kbd("C-t C-]")) { Search.just_increment }   # jump+to+Plus > alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-t C-_")) { Search.just_increment(:decrement=>true) }   # jump+to+Minus > alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-_")) { Search.subtract }   # Remove one char from isearch > alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-]")) { $el.isearch_yank_char }     # alternative for terminal
      $el.define_key(:isearch_mode_map, $el.kbd("C-\\"), :isearch_abort_really)

      # $el.define_key(:isearch_mode_map, "\C-l", :isearch_repeat_forward)
      $el.define_key(:isearch_mode_map, "\C-j", :isearch_repeat_forward)

      $el.define_key(:isearch_mode_map, "\e"){ Search.cancel }   # Escape

      $el.el4r_lisp_eval %`
        (progn

          ; Has to be defined in lisp, since passing C-@ fails (it's null, and that's what we use as a delimiter)
          ; Definition for jump+Space
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

    def self.misc

      Xiki.def("search+", :eval=>"XikihubClient.search_key")

      Xiki.def("go+", :eval=>"Grab.go_key")
      Xiki.def("tasks+", :eval=>"Launcher.options_key")

      Xiki.def("xiki+", :eval=>"Topic.xiki_key")

      Xiki.def("quit+", :noob=>1, :eval=>"DiffLog.quit")
      Xiki.def("backward+", :eval=>"Move.backward_key")
      Xiki.def("forward+", :eval=>"Move.forward_key")

      Xiki.def("previous+", :eval=>"Move.previous")
      Xiki.def("next+", :eval=>"Move.next")

      Keys.set("<up>"){ Move.previous }
      Keys.set("<down>"){ Move.next }
      Xiki.def("open+", :eval=>"Launcher.launch_or_collapse")   # expand+ > Ctrl+x

      Keys.set("<M-RET>") { Launcher.launch_or_collapse }   # Meta+Return, Alt+Return, Option+Return


      # Enable key that 'xiki' dictation command is mapped to
      Keys.set("M-X"){ Voice.launch }   # Option+Shift+X

      Keys.set("M-E"){ View.expose }   # Option+Shift+E
      Keys.set("M-N"){ X "content/notes" }   # Option+Shift+N
      Keys.set("M-L"){ X "content/links" }   # Option+Shift+L
      Keys.set("M-C"){ X "content/close" }   # Option+Shift+L


      # Pre-loading for caching done, don't do full-on el4r stuff...

      $el.define_key :isearch_mode_map, "\C-q", :isearch_quote_char   # This is necessary so "C-s C-q" won't quit
      $el.define_key :minibuffer_local_map, "\C-q", :quoted_insert   # So C-q still quotes control chars in the minibuffer

      # Right-clicking in mode-line do "expose linked"

      # Clicking in minibuffer > map it to something else > isearch?
      if $el.boundp(:minibuffer_inactive_mode_map)
        $el.define_key(:minibuffer_inactive_mode_map, $el.kbd("<mouse-1>"), :isearch_forward)
      end


      # Two escapes in a row > show currently open views
      Keys.set("\e\e"){ View.expose }   # Make two quick escapes switch to the last view
      Keys.set("C-k"){ Keys.expand [] }

      # Enabling control lock > ^U > up+, uplift+, uniform+, uplifted+, umbrella+, utility
      Keys.set("C-u"){ ControlLock.toggle }

      # Maybe swap these > C-- for negative > and > C-] for universal

      $el.define_key(:global_map, $el.kbd("M--"), :universal_argument)   # Required when control lock off

      # Freaking beeps > when not in control lock
      $el.define_key(:global_map, $el.kbd("C--"), :universal_argument)   # Works when control lock on
      $el.define_key(:global_map, $el.kbd("C-_"), :universal_argument)   # Required when control lock off
      #$# Is this Ctrl+/? > C-/?
      $el.define_key(:global_map, $el.kbd("C-]"), :negative_argument)

      $el.define_key(:global_map, $el.kbd("C-;")){ Launcher.open("files/") }   # ^', so show files from :n
      $el.define_key(:global_map, $el.kbd("C-'")){ Keys.timed_insert }

      Keys.set("C-."){ Keys.repeat }   # Repeat last command typed (like vim "."), except for trivial ones
      Keys.set("C-,"){ Keys.repeat :movement=>1 }   # Repeat last command typed (like vim "."), except for trivial ones

      Keys.set("\eb"){ Move.backward_word_key }   # Option+E > M-e
      Keys.set("\ef"){ Move.forward_word_key }   # Option+F > M-f

      Keys.set("\ew"){ Move.backward_delete_word }   # Option+W > M-w
      Keys.set("\ed"){ Move.forward_delete_word }   # Option+W > M-w

      Keys.set("\en"){ Move.next :prefix=>:u }   # Option+N > M-n
      Keys.set("\ep"){ Move.previous :prefix=>:u }   # Option+P > M-p

      Keys.set("C-\\"){ Keys.docs }

      $el.define_key :global_map, $el.kbd("C-z"), :undo

      $el.define_key :lisp_interaction_mode_map, $el.kbd("C-;"), nil   # Make lisp mode not interfere

      # C-l in ediff mode
      $el.defun(:ediff_disable_C_l) { $el.define_key(:ediff_mode_map, $el.kbd("C-l"), nil) }
      $el.add_hook :ediff_keymap_setup_hook, :ediff_disable_C_l

      # Find alternative, since C-x is now expand+
      # xiki+1, xiki+2, etc (meaning C-x C-2)
      #       (1..9).each do |n|
      #         $el.define_key(:global_map, $el.kbd("C-x C-#{n}")){ Launcher.do_last_launch :nth=>n, :here=>1 }
      #       end

      # Make Ctrl+X be expand, and make cua-mode play nicely with it
      # C+X, C+Space > do what C-x C-x used to do

      # Add a fallback keymap with "Ctrl+X <timeout>" mapped to kill-region.
      #   added where? : emulation-mode-map-alists

      $el.el4r_lisp_eval %`
        (progn
          (setq truncate-partial-width-windows nil)
          ;(set 'default-truncate-lines t)

          ; Add a keymap with "Ctrl+X" mapped to kill-region, that pre-empts cua--keymap-alist when there's a selection.
          (defvar xiki-cua--keymap-alist
           '((cua--ena-region-keymap keymap
             (24 . kill-region)   ; ^X
             (3 . cua-copy-region)   ; ^C
             )))

          (add-to-ordered-list 'emulation-mode-map-alists 'xiki-cua--keymap-alist 200)

          (defun xiki-launch () (interactive) (el4r-ruby-eval "Xiki::Topic.xiki_key"))

          (defun xiki-choose () (interactive) (el4r-ruby-eval "Xiki::Keys.expand ['content']"))
        )
      `

      $el.define_key(:cua__cua_keys_keymap, "\C-x", :xiki_launch)   # expand+
      $el.define_key(:cua__cua_keys_keymap, "\C-c", :xiki_choose)   # expand+

      $el.define_key(:global_map, "\C-j", :isearch_forward)

      # Todo > automatically populate this in yours.rb > when new install
      # Define one key, so it's obvious they should use yours+ for user keys
      # Xiki.def("yours+foo"){ View.open :txt=>"> Keys starting with ^Y\nYou may want to define your own keys as something like yours+foo\n\nMeaning ^Y then ^X, where ^X is anything you want.\n\n> Example\nXiki.def('yours+hi'){ View << 'hi there'}   # ^Y ^H\n\n\n" }


    end

    # Defines right-click
    def self.right_click

      # When not gui emacs, invoke what ctrl+T does...

      # if ! Environment.gui_emacs

      # In case the terminal maps right-click to mouse-3
      $el.define_key(:global_map, $el.kbd("<mouse-3>"), $el.el_lambda(:interactive=>"e") {|e|
        $el.mouse_set_point(e)
        Launcher.options_key
      })

      # In case the terminal maps right-click to mouse-2
      $el.define_key(:global_map, $el.kbd("<mouse-2>"), $el.el_lambda(:interactive=>"e") {|e|
        $el.mouse_set_point(e)
        Launcher.options_key
      })

    end

  end
end
