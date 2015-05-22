require 'open3'
require "#{Xiki.dir}misc/libs/session.rb"

# Didn't quite fix it > try commenting this out?  Look at the docs
Session::use_open3=true

module Xiki
  class Shell

    @@log = File.expand_path("~/xiki/misc/logs/shell_log.notes")

    def self.menu
      # All of this is deprecated, I think?
      %`
      - .log/
      - .current/
      - .history/
      - api/
        > In Shell (asynchronously)
        =Shell.run "ls"
        =Shell.run "ls", :dir=>"/tmp"

        > Inline (synchronously)
        =Shell.sync "ls"
        =Shell.sync "ls", :dir=>"/etc"
      - docs/
        You can run shell commands by typing things like this...

        > In current dir
        =$ ls

        > In other dir
        =/tmp/
          $ ls

        > Async, in any open console view
        =/tmp/
          % ls

        > Async, in other dir
        =/tmp/
          % ls

        > Async, in iTerm
        =/tmp/
          & ls

        > Commands you've run recently
        << log/

        > Commands from currently open consoles
        << current/
      `
    end

    def self.log
      View.open @@log
    end

    # Run the command in a console
    def self.[] command
      self.run command, :sync=>1
    end

    # Delegates to .run :sync=>1
    # Shell.sync "pwd"
    # Shell.sync "pwd", :clean=>1   # Clean up stupid ^H sequences
    def self.command command, options={}
      self.sync command, options
    end

    def self.sync command, options={}
      txt = self.run command, options.merge(:sync=>1)
      txt.gsub!(/.\cH/, "") if options[:clean]
      txt = Tree.quote(txt, :char=>"|") if options[:quote]
      txt
    end

    # Shell.async "ls", :dir=>"/misc/"
    # Shell.async "ls", :buffer=>"the buffer"
    def self.async command, options={}
      self.run command, options
    end

    # Has a Session instance for each view, to keep shell commands isolated
    @@sessions ||= {}

    def self.sessions
      @@sessions
    end

    def self.session
      @@sessions[View.name]
    end

    #
    # Runs shell command asynchronously.
    #
    # Shell.run "ls"
    # Shell.run "ls", :dir=>"/tmp/"
    # Shell.run "ls", :buffer=>"the buffer"
    #
    def self.run command, options={}

      dir = options[:dir]
      sync = options[:sync]
      buffer = options[:buffer]
      reuse_buffer = options[:reuse_buffer]

      orig = Location.new if options[:dont_move]

      # Nil out dir if blank
      dir = nil if dir && dir.length == 0

      if dir
        dir = Bookmarks.expand(dir)
        # If relative dir, make current dir be on end of current
        dir = "#{$el.elvar.default_directory}/#{dir}" unless dir =~ /^\//
        dir = dir.gsub(/\/\/+/, '/')

        # If file, but not dir, try backing up to the dir
        raise "- Directory '#{dir}' doesn't exist!" if ! File.exists? dir

        dir.sub!(/[^\/]+$/, '') if ! File.directory?(dir)

        # If dir exists, continue
        if File.directory?(dir)
          # Put slash on end if not there
          dir = "#{dir}/" unless dir =~ /\/$/
        else  # Otherwise, exit
          return puts("#{dir} is not a dir")
        end

        # Leave it nil, so existing dir will be used
        # else
        #   dir = $el ? $el.elvar.default_directory : "/tmp/"
      end

      if sync
        # Run command synchronously (in place)...

        return command if options[:no_enter]

        env = options[:env] || {}

        # rbenv, so clear out env so shim will determine ruby version based on dir...

        if ENV["RBENV_VERSION"]

          # Get this working again? > way to set env vars? > maybe have to pass it in as a separate command? > will that affect the current session of the surrounding ruby?
          # =commit/rbenv > make rbenv ruby version be used in $ shell commands.

          # Blank out RBENV_VERSION version
          env["RBENV_VERSION"] = "" if ! env["RBENV_VERSION"]   # But don't interfere with the :env option setting it

          # Remove 1st item in path that has: '/.rbenv/versions/'
          path = ENV["PATH"].dup
          path.sub! /[^:]+\/.rbenv\/versions\/[^:]+:/, ""
          env["PATH"] = path
        end

        # Run actual command...

        # Called with :session_key, so use cached session...

        session_key = options[:session_key]
        if session_key

          session = @@sessions[session_key]

          if ! session
            session = @@sessions[session_key] = Session::Bash.new # (:track_history=>true)
            # Creating new session, so use dir of view by default
            session.execute("cd '#{View.dir}'")
          end

        else
          # Not called with :session_key, so use new session...
          session = Session::Bash.new #(:track_history=>true)
        end

        dir_orig = nil

        if dir
          # Remember old dir, if dir not set, to restore it
          dir_orig = session.execute("pwd")[0].strip if session_key
          session.execute("cd '#{dir}'")
        end

        stdout, stderr = options[:stdin] ?
          session.execute(command, :stdin=>options[:stdin]) :
          session.execute(command)

        # Change back to original dir if we have a session open
        if dir_orig && command !~ /^cd( |\z)/
          session.execute("cd '#{dir_orig}'")
        end

        # :raise_error flag, so just raise stderr...
        raise stderr if stderr.any? && options[:raise_error]

        # :return_error flag, so return stderr separately...

        return [stdout, stderr == "" ? nil : stderr] if options[:return_error]

        stdout += "\n> error\n#{stderr}" if stderr.any?
        return stdout

        # Todo > hook up stdin again!

      else

        # Run command asynchronously (in console)...

        if View.in_bar? and ! options[:dont_leave_bar]
          View.to_after_bar
        end
        buffer ||= "*console #{dir}"

        if ! reuse_buffer
          buffer = $el.generate_new_buffer(buffer)
        end
        View.to_buffer buffer
        $el.erase_buffer if reuse_buffer
        $el.elvar.default_directory = dir if dir
        $el.shell $el.current_buffer

        # Don't prompt with "buffer has a running process" when closing
        $el.set_process_query_on_exit_flag $el.get_buffer_process($el.current_buffer), nil

        Move.bottom
        if command  # If nil, just open console
          $el.insert command
          Shell.enter unless options[:no_enter] || options[:mock]
        end
      end

      orig.go if options[:dont_move]

      nil
    end

    def self.open dir=nil
      View.handle_bar
      dir ||= $el.elvar.default_directory
      dir = File.expand_path(dir)+"/"
      View.to_buffer $el.generate_new_buffer("*console #{dir}")
      raise "dir '#{dir}' doesn't exist" unless File.directory?(dir)
      $el.elvar.default_directory = dir
      $el.shell $el.current_buffer
    end

    def self.enter command=nil
      View.insert command if command

      begin
        $el.comint_send_input
      rescue
        #       Ol << "Shell.enter error here!"
      end
    end

    def self.console?
      View.mode == :shell_mode
    end

    # Switches to a shell buffer running in this dir. Only considers
    # visible buffers with prompts at the end, and cursor at end.
    # So it's smart enough to not switch to a shell with a command in
    # progress or not yet executed.
    # Shell.to_shell_buffer "/tmp/"
    def self.to_shell_buffer dir=nil, options={}
      if dir
        dir = "#{dir}/" unless dir =~ /\/$/
        pattern = /^\*console #{Regexp.quote(dir)}(<| |$)/
      else
        # If already in a shell (regardless of buffer name)
        return true if View.mode == :shell_mode
        pattern = /^\*console/
      end

      return true if View.name =~ pattern   # If already there, do nothing

      # No dir passed, so find avalable shell in 1st visible dir...

      if ! dir

        # Try to find visible shell buffer in same dir and with prompt

        View.list.each do |w|
          $el.set_buffer $el.window_buffer(w)
          next if View.mode != :shell_mode || ! Shell.prompt?
          next if View.cursor != View.bottom
          View.to_window(w)
          return true
        end
      end

      if dir

        # TODO Make sure there's no ssh or cd in history!
        View.list.each do |w|
          $el.set_buffer $el.window_buffer(w)
          next if View.mode != :shell_mode || ! Shell.prompt?

          next if View.name !~ pattern   # Require pattern match for remote and local

          if dir !~ /@/   # If local, also check that dir hasn't changed (can't do in remote)
            next if Tree.slashless(dir) != Tree.slashless(View.dir)
          end

          next if View.cursor != View.bottom

          View.to_window(w)
          return true
        end

      # TODO: implement similar finding a dir for remote?
        # else

      end


      if Keys.prefix_u(:clear=>true)

        found = Buffers.list.find do |b|
          name = Buffers.name b
          next false unless name =~ pattern

          view = nil
          $el.with(:save_window_excursion) do
            View.to_buffer name

            next false unless Shell.prompt?

            cd_dir = View.dir
            cd_dir = "#{cd_dir}/" unless cd_dir =~ /\/$/
            next false unless cd_dir == dir
            next false if Shell.commands.join("\n") =~ /^(ssh|ftp) /
            true
          end
        end

        if found
          View.to_upper
          return View.to_buffer(found)
        end
      end

      # Wasn't found among visible, so create new buffer

      return false   if options[:no_create]   # Don't create it if option says not to

      if dir =~ /@/   # If there's a @, it's remote
        View.handle_bar
        View.to_buffer $el.generate_new_buffer("*console #{dir}")
        $el.elvar.default_directory = "/tmp"
        $el.shell $el.current_buffer
        if dir =~ /(.+?)(\/.+)/   # Split off dir if there
          line = self.ssh_line($1)
          Shell.enter line
          options[:cd_and_wait] ?
            View.insert("cd #{$2} && ") :
            Shell.enter("cd #{$2}")
        else
          line = self.ssh_line(dir)
          Shell.enter line
        end
      else
        Shell.open dir
      end
      return true
    end

    def self.do_last_command

      orig = View.index

      found = self.to_shell_buffer(nil, :no_create=>true)   # If not in shell buffer, go to it

      return View.message("No *console buffer was visible") unless found

      $el.erase_buffer
      $el.comint_previous_input(1)
      self.enter
      View.to_nth orig
    end

    def self.append_log command, dir, prefix=''

      return if command.blank?

      Shell.session_cache_add "$ #{command}"

      return if View.name =~ /_log.notes$/ || ! command
      if dir.nil?
        dir ||= Shell.dir
      end

      command = command.dup
      command.gsub!(/^/, prefix) unless command =~ /^ *!/
      command.gsub!(/^/, '  ')

      raise "Oops, dir didn't have a slash at the end" if dir !~ /\/$/
      raise "Oops, too many slashes at end" if dir =~ /\/\/$/

      txt = "#{dir}\n#{command}\n"
      File.open(@@log, "a") { |f| f << txt } rescue nil
    end

    def self.ssh_line path
      path = path.sub /^\//, ''
      path.sub! /\/$/, ''

      if path =~ /(.+):(.+)/   # If port exists (colon)
        "ssh -p #{$2} #{$1}"
        # Pull out and pass with -p
      else
        "ssh -A #{path}"
      end
    end

    def self.do_as_execute options={}

      if FileTree.handles? && ! Line.matches(/^\s*\|/)   # If we're in a file tree
        path = Tree.construct_path

        file = Line.without_label
        command = Keys.input :prompt=>"Shell command on this file (_ means the filename): "
        command = command =~ /\b_\b/ ? command.gsub(/\b_\b/, "\"#{file}\"") : "#{command} \"#{file}\""

        output = Shell.run(command, :dir=>File.dirname(path), :sync=>true)
        Tree.under(output, :escape=>'| ') if options[:insert]

        return View.message "Command ran with output: #{output.strip}."
      end

      command = Keys.input :prompt=>"Do shell command on '#{View.file_name}': "
      command = "#{command} #{View.file_name}"
      output = Shell.run(command, :dir=>View.dir, :sync=>true)
      View.insert(output) if options[:insert]

      return View.message "Command ran with output: #{output.strip}."

    end

    # Whether buffer ends with shell prompt "...$ "
    def self.prompt?
      right = View.bottom
      left = right - 10
      left = 1 if left < 1
      txt = View.txt left, right

      txt =~ /[>#%$] \z/
    end

    def self.history dir=nil, options={}

      # /, so say we need an argument

      return "
        : Pass a bookmark, to see the history for its directory. Like one of these:
        :xiki/
        :h/
        :tm/
        " if ! dir

      # $... arg from shell/history/, so return dir with commands nested under...

      was_bm = dir =~ /^\:/

      if was_bm
        dir = Bookmarks[dir]
      end

      # /history/, so just show all...

      dir = Files.dir_of dir

      shell_log = File.read(@@log)

      result = []
      match = false
      shell_log.split("\n").each do |l|
        if l =~ /^[+\/-]/
          next match = l =~ /\A[+ -]*#{Regexp.escape dir}/
        end

        result << "#{l}" if match
      end

      result = result.reverse.uniq

      if command = options[:command]
        result = result.select{|o| o =~ /^  \$ #{command}/}
      end

      return "| There's no history for '#{command}' in this directory yet\n| (#{dir}).\n|\n| Run a command and then try again." if result == []

      result = result.join("\n")+"\n"

      return "=#{dir}\n#{result}" if was_bm

      result.gsub /^  /, ''

    end

    def self.commands
      matches = $el.elvar.comint_input_ring.to_s.scan(/#\("(.+?)" /).flatten

      matches.map!{|o| o.gsub '\\"', '"'}
      matches
    end

    def self.custom_history
      dir = View.dir
      history = Shell.commands
      history.uniq! unless Keys.prefix_u
      history = history.join("\n").gsub(/^/, '% ')
      View.create :u if ! View.list_names.member?("*shell history")
      View.to_buffer "*shell history"
      View.kill_all
      Notes.mode

      View.insert "#{history}\n"
      View.to_highest
      Tree.filter
    end

    # Mapped to jump+history
    def self.history_for_bookmark
      bm = Keys.input(:timed => true, :prompt => "bookmark to show shell commands for (space for all, comma for current): ")

      return Launcher.open("shell/all/") if bm == " "
      return Launcher.open("shell/current/") if bm == ","
      if bm == "8"
        Shell.log; View.to_bottom; Search.isearch nil, :reverse=>true
        return
      end

      Launcher.open("shell/history/:#{bm}/")
    end

    def self.prompt_for_bookmark
      dir = Keys.bookmark_as_path :prompt=>"Bookmark to make shell prompt in: "

      View.to_buffer View.unique_name("untitled.notes")
      Notes.mode

      View << "#{dir}\n  $ "
      View >> "\n\n\n"

    end

    def self.current *args

      command = args.pop if args[-1] =~ /^\:/
      console = args.any? ? args.join("/") : nil

      txt = ""

      # /tree/, so list all consoles...

      if ! console
        $el.with(:save_excursion) do

          Buffers.list.each do |b|
            next if $el.buffer_file_name b
            name = $el.buffer_name b
            $el.set_buffer b
            next if $el.elvar.major_mode.to_s != 'shell-mode'

            next if name == "*ol"

            txt << "- #{name}/\n"
            self.commands.reverse.each do |h|
              txt << "  : $ #{h}\n"
            end
          end
        end
        return txt
      end

      # as+delete, so delete it...

     if Keys.prefix(:clear=>1) == "delete"

       Buffers.delete console

       # Delete it from the tree

       Tree.to_parent if command
       Tree.collapse
       Line.delete

       Line.previous if Line =~ /^$/

       return
      end

      # /shell/: command, so jump to it...

      View.to_buffer console, :after_bar=>1

      nil
    end

    def self.exit   # Kills running server or process in shell
      $el.comint_interrupt_subjob
    end

    def self.wait_until buffer, options={}
      max = options[:max] || 10
      message = options[:message] || "Launching..."
      while View.txt(:buffer=>buffer) !~ options[:contains]
        View.flash message, :times=>1
        max -= 1
        break if max < 0
      end
    end

    def self.shell_command_per_prompt prompt, options

      dir, command, task = options[:dir], options[:command], options[:task]

      if ! task && command !~ /^[%$]/
        self.append_log command, dir, "#{prompt} "
      end

      # "$ foo" history item under "$", so just replace parent with it...

      if command =~ /^[%$]/
        return "<$ #{command}"
      end

      # Just "$ " or "% " without command, so show the history?

      if ! command && prompt =~ /[%$]/

        return self.tasks_for_blank_prompt(task, dir, options) if task

        # Ctrl+X on a blank prompt, so show "~ recent/"...

        Tree.<< "~ recent/", :no_slash=>1
        Launcher.launch

        return ""

      end

      # Normal prompt with command, so run it...

      case prompt
      when "$"

        # Takes control if items underneath (task or normal)...

        if result = self.shell_items(options)   # if options[:task] || options[:args]
          result.gsub!(/.\cH/, "")
          return result
        end

        # If nested under a file instead of a dir, remove it and concat onto end of command...

        if dir && File.file?(dir.sub /\/$/, '')
          dir.sub!(/\/$/, '')

          dir, file = dir.match(/(.+)\/(.+)/)[1..2]
          command << " '#{file}'"
        end

        # Add slash on if just regular ls
        command = "ls -p" if command == "ls"

        # Shouldn't run this?
        txt = Tree.pipe self.sync(command, :dir=>dir, :session_key=>View.name)
        txt.gsub!(/.\cH/, "")   # Clean up stupid ^H sequences

        # $ foo, so give shell_wrapper a chance to decorate the output...

        # Todo Rename :shell_root_output to something else?
        # - that makes it more clear it's only passed when no children!?
        #   - maybe > :shell_root_output"

        if ! options[:args]
          options.merge! :shell_root_output=>txt
          self.shell_wrapper(options)
          txt = options[:shell_root_output]
        end
        txt = "<!" if txt == ":\n"
        return txt
      when "%"

        view_orig = View.name

        self.to_shell_buffer dir #, :cd_and_wait=>true
        View.to_bottom
        View.insert command
        self.enter

        View.to_buffer view_orig if View.buffer_visible? view_orig

        nil

      when "&"

        if ! Environment.gui_emacs

          # Quit and run in external shell

          return DiffLog.quit_and_run command
        end

        # Temp > for presentation > don't activate iterm
        # Applescript.run 'tell application "iTerm" to activate'
        return Iterm.run(command) if ! dir
        return Iterm.run("cd #{dir}\n#{command}")
      end
    end

    #       # Mouse, so return all...
    #       return menu if task == [] && options[:mouse]


    def self.tasks_for_blank_prompt task, dir, options

      task[0] = "~ #{task[0]}" if task[0]

      menu = Xik.new "
        ~ examples/
          ! FileTree.example_commands options
        ~ recent/
          ! options[:nest] = 1
          ! options[:no_task] = 1
          ! txt = Shell.external_plus_sticky_history
        ~ recent in dir/
          ! options[:nest] = 1
          ! options[:no_task] = 1
          ! Shell.history options[:dir]
      "

      result = menu[task, :eval=>options]

      result || ""

    end



    # Delegated to by .shell_command_per_prompt to handle when right-clicking or items under the command.
    def self.shell_items options   # New version

      dir, command, args, task = options[:dir], options[:command], options[:args], options[:task]

      # No args or :task items, so do nothing...

      # No longer return > give wrapper a chance to process it
      return nil if ! task && ! args

      # :... or ...\n arg, so delegate to the shell wrapper, if only to show appropriate error...

      return self.shell_wrapper(options) if args && (args[0] =~ /^[:]/ || args[0] =~ /\n/)
      return self.shell_notes(options) if (! args && task[0] == "notes") || args && args[0] =~ /^>/

      # If any other arg, delegate to the =foo menu...

      return self.shell_menu(options) if task == ["xiki command"] || args

      # ~ task, so show task list...

      menu = Xik.new "
        ~ examples/
          ! Shell.shell_examples options
        ~ save/
          ! Shell.shell_save options

        ~ recent/
          ! Shell.shell_recent options
        ~ recent in dir/
          ! Shell.shell_in_dir options

        ~ grab
          ! DiffLog.grab options
        ~ notes
        ~ xiki command
        ~ man
          ! Shell.shell_man options
        ~ expand
          ! Launcher.launch
          ! return nil
        ~ edit/
          + shell wrapper
          + examples
          + notes
          + main/
            + shell wrapper
            + examples
      "

      task[0] = "~ #{task[0]}" if task[0]
      result = menu[task, :eval=>options]

      result = self.shell_edit(task[1..-1], options) if ! result && task[0] == "~ edit"

      result

    end

    def self.shell_in_dir options

      options[:nest] = 1

      dir, command = options[:dir], options[:command]

      command_root = (command||"").sub(/ .*/, '')   # Cut off after the space

      # ~ history, so list history for this command and this dir...

      options[:no_task] = 1
      dir = Shell.dir if ! dir   # We're not nested under a dir, so grab dir from shell

      result = self.history dir, :command=>command

      # Look for matches to whole command...

      return result if result !~ /\A\| There's no history/

      # None found, so look for just command...

      self.history dir, :command=>command_root

    end

    def self.shell_recent options

      options[:nest] = 1
      options[:no_task] = 1
      txt = self.external_plus_sticky_history

      # Filter down

      command = options[:command]

      command_root = (command||"").sub(/ .*/, '')   # Cut off after the space



      # Look for matches to whole command...

      result = txt.split("\n").grep(/^\$ #{Regexp.escape command}/).join("\n")
      return result if ! result.blank?

      # None found, so look for just command...

      result = txt.split("\n").grep(/^\$ #{Regexp.escape command_root}/).join("\n")

    end

    def self.shell_notes options

      dir, command, args, task = options[:dir], options[:command], options[:args], options[:task]
      command_root = command.sub(/ .*/, '')   # Cut off after the space

      # $... and no task, so just run it...

      if args && args[-1] =~ /^\$/ && ! task
        command = args[-1]
        return "<$ #{command}"
      end

      # Todo > if text under heading, always grab siblings to pass...
      # - wait, does it do this anyway?

      if args && args[-1] =~ /^\$/
        args[-1].sub! /$/, "\n"   # Make it look like note contents
      end

      options[:no_task] = 1

      return Xiki.expand "notes/#{command_root}" if ! args   # Must be ~ notes, so just get the headings
      path = Path.join([command_root] + args)

      # If expanding the heading and no task, force "~ expand"
      options[:task] = ["expand"] if args.length == 1 && ! task

      txt = Xiki.expand "notes/#{path}", options.select{|k, v| [:task].include?(k) }

      txt.gsub! /^=\$/, '$'   # Change "=$ foo" lines to just "$ foo"

      # Undo escaping stuff that notes normally does
      txt.gsub! /^=/, '| '
      txt.gsub! /^ /, '|  '

      txt

      # Maybe use command-specific message when no headings yet?:
      # > mock > when not there yet
      # $ git
      #   > Example heading
      #     | You haven't created any notes for "git" yet.
      #     | Modify this command and the heading above.
      #     | Then right-click this command to save the note.
      #     $ git -foo

    end

    def self.shell_save options

      dir, command = options[:dir], options[:command]#, options[:task]
      command_root = command.sub(/ .*/, '')   # Cut off after the space

      # Maybe they just want to remember the command itself

      home_example_file = Bookmarks[":xh/misc/shell_examples/#{command_root}.menu"]

      txt = File.read(home_example_file) rescue ""
      txt = "$ #{command}\n#{txt.strip}"
      txt = "#{txt.strip}\n"
      File.open(home_example_file, "w") { |f| f << txt }

      # Also append this command to > the global 'sticky' log
      #   - makes commands always show up in ~reverse

      File.open(Bookmarks[":xh/misc/logs/shell_sticky_log.notes"], "a") { |f| f << "$ #{command}\n" }

      return "<! example saved!"
    end

    def self.shell_notes_edit command_root
      home_example_file = Bookmarks[":xh/notes/#{command_root}.notes"]

      View.open home_example_file
      View.<<("
        > Getting Started
        Add some example notes here, like these. You'll probably
        want to delete all this stuff first.

        > Another Section
        Just an example of another section. Delete or change this
        as well
        ".unindent) if ! View.txt.any?
    end

    def self.shell_examples_edit command_root, options={}

      home_or_main = options[:main] ? ":xs" : ":xh"

      home_example_file = Bookmarks["#{home_or_main}/misc/shell_examples/#{command_root}.menu"]

      View.open home_example_file
      View.<<("
        | Add some example commands here, like these. You'll probably
        | want to delete all this stuff first.
        $ #{command_root} -foo
        $ #{command_root} -bar
        ".unindent) if ! View.txt.any?
    end

    def self.shell_examples options

      dir, command, task = options[:dir], options[:command], options[:task]

      task.shift if task   # Remove the "examples" item
      command_root = command.sub(/ .*/, '')   # Cut off after the space

      # Try to load ~/xiki/misc/shell_examples file

      home_example_file = Bookmarks[":xh/misc/shell_examples/#{command_root}.menu"]
      file1 = home_example_file # if ! File.exists?(file)
      file1 = nil if ! File.exists?(file1)

      # ~ examples/edit, so just open the file...

      if task == ["edit"]
        options[:no_task] = 1
        self.shell_examples_edit command_root

        return ""
      end

      options[:nest] = 1
      options[:no_task] = 1

      # Try to load xiki :source/misc/shell_examples file

      file2 = Bookmarks[":source/misc/shell_examples/#{command_root}.menu"]
      file2 = nil if ! File.exists?(file2)

      # No examples yet, so prompt them to create them

      return "
        | There aren't any examples for '#{command_root}' yet. Expand
        | the 'save' task to save some examples. Or, edit the
        | text file directly, that the examples are stored in:
        + edit
        " if ! file2 && ! file1

      # Last item is $..., so just run
      if task && task[-1] =~ /^\$/
        command = task[-1]
        return "<$ #{command}"
      end

      txt = ""
      txt << "#{File.read(file1).strip}\n" if file1
      txt << "#{File.read(file2).strip}\n" if file2

      result = Tree.children txt, task

      result = "#{result.strip}\n"
      result = "#{result}+ edit\n" if task == []

      result

    end

    def self.shell_man options

      command, task = options[:command], options[:task]
      command_root = command.sub(/ .*/, '')   # Cut off after the space
      Tree.quote Shell.sync("man #{command_root} | col -x -b")# if task == ["man"]

    end

    def self.shell_menu options
      dir, command, args, task = options[:dir], options[:command], options[:args], options[:task]

      command_root = command.sub(/ .*/, '')   # Cut off after the space

      options[:no_task] = 1

      path = [command_root]
      path += args if args

      txt = Xiki.expand Path.join(path), options.select{|k, v| [:dir, :task].include?(k) }

      txt || ""   # Return blank string in case xiki command returned nil (if it returned nil, it'll continue on)

    end


    def self.shell_edit items, options


      dir, command, args, task = options[:dir], options[:command], options[:args], options[:task]
      command_root = command.sub(/ .*/, '')   # Cut off after the space

      # Escape in case the contain a slash!

      # Delegate to the "shell foo" command to handle it...

      # "shell wrapper", so show it

      case items[0]

      when "shell wrapper"
        # Use wrapper in home dir, unless one in source dir exists

        Command.open_wrapper_source command_root
        return ""
      when "notes"
        self.shell_notes_edit command_root
        return ""
      when "examples"
        self.shell_examples_edit command_root
        return ""
      when "main"

        case items[1]
        when "shell wrapper"
          Command.open_wrapper_source command_root, :only_main=>1
          return ""
        when "examples"
          self.shell_examples_edit command_root, :main=>1
        end
      end

      ""

    end


    # Delegates to shell_foo menu when "$ foo" with :... args.
    def self.shell_wrapper options

      dir, command, args, task = options[:dir], options[:command], options[:args], options[:task]
      command_root = command.sub(/ .*/, '')   # Cut off after the space

      # Escape in case the contain a slash!

      # Delegate to the "shell foo" command to handle it...

      wrapper = "#{Bookmarks[':xiki/misc/shell_wrappers/']}#{command_root}"
      wrapper = "#{Bookmarks[':xh/misc/shell_wrappers/']}#{command_root}" if ! Command.exists? wrapper

      if ! Command.exists? wrapper
        return "
          |
          | There's no command wrapper for '#{command_root}' yet. Modify this
          | example, and then right-click it and select 'create'!
          |
          =#{Files.tilda_for_home wrapper}.rb\n" + File.read(Bookmarks[":source/misc/shell_wrappers/foo.rb"]).gsub(/^/, '            : ').gsub(/ $/, '').gsub(/\bfoo\b/, command)
      end


      # Has \n, so was |... line and should do nothing

      if args && args[0] =~ /\n/
        return %<
          | Lines beginning with "|" in shell command output
          | aren't expandable. Expandable lines in shell output
          | begin with ":", to indicate they're expandable.
          >
      end

      wrapper << "//"

      # Prepend command to the wrapper, because wrapper has to know what the command was

      wrapper << Path.join(args) if args

      options_in = {:dir=>dir, :task=>task, :shell_command=>command, :shell_root_output=>options[:shell_root_output]}
      result = Xiki.expand wrapper, options_in

      # Pass some options back if set
      # Todo > extract this out to > __Menu|__Options.propagate_returned_options
      # And add more options
      # Propagate certain options returned, up the stack

      Options.propagate_some options_in, options

      result

    end

    def self.tasks_source command_name
      favorites = Xiki::Menu.source_files "shell #{command_name} task"
      return nil if favorites == []
      raise "- The '#{command_name} favorites' menu must be a single .menu file" if favorites.length > 1

      favorites = File.read favorites[0]
      favorites.strip.gsub(/^[+-] /, "~ ")
    end

    def self.cd dir
      self.sync "cd '#{File.expand_path dir}'", :session_key=>View.name
    end

    def self.exit_and_cd dir

      DiffLog.quit_and_run "cd #{Files.tilda_for_home(dir, :escape=>1)}"

      ""
    end

    # Shell.dir
    #   "/Users/craig/Dropbox/xiki"
    def self.dir
      key = View.name
      # Return the view dir if no session (don't unnecessarily create a session)
      if ! self.sessions[key]
        dir = View.dir
        return  FileTree.add_slash_maybe dir
      end

      dir = self.sync("pwd", :session_key=>key).strip
      FileTree.add_slash_maybe dir
    end

    # When a tab key pressed at the end of a $... line
    def self.tab

      # Todo > No "which foo", so show all commands starting with "foo"

      # Delegate to ~ menu

      Launcher.launch :task=>["xiki command"]

    end

    def self.all

      # Read history from file...

      txt = Bookmarks.read ":xh/misc/logs/shell_log.notes"

      # Split it into 2-line groups...

      txt = txt.scan(/.+\n  .+/)

      # Reverse and remove dups...

      txt.reverse!
      txt.uniq!

      txt.map!{|o| "=#{o}"}

      txt.each{|o| o.sub! /([^\/])\n/, "\\1/\n"}   # Temp fix for when no slashes

      # Return it...

      txt.join("\n")

    end

    def self.parent command
      Xsh.save_grab_commands command
      DiffLog.quit
    end

    def self.external_plus_sticky_history

      txt = ""

      txt.<< File.read(Bookmarks[":xh/misc/logs/shell_sticky_log.notes"]) rescue ""
      txt.gsub! /^\$ /, ''   # Remove "$ " at beginning

      # txt = File.read File.expand_path("~/xiki/misc/logs/shell_external_log.notes"), *Xiki::Files.encoding_binary
      txt.<< File.read(File.expand_path("~/xiki/misc/logs/shell_external_log.notes"), *Xiki::Files.encoding_binary)

      # Avoids "invalid byte sequence in UTF-8" error
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      # Removes date from lines like ": 1428454597:0;ls -l"
      txt.gsub!(/^: \d{5,20}:\d;/, '')

      txt.gsub!(/^/, '$ ')

      if cache = self.session_cache
        txt.<< cache
      end

      txt.gsub!(/ +$/, '')   # Remove trailing spaces
      txt = txt.split("\n").reverse.uniq.join("\n")
      txt.sub!(/^\$\n/, '')

      txt

    end


    def self.recent_history_external args=nil, options={}

      # Called from key shortcut, so create a new view for it...

      if options[:from_key_shortcut]
        dir = View.dir
        View.to_buffer "recent/"
        View.dir = dir
        Shell.cd dir
        View.kill_all
        View.<< "\n\n\n", :dont_move=>1
        Notes.mode
      end

      txt = self.external_plus_sticky_history

      # Delete first line, but only if "xsh -r" or "xsh"

      txt.sub!(/\A\$ xsh( -r)?\n/, '')   # cut of 1st, it's the one we just did (xsh -r)

      # arg, so only show lines starting with the arg
      if args.any?
        txt = txt.split("\n").select{|o| o =~ /#{args}/}.join("\n")
      end

      # | See the key shortcuts at the bottom. ^G grabs
      # | the command and runs it in your shell.
      # Add stuff at top
      View.insert %`
        | See the key shortcuts at the bottom. (^G also runs in shell)
        `.unindent+"\n"

      View.insert txt, :dont_move=>1
      left = View.cursor

      filter_options = {:left=>left, :right=>(left+txt.length)}

      # ^R from external shell, so make sure it quits when esc or return
      filter_options.merge!(:recent_history_external=>1, :xiki_in_initial_filter=>1) if ! options[:from_key_shortcut]

      Tree.filter filter_options

    end


    # Utility method, for wrapping quotes around a file only
    # when necessary, so we don't make things look busy by
    # using quotes when not necessary.
    # Shell.quote_file_maybe "hi"
    # Shell.quote_file_maybe "hi you"
    def self.quote_file_maybe path

      # Do nothing if it doesn't have spaces or weird chars
      return path if path =~ /\A[\w.\/-]+\z/i

      "\"#{path}\""
    end


    # In-memory cache of the shell commands we've run in this session.
    def self.session_cache
      $el.boundp(:xiki_shell_commands_session_cache) ?
        $el.elvar.xiki_shell_commands_session_cache :
        nil
    end

    def self.session_cache_add command

      return if command.blank?

      val = self.session_cache

      val ? val.<<("#{command}\n") : val = "#{command}\n"

      $el.elvar.xiki_shell_commands_session_cache = val

      nil

    end

  end

end
