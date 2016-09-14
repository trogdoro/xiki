require 'open3'
require "#{Xiki.dir}misc/libs/session.rb"

# Didn't quite fix it > try commenting this out?  Look at the docs
Session::use_open3=true

module Xiki
  class Shell

    @@log = File.expand_path("~/.xiki/misc/logs/shell_log.xiki")

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
      options[:sync] = 1
      txt = self.run command, options   #> |||||||

      # Might cause problems
      # Always clean shell commands
      if txt && txt.is_a?(String)
        txt.gsub!(/.\cH/, "")
        txt.gsub!(/\e\[(\d+)(;\d+)*m/, "")
      end

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
        dir = dir.gsub(/\/\/.*/, '/')   # Under menufied, so change "/foo//bar" to "/foo/" before cd'ing


        # If file, but not dir, try deleting lash
        dir.sub!(/\/$/, '') if ! File.directory?(dir)

        raise "- Directory '#{dir}' doesn't exist!" if ! File.exists? dir

        # If dir exists, continue
        if File.directory?(dir)
          # Put slash on end if not there
          dir = "#{dir}/" unless dir =~ /\/$/
        else  # Otherwise, exit
          return puts("#{dir} is not a dir")
        end

        # Leave it nil, so existing dir will be used

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

            # Use last dir I cd'ed to if xiki process restarted > Use buffer-local vars

            cd_dir = $el.boundp(:xiki_shell_last_cd) ?
              $el.elvar.xiki_shell_last_cd :
              View.dir(:startup_dir_if_topic=>1)

            # Creating new session, so use dir of view by default
            session.execute("cd '#{cd_dir}'")
          end

        else

          # Not called with :session_key, so use new session...
          session = Session::Bash.new #(:track_history=>true)
        end


        # "cd ." at left margin, so change to the view dir
        if ! dir && command == "cd ."
          command.sub! ".", "'#{View.dir}'"
        end


        dir_orig = nil

        is_cd_command = command =~ /\Acd\b/

        if dir
          # Session being temporarily over-ridden by dir, and not a cd command,
          # so remember old dir, to restore it after the command
          if session_key && ! is_cd_command
            dir_orig = session.execute("pwd")[0].strip
          end
          session.execute("cd '#{dir}'")
        end

        stdout, stderr = options[:stdin] ?
          session.execute(command, :stdin=>options[:stdin]) :
          session.execute(command)

        # Command at edge (left margin) or cd, so remember new dir
        # Store dir in var that won't go away when el4r restarts
        if session_key && (! dir || is_cd_command)
          last_cd = session.execute("pwd")[0].strip
          $el.make_local_variable :xiki_shell_last_cd
          $el.elvar.xiki_shell_last_cd = last_cd
        end

        # Change back to original dir
        if dir_orig# && command !~ /^cd( |\z)/
          session.execute("cd '#{dir_orig}'")
        end

        raise stderr if stderr.any? && options[:raise_error]

        # cd in shell prompt nested under dir, so indicate
        if session_key && dir && is_cd_command
          options[:dont_escape_output] = 1
          return "<* - applied to margin prompts!"
        end

        # :return_error flag, so return stderr separately...

        return [stdout, stderr == "" ? nil : stderr] if options[:return_error]

        return stderr if ! stdout.any?   # Don't show stderr if any stdout

        return stdout

        # Todo > hook up stdin again!

      else

        # Run command asynchronously (in console)...

        if View.in_bar? and ! options[:dont_leave_bar]
          View.to_after_bar
        end
        buffer ||= "console #{dir}"

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
      View.to_buffer $el.generate_new_buffer("console #{dir}")
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
        pattern = /^\console #{Regexp.quote(dir)}(<| |$)/
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
        View.to_buffer $el.generate_new_buffer("console #{dir}")
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

      return View.message("No console buffer was visible") unless found

      $el.erase_buffer
      $el.comint_previous_input(1)
      self.enter
      View.to_nth orig
    end

    def self.append_log command, dir, prefix=''

      return if command.blank?

      Shell.session_cache_add "$ #{command}"

      return if View.name =~ /_log.xiki$/ || ! command
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

      txt =~ /[>#%&$] \z/
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
      View.create_horizontal :u if ! View.list_names.member?("*shell history")
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

      View.to_buffer View.unique_name("untitled.xiki")
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

            next if name == "ol"

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

      dir, command, task, args = options[:dir], options[:command], options[:task], options[:args]
      stdin = nil

      # "$ foo .", and ^X without args, or ^O when args...

      if (options[:ctrlx] && ! args) || args
        if command =~ / (\.|%[a-z]+)$/
          return self.expand_file_tree_arg prompt, options
        end
      end

      if options[:ctrlx]
        topic = Topic.shell_command_to_topic command

        # Just ^X on command, so no need to pass :task, etc > that delegation happens in another place
        return Expander.expand "#{topic}"
      end


      # Not sure about this > May cause problems > did it because > Multiline input was passed in as :items not :args
      args ||= options[:items]

      ancestors = options[:ancestors]

      # Look at all ancestors > for file path

      shell_nested_dir = nil
      if ancestors
        ancestors.each do |path|
          if FileTree.handles?(path)
            shell_nested_dir = path
          end
        end
      end


      # :args starts with backslash > so append args after command
      if args && args.length == 1 && args[0] =~ /\A\\ /
        # siblings = Tree.siblings
        siblings = Tree.siblings :quotes=>"\\"
        Ol "siblings", siblings   # => ["\\ hey", "\\ you"]
        siblings = siblings.map{|o| o.sub(/^\\/, '')}.join('')
        Ol "siblings", siblings   # => " hey you"
        command << siblings
        Ol "continue here > Remove args!!"
        options.delete :args

      end

      if ! task && command !~ /^[%$&]/
        self.append_log command, dir, "#{prompt} "
      end

      # "$ foo" history item under "$", so just replace parent with it...

      if command =~ /^[$%&]/
        return "<$ #{command}"
      end

      # Just "$ " or "% " without command, so show the history?

      if ! command && prompt =~ /[%$&]/

        return self.tasks_for_blank_prompt(task, dir, options) if task

        # Ctrl+X on a blank prompt, so show "~ recent/"...

        Tree.<< "* recent/", :no_slash=>1
        Launcher.launch

        return ""

      end


      # "%...", So handle headings and actions By delegating to search > Otherwise treated like dollar prompt

      if prompt == "%"
        if ! args || args[0] =~ /^: /
          # "% foo" or "% foo/: foo", so treat just like "$"...
          prompt = "$"
        elsif args[0] =~ /^[>a-z]/
          # "> Heading" or "action", so delegate to search
          search = Topic.shell_command_to_topic "$ #{command}"

          return Xiki[":#{search}", args, options.select{|key, value| [:prefix, :task].include?(key)}]

        else
          return "Don't know how to handle > % #{command} '#{args}'"
        end
      end

      # % foo/> Heading, So delegate to search...

      # Normal prompt with command, so run it...

      case prompt
      when "$"

        # Command line ends with " |", So call as command, Passing second are as stdin

        if command.slice!(/ \|$/) && args && args.length == 1
          stdin = args[0]
          args = nil
        end

        options.merge! :shell_command=>command

        # Takes control if items underneath (task or normal)...
        if ! stdin && result = self.shell_items(options)   # if options[:task] || args   #> |||
          result.gsub!(/.\cH/, "")
          return result
        end

        # If nested under a file instead of a dir, remove it and concat onto end of command...

        if shell_nested_dir && File.file?(File.expand_path(shell_nested_dir.sub /\/$/, ''))
          shell_nested_dir.sub!(/\/$/, '')
          shell_nested_dir, file = shell_nested_dir.match(/(.+)\/(.+)/)[1..2]
          command << " '#{file}'"
        end

        # Just regular ls, so add option to show slashes
        command = "ls -p" if command == "ls"

        # Run actual $... command...   #> continue here > make sure dir is nil when > not nested
        # Make sure dir is nil when > not nested

        options_in = {:dir=>shell_nested_dir, :session_key=>View.name, :stdin=>stdin}
        txt = self.sync(command, options_in)   #> ||||||
        return txt if options_in[:dont_escape_output]


        # No output, so flash "- no output!"
        if txt == "\n"
          View.flash "- no output", :times=>1
          Move.down
          return ""
        end

        txt = Tree.pipe(txt)

        txt.gsub!(/.\cH/, "")   # Clean up stupid ^H sequences

        # $ foo, so give shell_wrapper a chance to decorate the output...

        if ! options[:args]
          options.merge! :shell_output=>txt
          result = self.shell_wrapper(options)   #> |||||||||||

          # If they return a string > Use that as a output
          # If they return nil > Use Original output
          txt = result if result
        end

        txt = "<*" if txt == ":\n"
        return txt

      when "&"

        options_in = {}
        if shell_nested_dir
          options_in[:dir] = shell_nested_dir
        else
          Grab.prepend_cd_if_changed command
        end

        return DiffLog.quit_and_run command, options_in

      end
    end


    def self.expand_file_tree_arg prompt, options

      dir, command, task, args = options[:dir], options[:command], options[:task], options[:args]

      last_arg = command[/(\.|%[a-z]+)$/]
      dir = Bookmarks[last_arg] if last_arg =~ /^%/

      if ! args
        options.delete :ctrlx
        options[:task] = ["all files"]
      end

      options[:propagate] = 1

      txt = Xiki[dir, args, options]

      # No output, which must mean it's a file, so replace dot...

      if options[:returned_file_contents]
        path = Tree.construct_path :raw=>1
        path = path[1..-1]
        Tree.to_root
        Tree.collapse
        Line.sub!(/(\.|%[a-z]+)$/, path.join())
        Move.to_end
        return ""
      end

      return txt

    end


    def self.maybe_show_shell_prompt_keys_tip

      # Dialog doesn't need to be shown, so return false to indicate they can continue running the command

      View.maybe_show_tip "Explaining Ctrl+O to run commands", "\n"+%`
        Tip: Press Ctrl+O to run shell commands (ie. "open" them).
        (Or Ctrl+G to go to your shell and run them.)

        Since everything is editable, typing return normally just
        inserts linebreaks. One exception to this is when the
        bottom bar shows "A-Z filter    Open or return...".

                     (press any key to continue)
      `.unindent, :under=>1

    end


    def self.tasks_for_blank_prompt task, dir, options

      task[0] = "* #{task[0]}" if task[0]

      # ~ examples/
      #   ! FileTree.example_commands options
      menu = Xik.new "
        * recent/
          ! options[:nest] = 1
          ! options[:no_task] = 1
          ! txt = Shell.external_plus_sticky_history
        * recent in dir/
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

      # Should this be done one level higher?
      # Todo > implement
      # "$ shell/! code", so pass as stdin...

      # No args or :task items, so do nothing...

      # No longer return > give wrapper a chance to process it
      return nil if ! task && ! args

      # :... or ...\n arg, so delegate to the shell wrapper, if only to show appropriate error...

      return self.shell_wrapper(options) if args && (args[0] =~ /^[:]/ || args[0] =~ /\n/)   #> |||


      # $ command/[any other arg], so delegate to the corresponding =command xiki command...

      return self.shell_menu(options) if task == ["xiki command"] || args   #> |||||||

      # ~ task, so show task list under $ foo...

      # Temp new version of dropdown (not implemented yet)


      # "> Heading/| Foo", so show items to save or share...

      return "* save\n* share" if args && args.length == 2 && args[0] =~ /^>/ && args[1] =~ /\n/

      txt = %`
        * add note
          ! Shell.add_note options
        * share
          ! Shell.add_note options
        * editor
          ! Grab.content_editor
          ! ""
        * interact
          ! ""

        * recent/
          ! Shell.shell_recent options
        * dir recent/
          ! Shell.shell_in_dir options
        * grab to shell
          ! Grab.go_key options
        * man
          ! Shell.shell_man options
      `


      menu = Xik.new txt

      # Put tilda before 1st item
      if task[0]
        task = task.dup   # Don't affect original option
        task[0] = "* #{task[0]}"
      end

      # Expand shell items task
      result = menu[task, :eval=>options]
      result = self.shell_edit(task[1..-1], options) if ! result && task[0] == "* edit"

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





    def self.add_note options

      command, task = options[:command], options[:task]
      command_root = (command||"").sub(/ .*/, '')   # Cut off after the space

      # ~ add note, so prompt for name...

      if task == ["add note"] || task == ["share"]
        options[:line_found], options[:no_search] = 4, true
        heading = command[/ .*?(\w+)/, 1]   # Try out > first word
        heading.sub!(/(.)/){$1.upcase} if heading


        Notes.add_note_prompt_shell command
        # Notes.add_note_prompt :shell=>command


        return ""


        # | Hints:
        # | 1. Add your note here.
        # | 2. Then type Ctrl+O to save or share.
        # | (These hints will be shown 2 more times)

        # | 1. Add your note here,
        # |    and optionally modify anything else.
      end

      # Get description they entered to use as heading
      heading = task[1].sub(/^: /, "")

      # Heading provided, so prepend to local notes file...

      if heading.any?
        filename = File.expand_path "~/xiki/#{command_root}.xiki"
        section = "#{heading}\n$ #{command}\n\n"

        Notes.add_section filename, section
        View.flash "#{heading}", :times=>3
        return ""
      end

    end


    def self.shell_notes_edit command_root
      home_example_file = Bookmarks["%x/#{command_root}.xiki"]

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

      txt = Xiki.expand Path.join(path), options.select{|k, v| [:dir, :task].include?(k) }   #> ||||||||

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
      when "xiki"
        self.shell_notes_edit command_root
        return ""

      when "main"

        case items[1]
        when "shell wrapper"
          Command.open_wrapper_source command_root, :only_main=>1
          return ""
        end
      end

      ""

    end


    # Delegates to shell_foo menu when "$ foo" with :... args.
    def self.shell_wrapper options   #> ||||-

      dir, command, args, task = options[:dir], options[:command], options[:args], options[:task]
      command_root = command.sub(/ .*/, '')   # Cut off after the space

      # |..., so prompt them to type ^S to search

      if args && args[0] =~ /\n/
        options[:line_found] = 5
        return %`
          :
          : No shell action is installed. Type ^S on the shell command
          : above to search for one on XikiHub. Or type ^O here:
          :
          = do it for me/
        `
      end

      # : ... Or create one yourself.
      # ~ search
      # ~ create

      # Todo > Look in topics for wrapper

      xiki_file = Topic.topic_to_filename command_root
      txt = File.read xiki_file rescue nil

      return if ! txt

      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      heading = nil

      # Figure out action that matches

      patterns = Topic.extract_opener_patterns txt

      # Assume no shell args for now

      command_no_flags = Topic.shell_command_to_topic command

      # The below code runs when there are children also right?

      commands_to_try = [command_no_flags]
      commands_to_try << command_root if command_no_flags != command_root

      # Look for "$ foo/:anything" or "$ foo subcommand/:anything"...

      commands_to_try.each do |command_to_try|

        if anything_heading = patterns["$ #{command_to_try}/:anything"]

          # Heading found, so run it
          user = TopicExpander.username_from_heading anything_heading
          path = Notes.heading_to_path anything_heading, :remove_username=>1

          # Delegate to action!
          args_in = [path, user]
          args_in += args if args

          result = nil

          # Use the shell dir when running commands
          Dir.chdir(self.dir) do   #> ||||
            result = Xiki.expand(command_to_try, args_in, options)   #> |||||
          end

          # Returning nil should mean they didn't handle, even when children (shell output) passed

          return result if result != nil

        end
      end

      # Look for patterns that match this specific line...

      if ! args   #> "git log --oneline"

        # Just make each matching output line have colon...

        patterns.each do |pattern, heading|

          next if ! pattern.start_with?("$ #{command_no_flags}/: ")

          # Make |... line into :... line
          colon_line = pattern[/\/(.+)/, 1].sub(/^../, '')
          options[:shell_output].sub! /^\| (#{Regexp.escape colon_line})/, ": \\1"   #> "$ git log/:anything"
        end
        return
      end

      # Arguments passed, so try to invoke shell handler


      heading = patterns["$ #{command_no_flags}/#{args[0]}"]

      return "| Shell expander not found." if ! heading

      user = TopicExpander.username_from_heading heading
      path = Notes.heading_to_path heading, :remove_username=>1

      # Delegate to action that matches

      args_in = [path, user]
      args_in += args if args

      result = nil

      # Use the shell dir when running commands
      Dir.chdir(self.dir) do   #> ||||
        result = Xiki.expand(command_no_flags, args_in, options)   #> |||||
      end

      return result

    end

    def self.tasks_source command_name
      favorites = Xiki::Command.source_files "shell #{command_name} task"
      return nil if favorites == []
      raise "- The '#{command_name} favorites' menu must be a single .menu file" if favorites.length > 1

      favorites = File.read favorites[0]
      favorites.strip.gsub(/^[+-] /, "* ")
    end

    def self.cd dir
      self.sync "cd '#{File.expand_path dir}'", :session_key=>View.name
    end

    def self.exit_and_cd dir

      DiffLog.quit_and_run "cd #{Files.tilde_for_home(dir, :escape=>1)}"

      ""
    end

    # Shell.dir
    #   "/Users/craig/Dropbox/xiki"
    def self.dir

      key = View.name

      # Return the view dir if no session (don't unnecessarily create a session)
      if ! @@sessions[key]
        dir = if $el.boundp(:xiki_shell_last_cd)
          $el.elvar.xiki_shell_last_cd
        else
          View.dir(:startup_dir_if_topic=>1)
        end
        return FileTree.add_slash_maybe dir
      end

      dir = self.sync("pwd", :session_key=>key).strip   #> ||||||
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

      txt = File.read File.expand_path("~/.xiki/misc/logs/shell_log.xiki")

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

    def self.external_plus_sticky_history

      txt = ""

      txt.<< File.read(File.expand_path("~/.xiki/misc/logs/shell_sticky_log.xiki")) rescue ""
      txt.gsub! /^\$ /, ''   # Remove "$ " at beginning

      txt.<< File.read(File.expand_path("~/.xiki/misc/logs/shell_external_log.xiki"), *Xiki::Files.encoding_binary)

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

      # Cursor is on "$..." line, so create a new view for it...

      if options[:from_key_shortcut]
        path = Tree.path rescue nil

        if path[-1] == ""

          Line << "$\n  * recent/"
          Launcher.launch # :task=>["recent/"]

          return ""

        elsif path[-1] =~ /^[$%&] /
          Tree << "* recent/"
          Launcher.launch # :task=>["recent/"]
          return ""
        end
      end

      dir = View.dir
      View.to_buffer "recent/"
      View.dir = dir
      self.cd dir
      View.kill_all
      View.<< "\n\n\n", :dont_move=>1
      Notes.mode

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
        | See the key shortcuts at the bottom. (Use ^G to run back in the shell)
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

    # Shell.command_exists? "ls"
    #   true
    # Shell.command_exists? "herring"
    #   false
    def self.command_exists? command
      Shell.command("type -P #{command}") =~ /\A\// ? true : false
    end

    def self.send_string txt
      $el.send_string_to_terminal txt
    end

    def self.quit_and_run commands, options={}
      DiffLog.quit_and_run commands, options
    end


  end

end
