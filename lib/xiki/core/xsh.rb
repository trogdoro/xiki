require 'shellwords'

module Xiki
  class Xsh

    # Was here for the xsh bash function that suspended called...

    def self.setup
      # Probably move to somewhere else
      $el.elvar.control_lock_color = "#f90"   # Orange
      $el.elvar.control_lock_color_transparent = "#f90"   # Orange
      $el.menu_bar_mode -1

    end

    # def self.run *args #, options={}
    def self.run options={}

      options_in = {:xiki_in_initial_filter=>1}

      # Todo > don't call these lines each time, probably > only once when :args_via_daemon
      # though when :args_via_env, .run is only called once

      Environment.xsh = true

      Xiki.init_in_client   # Do client-specific init stuff (normal .init methods can run before the process is connected to a client)

      # Define keys and such...

      self.setup

      # Enable theme and styles
      Themes.use 'Default'

      if options[:args_via_daemon]
        args = self.populate_args_from_arg_dir

        # No param files were created (special nil return value), so do nothing...

        return if ! args
      end

      if options[:args_via_env]
        args = $el.elvar.xsh_command_line_args.to_a.join(' ')
      end

      # Todo > Probably make sure this is "foo\^foo" and not "foo\\^foo" etc.
      args.gsub! /\\\^/, "^"   # Undo backslash escaping of some chars
      args.gsub! /\\\>/, ">"   # Undo backslash escaping of some chars

      # Command was "xsh xsh ...", so remove the redundant "xsh "...

      args.sub! /^xsh /, ''

      # Check for various flags...

      if args == "" || args == "-o"
        # New session, or not in xsh file, so presume "$ "
        options[:dont_expand] = 1
        args = "$ "

      # xsh ---...
      elsif args == "---"
        args = ""
        $el.elvar.xiki_bar_hidden = true

      # xsh --...
      elsif args == "--"
        args = ""

      # xsh = > or > ^G...

      elsif args == "+" || args == "-g"

        # Jump back to last place we were (or last place content+grab was run)
        return self.to_go_location

      # xsh - > or > ^T...
      elsif args == "-"

        args = "xiki/"

      # $ xsh -., so draw dir tree for current dir...
      elsif args == "-."

        Launcher.open Shell.dir, :task=>["all files"]
        return

      elsif args.slice! /^-n( |\z)/

        # -n, so show notes...

        View.open "%n"
        options[:do_nothing] = 1

      elsif args =~ /^[a-z]*\/$/i

        # $ xsh cd foo/, could be a shell command, so it won't match this (because of the space)...

        # Just leave it alone

      elsif args.slice! /^-c /
        args = "./\n  - ###{args}/"

      elsif args =~ /^%\w/
        View.open Bookmarks[args]
        options[:do_nothing] = 1

      elsif args == "-d"
        options[:do_nothing] = 1
        Search.isearch_diffs

      elsif args == "-i"
        options[:do_nothing] = 1
        Launcher.open "interactions/"

      elsif args == "-v"
        options[:do_nothing] = 1
        View.link_views

      elsif args == "-l"
        options[:do_nothing] = 1
        Launcher.open "links/"

      elsif args == "-b"
        options[:do_nothing] = 1
        Launcher.open "bookmarks/"

      elsif args =~ /^http:\/\//
        options[:not_shell] = 1
      elsif args =~ /\Aselect .+ from /
        options[:not_shell] = 1
      elsif args == "-e"
        args = "edited/"
      elsif args.slice! /^-i( |\b)/   # isolated
        options[:dont_expand] = 1
      elsif args.slice! /^-w /
        args = "http://localhost:8161/#{args}"

      elsif args.slice! /^-fa /
        args = "$ #{args}\n  * favorites/"
      elsif args.slice! /^-f /
        args = "./\n  - **#{args}/"
      elsif args.slice! /^-b /
        options_in[:task] = ["bookmark"]

        args = args == "." ?
          "./" :
          "./\n  - #{args}"
      elsif args.slice! /^-e /
        args = "$ #{args}\n  * examples/"

      elsif args.slice! /^-m\b/
        options[:do_nothing] = 1
        View.open "#{View.dir}/menu.xiki"

      elsif args == "-tab"
        # Esc, Tab with no args, so do tasks menu on blank line

        # Todo > fix > Ctrl+T on a blank line > errors when at very top of file

        args = ""
        options_in[:task] = []

      elsif args.slice! /^-tab /
        # Esc, Tab, so delegate to Notes.tab_key
        options[:tab] = 1
        args = "$ #{args}" if args =~ /\A\w/   # Treat as shell command if it's just a word


      elsif args == "=" || args == "-t"
        options_in[:task] = []
        args = View.dir :force_slash=>1

      elsif args.slice! /^(=|-t) /
        options_in[:task] = []

        # Treat as shell command
        args = "$ #{args}"

      elsif args.slice! /^-q /
        # Esc, Tab, so delegate to Notes.tab_key
        Launcher.open_topic :bm=>args
        return

      elsif args == "-q"
        Launcher.open_topic
        return

      elsif args == "-help"
        args = "help/"

      elsif args == "-ide"
        options[:do_nothing] = 1
        View.layout_todo_and_nav
        View.to_upper

      elsif args.slice! /^-slashify /
        args.sub! ' ', '/'
      elsif args == "$"
        args = "$ "
        options[:dont_expand] = 1

      elsif (args =~ /^[a-z]/i || args.slice!(/^-o /)) && ! options[:not_shell]   # Shell command...
        args = "$ #{args}"

      elsif args == "."
        args = View.dir

      # +foo, so to topic
      elsif args.slice!(/^(\+|-g )/)

        # grab+ on ^bookmark, so cd and exit...

        if args =~ /^%\w/
          args.sub!(/\/$/, '')
          dir = Shell.quote_file_maybe Bookmarks[args]
          return DiffLog.quit_and_run "cd #{dir}"
        end

        return Launcher.open(args, :task=>["view source"])

        # Todo > probably check to see if it exists
          # If doesn't exist, probably do auto-complete

      elsif args == "-c"   # xsh -c > show contents of all files
        View.<< "./"
        Launcher.launch :task=>["more", "all contents"]
        options[:do_nothing] = 1

      elsif args.slice! /^-r( |\b)/   # C-r, ^R

        # Ctrl+R, so delegate to Shell.recent_history_external
        Shell.recent_history_external args#, :from_key_shortcut=>1
        options[:do_nothing] = 1

      elsif args == "-f"
        View.<< "./"
        Launcher.launch :task=>["all files"]
        options[:do_nothing] = 1

      elsif args =~ /\A\+(\w.*)/
        # xsh +foo, so create xiki command...

        name = $1
        extension = name.slice! /\.\w+/
        extension ||= ".menu"

        require "#{Xiki.dir}roots/sample_menus/sample_menus_index.rb" # if !defined?(SampleMenus)
        txt = SampleMenus.by_extension(extension)

        View.open "~/.xiki/roots/#{name}#{extension}"
        View.<< txt if View.txt == ""
        options[:do_nothing] = 1
      elsif args =~ /\A\+\+(\w.*)/
        # xsh ++foo, so create shell command...

        name = $1
        file = "/usr/local/bin/#{name}"

        # If doesn't exist, write it first and chmod...
        if ! File.exists? file
          txt = "
            #!/bin/bash

            # Sample Shell Command

            echo 'hello world'
            ".unindent
          File.open(file, "w") { |f| f << txt }
          `chmod 755 "#{file}"`
        end

        View.open file
        options[:do_nothing] = 1

      elsif args == ":" || args == "-s"

        args = "xiki/"

      elsif args.slice! /^(:\b|-s )/

        # :foo, ^S search shared on XikiHub, so show "shared" command...

        if args == ""
          args = "shared/"
        else
          # Trying with > no prompt
          args = ":#{args}"
        end

      # $ xsh -shell ." > ie. ^X on "$ shell ." > so, draw dir tree for current dir...
      elsif args =~ /^-[a-z]+ \.$/

        options_in[:ctrlx] = 1
        # Maybe just add :ctrlx?:
        args = "$ #{args[/-(.+)/, 1]}"

      elsif args.slice! /^-/

        # -foo, so treat as topic...

        # Do nothing, it's a topic now

      end


      # The above branches should probably just return
      return if options[:do_nothing]

      # --foo, so cut off dash

      if args =~ /^--/   # If --foo, just treat as foo/?
        args.sub! /^--/, ''
      end

      # What was this doing?
      # args.sub! /^\$ (\d\d\d-)/, "\\1"

      # Open the "xsh" view...

      dir = options[:dir] || $el.elvar.default_directory
      View.to_new_session
      View.kill_all

      View.dir = dir
      Notes.mode

      # Moved this higher
      View.<< "\n\n\n", :dont_move=>1
      View.tab_width 12

      exists_in_path = Shell.sync('type -P xsh') =~ /\A\//

      # The 'xsh' command not in $PATH, or no ~/.xsh, so prompt them to configure...

      if args == "$ " && ! ENV['XIKI_SUPPRESS_WELCOME'] && (! exists_in_path || ! File.exists?(File.expand_path "~/.xsh"))
        welcome = Xiki["xsh/setup"]
        View << "\nxsh/setup\n#{welcome.gsub /^/, '  '}\n\n"
      end

      View << args

      # Esc, Tab
      return Notes.tab_key if options[:tab]

      # Todo > use .shellsplit, and .shellescape > to quote args with spaces instead of backslash-escaping spaces

      Launcher.launch(options_in) unless options[:dont_expand]

    end


    # Loads args passed by xsh bash script function in ~/.xiki/misc/params/.
    def self.populate_args_from_arg_dir

      txt = File.read File.expand_path("~/.xiki/misc/tmp/params.txt")
      txt.sub!(/ $/, '')   # Way we save the args makes them always have a trailing slash
      txt

    end


    def self.exit_and_run path
      return "|-Don't know how to handle $$... when not run from shell." if Environment.gui_emacs
      path = "clear\n#{path}"
      $el.kill_emacs path
    end

    def self.exit

      shell_stuff = "clear"

      # TODO > new behavior for grabbing output and printing in parent shell?
      #   Only grab stuff when C-c/q when filtering!
      #   If on dir, cd to it
      #   If on file, echo it and siblings"]

      if Keys.prefix_u
        txt = Tree.siblings.map{|o| "#{o}\n"}.join
        if txt =~ /\A:/   # Only if lines we're on is :... (shell command output)
          txt.gsub!(/^: ?/, '')
          command = Tree.construct_path(:list=>1)[0]
          command.sub! /^\$ /, "$ xsh "
          txt = "#{command}\n#{txt}"
          File.open("/tmp/xikistuff", "w") { |f| f << txt }
          shell_stuff = "clear; cat /tmp/xikistuff"
        end
      end

      $el.suspend_emacs shell_stuff

    end


    def self.chdir_when_xsh_session

      # Only do something if this is a xsh session file...

      return if ! View.file_name || View.dir != Bookmarks["~/.xiki/interactions"]

      # Change the dir to where it was originally created!

      original_dir = self.determine_session_orig_dir View.file_name

      if ! original_dir
        # Todo > try to re-associate the file (via the file id thing I researched)
        return
      end

      Shell.cd original_dir
    end

    def self.determine_session_orig_dir file_name
      File.read(File.expand_path("~/.xiki/misc/interactions_dirs/#{file_name}")) rescue nil
    end

    def self.save_go_commands commands
      tmp_dir = File.expand_path "~/.xiki/misc/tmp"
      FileUtils.mkdir_p tmp_dir   # Make sure dir exists
      File.open("#{tmp_dir}/go_to_shell_commands.xiki", "w") { |f| f << "#{commands.strip}\n" }

      nil
    end

    def self.to_go_location

      # Check for 'G' bookmark...

      g_bookmark = File.read(File.expand_path "~/.xiki/bookmarks/g.xiki") rescue nil

      # 'G' bookmark exists, so just go to it (it's the file we were looking at)
      if g_bookmark
        View.open g_bookmark.strip
        return
      end

      # No "G" bookmark, so we were in a new session when we quit, so go to it...

      # 1. Read in text from sessions.xiki
      sessions_file = File.expand_path "~/xiki/sessions.xiki"
      txt = File.read(sessions_file) rescue ""
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

      # 2. Pull out top sections
      section = txt[/.+?\n(.+?\n)> /m, 1]


      # 3. Open new View with contents of top section
      View.to_new_session
      Notes.mode
      View << section

      # Restore cursor position and directory
      meta_contents = File.read(File.expand_path("~/.xiki/misc/last_session_meta.xiki"))
      cursor, dir = meta_contents.split("\n")

      View.dir = dir
      View.cursor = cursor.to_i

      # Save heading into variable
      $el.elvar.session_heading = txt[/.+/]
      $el.make_variable_buffer_local :session_heading

    end

  end
end
