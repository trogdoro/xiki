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

      if options[:args_via_daemon]
        args = self.populate_args_from_arg_dir

        # No param files were created (special nil return value), so do nothing...

        return if ! args
      end

      if options[:args_via_env]
        args = $el.elvar.xsh_command_line_args.to_a.join(' ')
      end

      args.gsub! /\A\\\^/, "^"   # Undo backslash escaping of some chars

      # if -c, pull it off and set :xic...

      # Check for various flags...

      if args == ""
        # New session, or not in xsh file, so presume "$ "
        options[:dont_expand] = 1
        args = "$ "

      elsif args.slice! /^-g /

        # Grab on :foo, so just cd and exit...

        if args =~ /^:\w+/
          dir = Shell.quote_file_maybe Bookmarks[args]
          DiffLog.quit_and_run "cd #{dir}"
        end

        # Grab on line, so don't launch (just insert)
        options[:dont_expand] = 1
        args = "$ #{args}"

      elsif args == "-g"

        self.to_grab_location

        options[:do_nothing] = 1

      elsif args.slice! /^-s /

        args = "./\n  - ###{args}/"
      elsif args =~ /^:\w/
        View.open Bookmarks[args]
        options[:do_nothing] = 1
      elsif args == "-d"
        options[:do_nothing] = 1
        Search.isearch_diffs

      elsif args == "^"
        options[:do_nothing] = 1
        Launcher.open "notes/"
      elsif args == ":"
        options[:do_nothing] = 1
        Launcher.open "bookmarks/"
      elsif args == "-b"
        options[:do_nothing] = 1
        Launcher.open "bookmarks/"

      elsif args =~ /^http:\/\//
        options[:not_shell] = 1
      elsif args =~ /\Aselect .+ from /
        options[:not_shell] = 1
      elsif args == "-e"
        args = "edited"
      elsif args.slice! /^-i( |\b)/   # isolated
        options[:dont_expand] = 1
      elsif args.slice! /^-w /
        args = "http://localhost:8161/#{args}"

      elsif args == "-h"
        args = "$"
      elsif args.slice! /^-h( |\b)/
        args = "$ #{args}\n  ~ history/"

      elsif args.slice! /^-fa /
        args = "$ #{args}\n  ~ favorites/"
      elsif args.slice! /^-f /
        args = "./\n  - **#{args}/"
      elsif args.slice! /^-b /
        options_in[:task] = ["bookmark"]

        args = args == "." ?
          "./" :
          "./\n  - #{args}"
      elsif args.slice! /^-e /
        args = "$ #{args}\n  ~ examples/"

      elsif args.slice! /^-m\b/
        options[:do_nothing] = 1
        View.open "#{View.dir}/menu.xiki"

      elsif args.slice! /^-o\b/

        # -o foo, so open it as a file

        if args.any?
          options[:do_nothing] = 1
          View.open "#{View.dir}/#{args.strip}"
        else
          args = "recent/"
        end

      elsif args.slice! /^-x /
        args = "$ #{args}\n  ~ xiki command/"

      elsif args == "-tab"
        # Esc, Tab with no args, so do tasks menu on blank line

        # Todo > fix > Ctrl+T on a blank line > errors when at very top of file

        args = ""
        options_in[:task] = []

      elsif args.slice! /^-tab /
        # Esc, Tab, so delegate to Notes.tab_key
        options[:tab] = 1
        args = "$ #{args}" if args =~ /\A\w/   # Treat as shell command if it's just a word

      elsif args.slice! /^-t\b/
        args.strip!
        options_in[:task] = []

        # It's a blank prompt, so use the current dir
        if args == ""
          args = View.dir
        elsif args =~ /\A\w/   # Treat as shell command if it's just a word
          args = "$ #{args}"
        elsif args =~ /\A:\w/   # Bookmark
          args = Bookmarks[args]
        end

      elsif args == "-ide"
        options[:do_nothing] = 1
        View.layout_todo_and_nav
        View.to_upper
        View.open ":g"

      elsif args.slice! /^-slashify /
        args.sub! ' ', '/'
      elsif args == "$"
        args = "$ "
        options[:dont_expand] = 1
      elsif args == "-" # || args == ""
        args = ""
        options[:dont_expand] = 1

      elsif args =~ /\A[a-z]/i && ! options[:not_shell]
        args = "$ #{args}"
      elsif args =~ /^=/   # If =foo, make it nest under dir?
        args.sub! /^=/, ''
      elsif args == "."
        args = View.dir
      end

      # -foo, so cut off dash

      if args =~ /^--?/   # If -foo, just treat as a command
        args.sub! /^--?/, ''
      end

      # What was this doing?
      # args.sub! /^\$ (\d\d\d-)/, "\\1"

      # Enable theme and styles...

      Themes.use 'Default'
      Styles.reload_styles   # So it uses dark headings

      return if options[:do_nothing]

      # Open the "xsh" view...

      dir = options[:dir] || $el.elvar.default_directory
      View.to_buffer View.unique_name("xsh")
      View.kill_all

      View.dir = dir
      Notes.mode

      # Moved this higher
      View.<< "\n\n\n", :dont_move=>1
      View.tab_width 12

      exists_in_path = Shell.sync('type -P xsh') =~ /\A\//

      # The 'xsh' command not in $PATH, or no ~/.xsh, so prompt them to configure...

      if ! exists_in_path || ! File.exists?(File.expand_path "~/.xsh")
        welcome = Xiki["welcome"]
        View << "\nwelcome/\n#{welcome.gsub /^/, '  '}\n\n"
      end

      # Alternate ways of expanding the args...

      if args == "s"
        View.<< "./"
        Launcher.launch :task=>["filter", "all contents"]
        return

      elsif args.slice! /^r( |\b)/   # C-r, ^R

        # Ctrl+R, so delegate to Shell.recent_history_external
        Shell.recent_history_external args
        return

      elsif args == "f"
        View.<< "./"
        Launcher.launch :task=>["tree"]
        return

      elsif args =~ /\A\+(\w.*)/
        # xsh +foo, so create xiki command...

        name = $1
        extension = name.slice! /\.\w+/
        extension ||= ".menu"

        require "#{Xiki.dir}commands/sample_menus/sample_menus_index.rb" # if !defined?(SampleMenus)
        txt = SampleMenus.by_extension(extension)

        View.open "~/xiki/commands/#{name}#{extension}"
        View.<< txt if View.txt == ""
        return
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
        return

      elsif args =~ /\A\+\+\+(\w.*)/

        # xsh +++foo, so create shell command wrapper...

        name = $1
        extension = name.slice! /\.\w+/
        extension ||= ".rb"

        # TODO > break it out into separate samples per language
        # require "#{Xiki.dir}commands/sample_menus/sample_menus_index.rb" # if !defined?(SampleMenus)
        # txt = SampleMenus.by_extension(extension)

        View.open "~/xiki/commands/shell_#{name}#{extension}"

        txt = %`
          # This will run when they expand
          # the command's output.

          return "hello world"

          # Example: do something based on
          # the line they expanded:
          #
          #   return "You expanded 'a'" if args[1] == ": a"
          #
          `.unindent

        View.<< txt if View.txt == ""
        return

      end

      View << args

      # Esc, Tab
      return Notes.tab_key if options[:tab]

      # Todo > use .shellsplit, and .shellescape > to quote args with spaces instead of backslash-escaping spaces

      Launcher.launch(options_in) unless options[:dont_expand]

    end

    # Loads args passed by xsh bash script function in ~/xiki/misc/params/.
    def self.populate_args_from_arg_dir

      txt = File.read File.expand_path("~/xiki/misc/tmp/params.txt")
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

      return if ! View.file_name || View.dir != Bookmarks["~/xiki/sessions"]

      # Change the dir to where it was originally created!

      original_dir = self.determine_session_orig_dir View.file_name

      if ! original_dir
        # Todo > try to re-associate the file (via the file id thing I researched)
        return
      end

      Shell.cd original_dir
    end

    def self.determine_session_orig_dir file_name
      File.read File.expand_path("~/xiki/misc/sessions_dirs/#{file_name}") rescue nil
    end

    def self.save_grab_commands commands
      tmp_dir = File.expand_path "~/xiki/misc/tmp"
      FileUtils.mkdir_p tmp_dir   # Make sure dir exists
      File.open("#{tmp_dir}/grabbed_commands.notes", "w") { |f| f << "#{commands.strip}\n" }
    end

    def self.to_grab_location
      file = File.expand_path "~/xiki/bookmarks/g.notes"

      location = File.read(file) rescue nil
      return if ! location

      location.strip!
      View.open location
    end

  end
end
