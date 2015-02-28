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

      options_in = {}

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
        args = $el.elvar.xsh_command_line_args.to_a
      end

      args = args.shelljoin
      args.gsub! "\\^", "^"   # Undo backslash escaping of some chars

      # if -c, pull it off and set :xic...

      # Check for various flags...

      if args == ""
        # New session, or not in xsh file, so presume "$ "
        args = "$ "
        options[:dont_expand] = 1

      elsif args.slice! /^-g /

        # Grab on :foo, so just cd and exit...

        if args =~ /^:\w+/
          dir = Shell.quote_file_maybe Bookmarks[args]
          Xsh.save_grab_commands "cd #{dir}"
          DiffLog.quit
        end

        # Grab on line, so don't launch (just insert)
        options[:dont_expand] = 1
        args = "$ #{args}"

      elsif args.slice! /^-g\b/

        tmp_dir = File.expand_path "~/xiki/misc/tmp"
        FileUtils.mkdir_p tmp_dir   # Make sure dir exists
        file = "#{tmp_dir}/last_quit_location.notes"

        location = File.read(file) rescue nil
        location ||= "/tmp/"

        location.strip!
        View.open location
        options[:do_nothing] = 1

      elsif args.slice! /^-s /

        args = "./\n  - ###{args}/"
      elsif args =~ /^:\w/
        View.open Bookmarks[args]
        options[:do_nothing] = 1
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
        options_in[:dropdown] = ["bookmark"]

        args = args == "." ?
          "./" :
          "./\n  - #{args}"
      elsif args.slice! /^-e /
        args = "$ #{args}\n  ~ examples/"

      elsif args.slice! /^-m\b/
        options[:do_nothing] = 1
        View.open "#{View.dir}/menu.xiki"

      elsif args.slice! /^-o\b/

        # -o foo, so open it as a file...

        if args.any?
          options[:do_nothing] = 1
          View.open "#{View.dir}/#{args.strip}"
        else
          args = "opened/"
        end

      elsif args.slice! /^-x /
        args = "$ #{args}\n  ~ xiki command/"

      elsif args == "-t"
        # Esc, Tab with no args, so explain why it doesn't make sense
        args = "
          | Doing Esc, Tab on a blank prompt doesn't make sense.
          | Next time, type a command or file path to autocomplete
          | it before doing Esc, Tab.
        ".unindent
        options[:dont_expand] = 1
      elsif args.slice! /^-t /
        # Esc, Tab, so delegate to Notes.tab_key
        options[:tab] = 1
        args = "$ #{args}" if args =~ /\A\w/   # Treat as shell command if it's just a word
      elsif args.slice! /^-d /
        options_in[:dropdown] = []
        args = "$ #{args}" if args =~ /\A\w/   # Treat as shell command if it's just a word
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

      args.sub! /^\$ (\d\d\d-)/, "\\1"

      Themes.use 'Default'
      Styles.reload_styles   # So it uses dark headings

      View.message ""   # Suppresses "Loading places from..." message at startup > it happens when "xsh :foo", etc.

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

      exists_in_path = Shell.sync('which xsh') =~ /\A\//

      if ! exists_in_path
        welcome = Xiki["welcome"]
        View << "\nwelcome/\n#{welcome.gsub /^/, '  '}\n\n"
      end

      # Alternate ways of expanding the args...

      if args == "s"
        View.<< "./"
        Launcher.launch :dropdown=>["filter", "all contents"]
        return

      elsif args == "f"
        View.<< "./"
        Launcher.launch :dropdown=>["tree"]
        return

      elsif args == "r"
        Shell.recent_history_external(options[:reverse]) # if options[:reverse]
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
      Launcher.launch(options_in) unless options[:dont_expand]

    end

    # Called when a new emacsclient session joins
    def self.populate_args_from_arg_dir
      dir = File.expand_path "~/xiki/misc/params"
      return if ! File.directory? dir

      files = Dir["#{dir}/*"].sort
      args = files.map{|o| File.read(o).strip }

      return nil if args == []
      args = [] if args == [""]

      files.map{|o| File.delete(o) }

      args

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

  end
end
