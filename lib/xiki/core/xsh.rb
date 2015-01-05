require 'shellwords'

module Xiki
  class Xsh

    # Was here for the xsh bash function that suspended called...

    def self.setup
      # Probably move to somewhere else
      $el.elvar.control_lock_color = "#f90"   # Yellow
      $el.menu_bar_mode -1
    end

    # def self.run *args #, options={}
    def self.run options={}

      $el.message ""

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

      if args.slice! /^-s /
        args = "./###{args}/"
      elsif args =~ /^:\w/
        options[:not_shell] = 1
        args = Bookmarks[args]
      elsif args =~ /^http:\/\//
        options[:not_shell] = 1
      elsif args =~ /\Aselect .+ from /
        options[:not_shell] = 1
      elsif args == "-e"
        args = "edited"
      elsif args.slice! /^-i ?/   # isolated
        options[:dont_expand] = 1
        # Do nothing, just remove it
        #options[:no_socket] = 1
      elsif args.slice! /^-w /
        Ol["Start server if not running yet!"]
        args = "http://localhost:8161/#{args}"


      elsif args == "-h"
        args = "$"
      elsif args.slice! /^-h ?/
        args = "$ #{args}\n  ~ history/"


      elsif args.slice! /^-f /
        args = "$ #{args}\n  ~ favorites/"
      elsif args.slice! /^-n /
        args = "./**#{args}/"
      elsif args.slice! /^-e /
        args = "$ #{args}\n  ~ examples/"
      elsif args.slice! /^-m /
        args = "$ #{args}\n  ~ menu/"
      elsif args.slice! /^-d /
        options_in[:dropdown] = []
        args = "$ #{args}"
      elsif args.slice! /^-slashify /
        args.sub! ' ', '/'
      elsif args == "$"
        args = "$ "
        options[:dont_expand] = 1
      elsif args == "-" # || args == ""
        args = ""
        options[:dont_expand] = 1
      elsif args == "-r" || args == ""
        options[:dont_expand] = 1

        # New session, or not in xsh file, so presume "$ "
        args = "$ "
        options[:dont_expand] = 1
      elsif args =~ /\A[a-z]/i && ! options[:not_shell]
        args = "$ #{args}"
      elsif args =~ /^--?/   # If -foo, just treat as a command
        args.sub! /^--?/, ''
      elsif args =~ /^=/   # If =foo, make it nest under dir?
        args.sub! /^=/, ''
      end

      args.sub! /^\$ (\d\d\d-)/, "\\1"

      args = "./" if args == "."
      dir = options[:dir] || $el.elvar.default_directory

      View.to_buffer View.unique_name("xsh")
      View.kill_all

      View.refresh   # Attempt to minimize flickering (show the blank screen) - didn't work, but leave it in anyway I guess

      View.dir = dir
      Notes.mode

      Themes.use 'Default Sparse'
      Styles.reload_styles   # So it uses dark headings

      exists_in_path = Shell.sync('which xsh').any?
      if ! exists_in_path
        welcome = Xiki["welcome", :add_xiki_to_path=>1]
        View << "welcome/\n#{welcome.gsub /^/, '  '}\n\n\n"
      end

      # Check for some args late in the game...
      if args == "n"
        View.<< "./"
        Launcher.launch :dropdown=>["filter", "filenames"]
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

      elsif args == "s"
        View.<< "./"
        Launcher.launch :dropdown=>["filter", "all"]
        return
      end

      View << "#{args}\n\n\n"

      Move.backward 3
      View.tab_width 12
      View.refresh

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

    def self.use_default_theme_maybe

      # If not set, declare it as frame local...
      if ! $el.boundp(:left_initial_theme)
        $el.make_variable_frame_local :left_initial_theme
      end

      # if Environment.xsh? && ! self.left_initial_theme
      if Environment.xsh? && ! $el.elvar.left_initial_theme
        Styles.frame_only = 1
        Themes.use('Default') rescue nil   # In case we moved the files after starting
        Styles.frame_only = nil
        $el.elvar.left_initial_theme = 1
      end

    end

  end
end
