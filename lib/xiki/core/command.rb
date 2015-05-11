module Xiki
  class Command

    # Tells you whether a command exists.
    # path to the command but with no extension.
    #
    # Command.exists? "/tmp/git//"
    # Command.exists? "/tmp/git"
    def self.exists? path

      # Check if dir exists

      return true if File.exists? path
      return true if Dir["#{path}.*"].any?

      nil   # Doesn't exist
    end


    # Todo > rename Menu.to_menu to this!
    # def self.open_source command_root, options={}


    # Open user or main command wrapper.
    def self.open_wrapper_source command_root, options={}

      # Hmm > extract out "shell_wrappers" paths?

      # Assume main wrapper exists
      wrapper = Bookmarks[":xs/misc/shell_wrappers/#{command_root}"]

      if ! options[:only_main]
        # User wrapper exists, so use it instead
        source_dir_wrapper = Bookmarks[":xh/misc/shell_wrappers/#{command_root}"]
        wrapper = source_dir_wrapper if Command.exists? source_dir_wrapper
      end

      source_options = Expander.expanders("#{wrapper}//") rescue nil
      if source_options
        wrapper = Tree.source(source_options)
      else
        # Not there, so make it .rb
        wrapper << ".rb"
      end

      wrapper = wrapper[0] if wrapper.is_a?(Array) and wrapper.length == 1

      # If array (multiple items), show tree

      if wrapper.is_a?(Array)
        View.open :txt=>wrapper.join("\n")
        return ""
      end

      View.open wrapper

      # File doesn't exist, so use default wrapper

      if ! File.exists? wrapper # && default_file = options[:default_file]
        View.kill_all
        txt = File.read("#{Xiki.dir}misc/defaults/shell_wrappers.rb")
        txt.gsub! "foo", command_root
        View.<< txt, :dont_move=>1
      end

    end


    def self.to_menu

      # "item" is usually for the sub-item we were on, not the actual command name
      item = Line.without_label

      # > command heading/, so handle separately...

      return self.to_menu_command_heading(item) if item =~ /^>/

      item = Path.split(item)[-1]   # Grab last item (in case multiple items on the same line separated by slashes)

      path = Tree.path

      # If up+, jump to parent menu...

      if Keys.prefix_u
        path.pop
      end

      # +foo, so remove plus...

      path[0].sub! /^\+/, ''

      # Remove $ if "$ foo" command
      path[0].sub! /^\$ /, ''

      options = Expander.expanders path

      source = Tree.source options

      return View.flash "- no source found!" if ! source

      # If was a string, show tree in new view...

      if source.is_a?(String)
        View.open(:txt=>source, :line_found=>2)
        Launcher.enter_all if Line =~ /\/$/   # Show dir recursively, or file contents in tree
        return
      end

      # Must be [file, line_number], so open and jump to line...

      file, line_number = source

      View.open file

      return View.line = line_number if line_number

      # Try to find string we were on when key was pressed
      if item
        # Todo > Probably don't search if we were on the command root (path only has 1 item)
        orig = View.cursor
        View.to_highest
        item = Search.quote_elisp_regex item

        # How does this even make sense? > [+-] in 'to' part?
        item.sub! /^(- |\\\+ )/, "[+-] \\.?"   # match if + instead of -, or dot after bullet
        found = Search.forward item
        Move.to_axis
        View.cursor = orig if ! found
      end
    end


    # Delegated to by .to_menu when it's a command heading
    def self.to_menu_command_heading item

      command = Xiki::Notes.command_heading :check_current_line=>1

      if ! command
        View.flash "- This heading contains no command!"
        return
      end

    end

  end
end
