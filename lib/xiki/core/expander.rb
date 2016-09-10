module Xiki
  class Expander

    def self.menu
      %`
      - docs/
        - todo: move the def examples (not the expand examples) to...!
        @defs/docs/
        | probably move most of it into this menu?

        - summary/
          | This class handles "expanding" paths.  Expanding paths is the core
          | feature of Xiki.  It's what happens when you double-click (or
          | Control-return) on things, but is often used purely programatically as
          | well.
          |
          | Examples:
          | Expander.expand "ip"   # => "192.0.0.1" (assuming "ip" menu exists)
          | Xiki["ip/"]   # Same thing
        - expanding/
          | Expander.expand() handles expanding paths. "Expanding" is roughly a
          | synonym for evaluating, running, or executing.
          - more/
            | Xiki[] delegates to Expander.expand, and is the preferred way of
            | expanding.  Usually a single string is passed as a parameter, a
            | "path".  And usually a single string is returned as the result.
          - paths/
            | "Path" is a loose term for strings that are passed into
            | Expander.expand().  Here are some common examples of paths:
            |
            |   /tmp/a.txt
            |   animals/dogs/
            |   ip
            |   $ df
            |   select * from users
            |
            | Paths can be files, directories, menus, sql statements, ruby code,
            | just arbitrary strings, pretty much anything.  As you can see, some
            | are more path-like (having slashes and words) and some are less so.
          - examples/
            - files and dirs__/
              | First just discuss dir contents and file contents
              |   (/tmp/a)
              | Then, below, discuss "menu-izing" them
              |   (/tmp/a//)
            - menus that are already defined__/
              | If an "ip" menu is already defined (see "defining" below), or
              | exists in a dir in XIKI_PATH such as ~/.xiki/roots/, you can simply
              | invoke it by name:
              |   Xiki["ip/"]
              |
              | Many menus can be passed paths after the menu name:
              |   Xiki["mysql/setup"]
              |   Xiki["mysql/setup/start"]
              - more/
                | Here's an example of a path that isn't a menu:
                |   Xiki["select * from users"]
            - ruby files/
              | You can treat simple .rb files as menus, by adding "//" at the end.
              |   Xiki["/tmp/foo.rb//"]
            - menu files/
              | Xiki["/tmp/foo.rb//"]   # Treats the file(s) (or dir) at this path as
              | Xiki["/tmp/foo//a/b"]   # Same thing, but passing a path to it
            - dirs/
              | Xiki["/tmp/foo//"]   # Treats the file(s) (or dir) at this path as
              |                    # a menu, and returns the results.
            - more/
              - multiple files/
                | Xiki["/tmp/foo"]   # multiple files__
              - multiple dirs/
                | Xiki["/tmp/foo"]   # multiple dirs__
        - defining/
          | You can define menus with Expander.def
          - examples/
            - files/
              |
              | Xiki.def "/tmp/foo"   # Define a "foo" menu, backed by the file(s)
              |                       # (or dir) at this path, that can be invoked
              |                       # later by .expand.
              | Xiki.def "/tmp/menu/*"   # Define the __
            - blocks/
      - api/
        @Expander.expand "ip"
      `
    end

    # Called by .expand.  Breakes path into a hash of properties.
    # It extracts properties that can be deduced by the path alone.
    # Not until the .expand methods of the individual expanders
    # will we add stuff to the hash based on definitions or menu
    # dirs.
    #
    # This stuff this method extracts should probably be limited
    # to elements that determine which expander to use, or are of
    # interest to multiple expanders.
    #
    # Expander.parse("a").should == {:name=>"a", :path=>"a"}
    # Expander.parse("/tmp").should == {:file_path=>"/tmp"}
    # Expander.parse(:foo=>"bar").should == {:foo=>"bar"}
    def self.parse *args

      options = args[-1].is_a?(Hash) ? args.pop : {}   # Extract options if there

      # Just return if input has already been parsed into a hash
      return options if args.empty?

      thing = args.shift

      # If 1st arg is array, treat it as list with ancestors
      if thing.is_a? Array
        ancestors = thing[0..-2]
        options[:ancestors] = ancestors if ancestors.any?
        thing = thing[-1]
      end

      # If 2nd arg is array, it's a list of args
      options[:items] = args.shift if args[0].is_a? Array

      # If non-string add and return...

      return options.merge!(:name=>Command.format_name(thing.to_s)) if thing.is_a? Symbol
      return options.merge!(:class=>thing) if thing.is_a? Class

      raise "Don't know how to deal with #{thing.class} #{thing.inspect}" if ! thing.is_a? String

      # It's a string (possibly name, file, pathified, pattern)

      self.extract_ancestors thing, options   # Move ...@ ancestors into options if any

      # Split up all path types, to pull off options...
      self.extract_task_items thing, options

      # If menu-like, extract menu and items and return...

      # Command or topic name, so set :name option

      # Todo > remove .foo

      if thing =~ /\A`?\w[\w _-]*(\/|\z)/

        path_items = Path.split thing   # Split into items

        options[:name] = Command.format_name path_items.shift   # First arg is name, so pull it off

        # Split off extension if any...

        extension = options[:name].slice!(/\.[a-z]*$/) if options[:name] !~ /\A\./

        options[:extension] = extension if extension

        # Store original path, since it could be a pattern, and set items extracted from path...

        if ! options[:items]
          options[:path] = thing   # Store original path, since it could be a pattern that just looks like a menu
          options[:items] = path_items if path_items.any?
        else   # If already items (2nd arg was an array)

          path_old = options[:path]
          options[:path] = ([thing] + options[:items]).join("/")   # Make path string include 2nd arg items
          # Append slash to path, if old path had one at end
          options[:path] << "/" if path_old =~ /\/$/

          options[:items] = path_items + options[:items] if path_items.any?   # Prepend any path items 2nd arg items
        end

        return options
      end

      # Eventually, remove $...

      # If just "." or "~", treat as file path

      # Expand bookmarks

      thing = self.expand_file_path thing if thing =~ /^[~.%]/   # Expand out file shortcuts if any

      # If not a file, it's probably a pattern, so store in line and return...

      return options.merge!(:path=>thing) if thing !~ /^\/($|[\w. -]+$|[\w. -]+\/)/ && thing !~ %r"^//"
      # It's file-ish, so either file or menufied ("//")

      # If menufied (has "//"), parse and return...

      if thing =~ %r"^(/[\w./ -]+|)//(.*)"   # If /foo// or //foo

        menufied, items = $1, $2

        # Dot in filename, so assume file extension to ignore, but only if corresponding file exists...

        if File.file? menufied
          menufied.sub! /\.\w+$/, ''
        end

        menufied = "/" if menufied.blank?

        options[:menufied] = menufied
        options[:path] = thing

        raise "didn't anticipate expanders yet" if options[:expanders]

        # Grab args

        # Append to existing items

        items = Path.split items
        if items.any?
          options[:items] ||= []   # Set to empty if not already set
          # Appening items > might cause problems
          options[:items] = items + options[:items]
        end

        return options
      end

      # Else, assume just a file path...
      options[:file_path] = thing

      # TODO below: handle path based on whether API or EDITOR
        # If just file (and no ## or $, etc) and EDITOR
          # Call View.open method
        # Else, just delegate to FileTree
          # Handle ## and $, etc in ew version - FileTree2?

      options
    end

    # Most important method in Xiki.  Called when stuff is double-clicked on.
    # Expander.expand "ip"   # => "192.0.0.1"
    # expander/docs/expanding/
    def self.expand *args

      options = nil

      options_before_propagate = self.handle_propagate_option_in args

      # If 1st arg is just a hash with sources, we're being called again so don't re-parse and re-find expands
      if args[0].is_a?(Hash) # && args[0][:expanders]
        options = args[0]

      else

        # Break path down into attributes
        options = self.parse *args

        # Remove any pre-existing :expanders, so they don't clash and make infinite loop!
        options.delete :expanders
        options.delete :expanders_index

        # Figure out what type of expander (to use and set some more attributes along the way)
        self.expanders options
      end

      options.delete(:items) if options[:items] == []

      # Blank arg list, so set to nil

      # ^... "grab option", so delegate to Grab.foo method...

      return Grab.call_grab_method options if options[:grab_option]

      # # Task, so make future ^X's expand with task...

      # Command source passed in as text, so .invoke it as the literal command text
      # return self.expand_literal_command(options[:command_text], options) if options[:command_text]

      # It's a class, so just .invoke it directly

      return Invoker.invoke *args if options[:class]

      expanders = options[:expanders]

      if ! expanders || expanders.length == 0

        options[:no_slash] = true

        return "<* Your indenting looks messed up!" if options[:not_well_formed]

        if options[:path].blank?

          # return CommandSuggester.blank_at options[:ancestors] if options[:ancestors]

          # task, so show options...
          return self.blank_line_option_items options if options[:task]   #> ||

          return "<* Try Ctrl+T or Ctrl+X on blank lines"
        end

        return "<* No menu or pattern found!"
      end

      # Delegate to one or more expanders...

      options[:expanders_index] = 0
      options.delete :halt   # Any :halt from .expanders was only meant to stop looking for more expanders

      expanders.each do |expander|
        expander = expander[:expander] if expander.is_a?(Hash)   # For patterns, :expanders has {:expander=>Pattern, ...}

        expander.expand options

        break if expander == Command && options[:output]   # Always stop going after MenuExpander, if it had output

        break if options[:halt]   # Possibly .expands? didn't halt but .expand did
        options[:expanders_index] += 1
      end

      Launcher.append_log(options[:path]) if options[:path] && options[:expanders].find{|o| o == Command} && ! options[:dont_log]
      txt = options[:output]

      if options[:client] == "web" && txt !~ /^\s*<html[ >]/i
        if options[:extension]
          txt = "extension but web"
        else
          txt = Xiki::Html.to_html txt, options
        end
      end

      self.handle_propagate_option_out(options_before_propagate, options) if options_before_propagate   #> |||||

      txt
    end

    def self.handle_propagate_option_in args

      # Find location of options in args
      options_index = args.index{|o| o.is_a? Hash}

      return if ! options_index

      # No :propagate, so do nothing
      return if ! args[options_index][:propagate]

      # Save original and return them, so we can hold on and propagate back into them later
      original_options = args[options_index]

      # Create a blank hash and copy only the important options into it, and make that be the actual options that are propagated down
      options_in = {}
      Options.propagate_some_inward original_options, options_in
      args[options_index] = options_in

      original_options

    end

    def self.handle_propagate_option_out options_before_propagate, options

      # Just propagate out to original
      Options.propagate_some_outward options_before_propagate, options   #> ||||||

    end


    def self.blank_line_option_items options

      option_item = options[:task]

      # Show blank line tilde options...

      menu = Notes.option_items(:context=>:blank_line)   #> |||
      xik = Xik.new(menu)

      option_item = OptionItems.prepend_asterisk option_item

      txt = xik[option_item, :eval=>options]

      txt

    end


    # Adds :expander=>TheClass to options that says it .expands? this path (represented by the options).
    # Expander.expanders(:name=>"foo")[:expanders].should == [Xiki::Command]
    # Expander.expanders(:file_path=>"/tmp/").should == {:file_path=>"/tmp/", :expander=>FileTree}
    # Expander.expanders("a").should == "guess"
    def self.expanders *args
      options = args[0]   # Probably a single options hash
      options = self.parse(*args) if ! args[0].is_a?(Hash)   # If not just a hash, assume they want us to parse

      [PrePattern, Command, FileTree, TopicExpander, Pattern, CommandSuggester].each do |clazz|   # For each expander
        clazz.expands? options
        break if options[:halt]
      end

      options
    end

    # Just expands command body that was passed in (usually options[:command_text])

    # Move ancestors into options if any
    #
    # Expander.extract_ancestors("a/@b/", {})
    # Args are changed to: "b/", {:ancestors=>["a/"]}
    def self.extract_ancestors path, options={}

      path_new = Path.split path, :outer=>1   # Split off @'s

      # If ancestors, pass as options?
      if path_new.length > 1
        options[:ancestors] = path_new[0..-2]
        path.replace path_new[-1]
      end
    end

    # Expander.extract_task_items "foo/~ rename/"
    # Extracts "* foo" items from the path, removing them.
    def self.extract_task_items thing, options={}

      path = Path.split thing

      # *... item, so pull them off and store in :task...

      # "^ foo" items are no longer used, right?

      # Eventually look only for astexix > Not also tilde > but maybe also caret?
      index = path.index{|o| o =~ /^[*~^] / && o !~ /\n/}   # Tasks won't have linebreaks

      return if ! index

      task = path.slice! index..-1
      path = Path.join path


      is_grab_option = task[0] =~ /^\^/

      task[0].sub! /^[*~^] /, ''

      is_grab_option ?
        options[:grab_option] = task :
        options[:task] = task

      thing.replace path
    end


    # Defines menus so they can later be called with Expander.expand.
    # Note that menus in a XIKI_PATH dir don't need to be def'ed.
    #
    # Xiki.def(:abc){ "some code" }
    #
    # > Future plans for how you'll be able to define menus
    # - "pre" pattern menus
    #   | Xiki.def(:pre=>1, :view=>"foo"){...}
    # - normal pattern menus (without a regex)
    #   | Xiki.def(:view=>"foo"){...}
    #   | Xiki.def(:token1=>"select"){...}   # optimized via hash lookup
    #   | Xiki.def(/^select/, :token1=>"select"){...}   # optimized via hash, with pattern that runs after
    def self.def *args, &block

      # Maybe know by the "/" at end that it was called via a menu?
      # And delegate to "list" again?
      # Use yield to check??

      options = args[-1].is_a?(Hash) ? args.pop : {}   # Extract options if there

      implementation = block || args[0]

      # If symbol, just treat as string
      args[0] = args[0].to_s if args[0].is_a? Symbol

      # If :eval, use as block
      block = options[:eval] if options[:eval]

      if args[0].is_a? String

        kind = Command

        # Command if symbol or string...

        name = nil

        # If a file path, grab name from it
        if args[0] =~ %r"^/"
          # Remove slashes and extension from end
          # For now, the decision is to ignore the extension at define time
          args[0].sub! /(\.\w+)?\/*$/, ''
          name = args[0][%r"([^\/]+)/*$"]   # "
        else   # Must be normal foo/... menu path

          # foo+bar name, so define normal key shortcut...

          return self.def_key args, options, block if args[0] =~ /\+/
          name = args[0].scan(/\w+/)[-1]   # Might be a menufied path
        end

        Command.defs[name || "no_key"] = implementation

      elsif args[0].is_a? Regexp

        # Pattern (or PrePattern) if 1st arg is a regexp...

        if options.delete :pre
          patterns = PrePattern.defs
        else
          patterns = Pattern.defs
        end

        # Turn any remaining options keys into nesting that limits it. Example:
        # options: :target_extension=>".rb"
        # resulting keys: [:target_extension, ".rb"]

        keys = []
        options.each{|k, v| keys += [k, v]}

        keys.<< :global if keys.empty?
        keys += [args[0], implementation]

        Xi.hset patterns, *keys

      else

        # For now, assume just one key is passed in, like...
        key, val = options.to_a[0]   # => [:view, "foo"]
        PrePattern.defs[key] = {val=>implementation}

        # Result should look like...
        #   PrePattern.defs=>{
        #     :view=>{
        #       "foo"=>implementation

      end

      nil
    end


    # Just expands out ~/, ./, ../, and :foo/ at beginning of paths,
    # leaving the rest in tact
    #
    # Expander.expand_file_path("%n")
    # Expander.expand_file_path("%n")

    # Expander.expand_file_path("%xiki/a//b").should =~ %r".+/xiki/a//b$"
    # Expander.expand_file_path("%xiki//")
    # Expander.expand_file_path("%ru//")
    def self.expand_file_path path

      return path if path !~ %r"^(~/|\.|%\w)"   # One regex to speed up when obviously not a match

      # Expand out ~
      return path.sub "~", File.expand_path("~") if path =~ %r"^~/"

      # Expand out . and ..

      return path.sub($1, File.expand_path($1, Shell.dir)) if path =~ %r"^(\.+)/"

      # Expand out %foo/ bookmarks

      if path =~ /^(%[\w]+)(\/|$)/
        file = Bookmarks[$1]
        file.sub! /\/$/, ''   # Clear trailing slash so we can replace consistently with dirs and files
        return path.sub /%\w+/, file
      end

      # TODO > Make bookmarks not emacs dependant > Use Bookmarks2 to expand bookmarks - or just update Bookmarks?!

      path   # No match, so return unchanged
    end

    def self.def_key args, options, block

      command = args[1]

      # 2nd arg is string, so treat it like a command ...

      # Example: Xiki.def "view+dimensions", "dimensions/", :hotkey=>1
      if args.length == 2 && command.is_a?(String)
        block =
          if command =~ /^=/   # If command is =foo..., prompt for bookmark to nest result under
            lambda{
              file = Keys.bookmark_as_path :include_file=>1, :prompt=>"Enter a bookmark: "
              Launcher.open "#{file}\n  #{command}", options
            }

          elsif command =~ /^\.=/   # If command is .=foo..., nest under current file

            # For key shortcuts, if .=..., use current file, or view name...

            command.sub! /^\./, ''
            ->{
              bm = Keys.input :optional=>1, :prompt=>"Enter bookmark, or pause for current file."   # Terminated by pause

              # Nest under... tree path if we're in one, else view path, else buffer name...

              ancestor = FileTree.grab_file_on_line
              ancestor ||= View.file || View.name

              file = bm ? Keys.bookmark_as_path(:bm=>bm, :include_file=>1) : ancestor
              Launcher.open "#{file}\n  #{command}", options
            }
          else

            # Make key just open the command...
            lambda{ Launcher.open command, options }
          end
      end

      # Just normal definition...

      args[0].gsub! '+', '_'


      keys = Keys.words_to_letters args[0]

      # foo, so save to Keys.map...

      if args[0] =~ /^[a-z]/
        # Define in nested map of keys...
        path = args[0].split("_")
        Keys.map((path + [block]), options)

        # Todo > remove "l" and "c" from this, since l is "list"

        # Make jump+ be search

        return unless path[0] =~ /^[j]/   # search+... and custom+... keys still need to be defined the old way
      end

      # search+..., custom+..., or other first word, so map the standard emacs way...

      key = Keys.words_to_letters args[0]

      map = :global_map

      # search+..., so pull off "search+" and add to :isearch_mode_map

      # Make go+ be search

      if key =~ /^J/
        key.sub! /./, ''   # Chop of S
        map = :isearch_mode_map
      end

      key.gsub!(/./){|o| (o.downcase.sum - 96).chr }   # Convert to control chars

      # If block is a string, make wrapper to eval
      if block.is_a?(String)
        return Keys.define_key_that_evals map, key.inspect, "Xiki::#{block}".inspect
      end

      $el.define_key(map, key, &block)

      nil
    end


  end
end
