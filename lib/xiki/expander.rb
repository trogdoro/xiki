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
        | Xiki["ip"]   # Same thing
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
            | exists in a dir in MENU_PATH such as ~/menu/, you can simply
            | invoke it by name:
            |   Xiki["ip"]
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
  # Expander.parse("a").should == {:name=>"a"}
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

    return options.merge!(:name=>Menu.format_name(thing.to_s)) if thing.is_a? Symbol
    return options.merge!(:class=>thing) if thing.is_a? Class

    raise "Don't know how to deal with #{thing.class} #{thing.inspect}" if ! thing.is_a? String

    # It's a string (possibly name, file, pathified, pattern)

    self.extract_ancestors thing, options   # Move ...@ ancestors into options if any

    # If menu-like, extract menu and items and return...

    if thing =~ /^[\w _-]+(\/.*|$)/
      path_items = Path.split thing   # Split into items

      options[:name] = Menu.format_name path_items.shift   # First arg is name, so pull it off

      # Store original path, since it could be a pattern, and set items extracted from path...

      if ! options[:items]
        options[:path] = thing   # Store original path, since it could be a pattern that just looks like a menu
        options[:items] = path_items if path_items.any?
      else   # If already items (2nd arg was an array)
        options[:path] = ([thing] + options[:items]).join("/")   # Make path string include 2nd arg items
        options[:items] = path_items + options[:items] if path_items.any?   # Prepend any path items 2nd arg items
      end

      return options
    end

    thing = self.expand_file_path thing if thing =~ /^[~.$]/   # Expand out file shortcuts if any

    # If not a file, it's probably a pattern, so store in line and return...

    return options.merge!(:path=>thing) if thing !~ /^\/($|[\w. -]+$|[\w. -]+\/)/ && thing !~ %r"^//"

    # It's file-ish, so either file or menufied ("//")

    # If menufied (has "//"), parse and return...

    if thing =~ %r"^(/[\w./ -]+|)//(.*)"   # If /foo// or //foo
      menufied, items = $1, $2
      menufied.sub! /\.\w+$/, ''

      menufied = "/" if menufied.blank?

      options[:menufied] = menufied
      options[:path] = thing
      raise "didn't anticipate expanders yet" if options[:expanders]

      # Grab args

      items = Path.split items
      options[:items] = items if items.any?

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
  # @expander/docs/expanding/
  def self.expand *args

    # Break path down into attributes...

    options = self.parse *args

    # Figure out what type of expander (to use and set some more attributes along the way)...

    self.expanders options

    expanders = options[:expanders]

    if ! expanders || expanders.length == 0
      options[:no_slash] = true
      return "@flash/- Your indenting looks messed up!" if options[:not_well_formed]
      return "@flash/- Can't launch empty line!" if options[:path].blank?
      return "@flash/- No menu or pattern defined!"
    end

    # Delegate to one or more expanders...

    options[:expanders_index] = 0
    options.delete :halt   # Any :halt from .expanders was only meant to stop looking for more expanders

    expanders.each do |expander|
      expander = expander[:expander] if expander.is_a?(Hash)   # For patterns, :expanders has {:expander=>Pattern, ...}
      expander.expand options

      break if expander == Menu && options[:output]   # Always stop going after MenuExpander, if it had output

      break if options[:halt]   # Possibly .expands? didn't halt but .expand did
      options[:expanders_index] += 1
    end

    Launcher.append_log(options[:path]) if options[:path] && options[:expanders].find{|o| o == Menu} && ! options[:dont_log]
    options[:output]
  end


  # Adds :expander=>TheClass to options that says it .expands? this path (represented by the options).
  # Expander.expanders(:name=>"foo").should == "guess"
  # Expander.expanders(:file_path=>"/tmp/").should == {:file_path=>"/tmp/", :expander=>FileTree}
  # Expander.expanders("a").should == "guess"
  def self.expanders *args
    options = args[0]   # Probably a single options hash
    options = self.parse(*args) if ! args[0].is_a?(Hash)   # If not just a hash, assume they want us to parse

    [PrePattern, Menu, FileTree, Pattern, MenuSuggester].each do |clazz|
      clazz.expands? options
      break if options[:halt]
    end

    options
  end


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

  # Defines menus so they can later be called with Expander.expand.
  # Note that menus in a MENU_PATH dir don't need to be def'ed.
  #
  # Xiki.def(:abc){ "some code" }
  #
  # > Future plans for how you'll be able to define menus
  # - "pre" pattern menus
  #   | Xiki.def(:pre=>1, :view=>"*ol"){...}
  # - normal pattern menus (without a regex)
  #   | Xiki.def(:view=>"*ol"){...}
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

    if args[0].is_a? String
      kind = Menu

      # Menu if symbol or string...

      name = nil

      # If a file path, grab name from it
      if args[0] =~ %r"^/"
        # Remove slashes and extension from end
        # For now, the decision is to ignore the extension at define time
        args[0].sub! /(\.\w+)?\/*$/, ''
        name = args[0][%r"([^\/]+)/*$"]   # "
      else   # Must be normal foo/... menu path
        return self.delegate_to_keys args, options, block if args[0] =~ /\+/   # If name has "+", delegate to key shortcut
        name = args[0].scan(/\w+/)[-1]   # Might be a menufied path
      end

      Menu.defs[name || "no_key"] = implementation

    elsif args[0].is_a? Regexp

      # Pattern (or PrePattern) if 1st arg is a regexp...

      if options.delete :pre
        patterns = PrePattern.defs
      else
        patterns = Pattern.defs
      end

      keys = []
      options.each do |k, v|
        keys += [k, v]
      end

      keys.<< :global if keys.empty?
      keys += [args[0], implementation]

      Xi.hset patterns, *keys

    else

      # For now, assume just one key is passed in, like...
      # options   # => {:view=>"*ol"}
      key, val = options.to_a[0]   # => [:view, "*ol"]
      PrePattern.defs[key] = {val=>implementation}

      # Result should look like...
      #   PrePattern.defs=>{
      #     :view=>{
      #       "*ol"=>implementation

    end

    nil
  end


  # Just expands out ~/, ./, ../, and $foo/ at beginning of paths,
  # leaving the rest in tact
  #
  # Expander.expand_file_path("$x/a//b").should =~ %r".+/xiki/a//b$"
  # Expander.expand_file_path("$x//")
  #   /projects/xiki//
  # Expander.expand_file_path("$ru//")
  #   /Users/craig/notes/ruby/ruby.notes//
  def self.expand_file_path path

    # Probably restore this line
    return path if path !~ %r"^(~/|\.|\$\w)"   # One regex to speed up when obviously not a match

    # Expand out ~
    return path.sub "~", File.expand_path("~") if path =~ %r"^~/"

    # Expand out . and ..
    return path.sub $1, File.expand_path($1) if path =~ %r"^(\.+)/"

    # Expand out $foo/ bookmarks
    if path =~ %r"^(\$[\w]+)(/|$)"
      file = Bookmarks[$1]
      file.sub! /\/$/, ''   # Clear trailing slash so we can replace consistently with dirs and files
      return path.sub /\$\w+/, file
    end

    # TODO > Make bookmarks not emacs dependant > Use Bookmarks2 to expand bookmarks - or just update Bookmarks?!

    path   # No match, so return unchanged
  end

  def self.delegate_to_keys args, options, block

    # If 2 args and 2nd is string, wrap block based on enter+ other
    if args.length == 2 && menu.is_a?(String)
      options.merge! :bar_is_fine=>1

      menu = args[1]

      block =
        if args[0] =~ /^enter\+/
          lambda{ Launcher.insert menu, options }
        elsif menu =~ /^@/   # If menu is @foo..., prompt for bookmark and nest under result
          lambda{
            file = Keys.bookmark_as_path :include_file=>1, :prompt=>"Enter a bookmark: "
            Launcher.open "#{file}\n  #{menu}", options
          }
        elsif menu =~ /^\.@/   # If menu is .@foo..., nest under current file
          menu.sub! /^\./, ''
          lambda{
            bm = Keys.input :optional=>1, :prompt=>"Enter bookmark, or pause for current file."   # Terminated by pause
            file = bm ? Keys.bookmark_as_path(:bm=>bm, :include_file=>1) : View.file
            Launcher.open "#{file}\n  #{menu}", options
          }
        else
          lambda{ Launcher.open menu, options }
        end
    end

    args[0].gsub! '+', '_'
    Keys.method_missing args[0], &block

    # Store where it was defined for open+key, else we won't know (have to climb stack to do this)

    definer = caller(0)[3]
    keys = Keys.words_to_letters args[0]
    Keys.source[keys] = definer

    nil
  end

end
