module Xiki
  class Command

    # Tells you whether a command exists.
    # path to the command but with no extension.
    #
    # Command.exists? "/tmp/git//"
    # Command.exists? "/tmp/git"
    # Command.exists? "ip"
    def self.exists? path

      # It's a file path, so just check file...

      # Might be causing problem > if path didn't always have a slash!
      if path =~ /\/./   # Path (not just one at end)
        # Check if dir exists
        return true if File.exists? path
        return true if Dir["#{path}.*"].any?
        return nil   # Doesn't exist
      end

      # No slash, so assume it's a command, and look it up...

      options = Command.root_sources_from_path_env :name=>path.sub(/\/$/, '')
      options[:sources]

    end


    # Todo > rename Command.to_menu to this!
    # def self.open_source command_root, options={}


    # Open user or main command wrapper.
    def self.open_wrapper_source command_root, options={}

      # Hmm > extract out "shell_wrappers" paths?

      # Assume main wrapper exists
      wrapper = Bookmarks["%xs/misc/shell_wrappers/#{command_root}"]

      if ! options[:only_main]
        # User wrapper exists, so use it instead
        source_dir_wrapper = File.expand_path("~/.xiki/misc/shell_wrappers/#{command_root}")
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
        raise "Obsolete!!!"
      end

    end


    def self.to_menu


      # "item" is usually for the sub-item we were on, not the actual command name
      item = Line.without_label

      # > command heading/, so handle separately...

      # Disabled command headings for now
      # return self.to_menu_command_heading(item) if item =~ /^>/

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

      # Add dot, if there's not one there
      path[0].sub! /^/, "." if path[0] !~ /^\./

      options = Expander.expanders path

      # .notes file, so just jump to it...


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
    # def self.to_menu_command_heading item
    #
    #   command = Xiki::Notes.command_heading :check_current_line=>1
    #
    #   if ! command
    #     View.flash "- This heading contains no command!"
    #     return
    #   end
    # end





    # Extracted from menu.rb

    def self.install *args
      raise "- Commenting this could cause problems!!!"
      '
      > TODO
      | implement this.

      - Look in gem dirs for installed gems with this name
      - Google search for xiki menu on web
        @google/"foo.menu"
      '
    end

    def self.simple_class *args
      root = 'foo'
      trunk = Tree.path
      root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1   # If nested path (due to @), grab root of parent

      %`
      - =~/.xiki/roots/
        - #{root}.rb
          | class #{TextUtil.camel_case(root)}
          |   def self.menu *args
          |     "- sample item/\\n- Args passed: \#{args.inspect}\\n- Customize me in) =~/.xiki/roots/#{menu}.rb"
          |   end
          | end
      `
    end

    def self.menu_with_method *args
      root = 'foo'
      trunk = Tree.path
      root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1   # If nested path (due to =), grab root of parent

      %`
      - =~/.xiki/roots/
        - #{root}.rb
          | class #{TextUtil.camel_case(root)}
          |   def self.menu
          |     "
          |     - cake/
          |       - chocolate/
          |     - .pie/
          |     "
          |   end
          |
          |   def self.pie
          |     "- apple/"
          |   end
          | end
      `
    end

    def self.menu_with_two_methods *args
      root = 'foo'
      trunk = Tree.path
      root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1   # If nested path (due to =), grab root of parent

      %`
      - =~/.xiki/roots/
        - #{root}.rb
          | class #{TextUtil.camel_case(root)}
          |   def self.menu
          |     "
          |     - sammiches/
          |       - ham/
          |         - .buy/
          |       - tofu/
          |         - .buy/
          |     - .checkout/
          |       - cash/
          |       - credit/
          |     "
          |   end
          |   def self.buy category, item
          |     "- buying \#{item} \#{category}"
          |   end
          |   def self.checkout kind
          |     "- checking out as \#{kind}..."
          |   end
          | end
          |
      `
    end

    def self.using *args
      txt = %`
        > Summary
        | How to use Xiki menus.  Xiki menus are menus you type, not menu bar.
        |
        | All xiki menus can be used the same way.  Just type something and
        | double-click on it (or type Ctrl-enter while the cursor is on the line)
        | to open the menu.
        |
        - example/
          | 1: Type "sammiches" on a line (the "=" isn't necessary when the line
          |    isn't indented)
          =sammiches
          |
          | 2: Double-click (or Ctrl-enter) on it to open it. You can try it on
          |    the line above.  It will look like this:
          | =sammiches/
          |   + meat/
          |   + veggie/
          |   + checkout/
          |
          | 3: Double-click to open items. It will look like this:
          | =sammiches/
          |   + meat/
          |   + veggie/
          |     + cucumber/
          |     + bark/
          |   + checkout/
          |
        - mouse/
          | You can click on the "bullets" (the - and + at the beginnings of lines)
          | to expand and collapse.  You can also double-click to expand and
          | collapse.
          |
        - keyboard/
          | You can do everything with the keyboard that you can do with the mouse.
          | Type Ctrl-enter while your cursor is on the same line as a menu or menu
          | item to open it.  Or, if it already has things under it, it will collapse.
          |
          | Right after you open a menu, some keys you type have special meaning. This
          | is the case whether you you used the mouse or keyboard to open the menu.
          | The cursor turns into a block to indicate this.
          |
          | Special keys right after opening:
          - search to narrow down/
            | If you type numbers and letters, it incrementally narrows down the list.
          - opening/
            | Return - opens menu item cursor is on
            | Tab - opens, hiding the other items at that level
            | Ctrl-/ - opens, moving the item onto the same line as its parent
            | Ctrl-1, Ctrl-2, Ctr-3, etc. - opens the nth item
            | Ctrl-G: stops searching
            |
        `

      Tree.children txt, args
    end

    def self.creating *args
      txt = %q`
        > Summary
        | How to make your own Xiki menus.  This is strongly encouraged!  Make
        | a bunch of menus for all the various things you do, if you're so
        | inclined.  You can make menus for from simple notes, to powerful
        | CRUD interfaces etc. for controlling external tools.
        |
        - Pretend they exist/
          | The easiest way to create new menus is to type them and open them as
          | though they exist.  Xiki will then guide you through turning them
          | into actual menus.
          |
          | The simplest option is the first one it shows you.  You can just type
          | menu items inline to create the menu.  For example, you could type
          | this:
          |
          | milkshakes/
          |   - chocolate/
          |   - vanilla/
          |
          | And then with your cursor on one of these lines you could type
          | Ctrl-a Ctrl-m (for "as menu") to turn it into an actual menu.  Then,
          | the next time you type "milkshakes" and open it, it would show the
          | menu items.  For this menu to be useful, you'll probably want to add
          | more items underneath them, or open the items to be prompted to
          | create a ruby class to run when they're opened in the future.
          |
        - Screencasts/
          | Check out these two points in one of the xiki screencast to see
          | creating menus in action:
          |
          =http://www.youtube.com/watch?v=bUR_eUVcABg#t=1m30s
          =http://www.youtube.com/watch?v=bUR_eUVcABg#t=1m57s
          |
        - Creating .menu files/
          | You can make menus without code, by just putting "whatever.menu" files
          | in the "menu" dir in your home dir.
          |
          | For example you could create a "drinks.menu" file, like the following.
          | (The "|" characters aren't actually in the file).
          |
          | ~/.xiki/roots/
          |   - drinks.menu
          |     | - fruit/
          |     |   - lemonade/
          |     |   - smoothie/
          |     | - caffeinated/
          |     |   - coffee/
          |     |   - tea/
          |
          | Then when you type drinks on a line and open it, it will look like
          | this:
          |
          | drinks/
          |   + fruit/
          |   + caffeinated/
          |
        - Delegating/
          | You can make simple menus that delegate to other things, using the
          | "=" character.  For example:
          |
          | ~/.xiki/roots/
          |   - foo.menu
          |     | - =other menu/
          |     | - =MyClass.my_method
          |     | =$ shell command
          |
        - Wiki elements/
          | You can put wiki elements in menus, like headings, paragraphs and
          | bullet points.  Thus you can make a menu just to store notes:
          |
          | shopping list/
          |   > Grocery
          |   - Eggs
          |   - Vodka
          |
          |   > Pet store
          |   Not sure yet.  Maybe just a bunch of snakes.
          |
        - Creating .rb files/
          | Create ruby files in ~/.xiki/roots/ to make dynamic menus.  The .menu class
          | method will be invoked.  Example:
          |
          | ~/.xiki/roots/
          |   - pie.rb
          |     | class Pie
          |     |   def self.menu *args
          |     |     "
          |     |     - fruit/
          |     |       - apple/
          |     |       - pumpkin/
          |     |     - nuts/
          |     |     "
          |     |   end
          |     | end
          |
          | To make a menu run a method, put a dot in front of it:
          |
          | ~/.xiki/roots/
          |   - pie.rb
          |     | class Pie
          |     |   def self.menu *args
          |     |     "
          |     |     - fruit/
          |     |       - apple/
          |     |       - pumpkin/
          |     |     - .nuts/
          |     |     "
          |     |   end
          |     |   def self.nuts *args
          |     |     # Put any code here.  The string you return will be inserted
          |     |     # into the tree.
          |     |     "- pecan/\n- pecan/"
          |     |   end
          |     | end
          |
          |
          > For more info, see:
          =menu/api/
          |
        `

      Tree.children(txt, args.join('/'))
    end

    def self.reload_menus
      Launcher.load_tools_dir
      View.flash
      nil
    end


    # Deprecated?
    def self.external menu="", options={}

      View.message ""

      View.wrap :off

      # IF nothing passed, must want to do tiny search box
      if menu.empty?
        Launcher.open ""
        View.message ""
        View.prompt "Type anything", :timed=>1, :times=>2 #, :color=>:rainbow

        Launcher.launch
      else
        Launcher.open menu, options
      end
    end

    def self.as_command options={}
      orig = View.cursor

      # Move down when at left margin (equivalent to doing it on the next step)
      if $el && Line.indent == ""
        Line.next
      end

      Tree.to_root

      root, left = Line.value, View.cursor
      root = Line.without_label :line=>root

      root = TextUtil.snake_case(root).sub(/^_+/, '')

      Tree.after_children
      right = View.cursor
      View.cursor = left

      # Go until end of paragraph (simple for now)
      Effects.blink(:left=>left, :right=>right) if ! options[:no_blink]
      txt = View.txt left, right
      txt.sub! /.+\n/, ''
      txt.gsub! /^  /, ''

      # Remove help text that prompts you to create the command with the items (if exists)
      txt.sub! /^ +> Save\?\n += save command\/\n.+\n.+\n/, ''
      # Remove help text that prompts you to update commands
      txt.sub! /^ +> Make this into a command\?\n += save command\/\n.+\n/, ''

      return Tree.<<("| You must supply something to put under the '#{root}' command.\n| First, add some lines here, such as these:\n- line/\n- another line/\n", :no_search=>1) if txt.empty?

      command_dir = File.expand_path "~/.xiki/roots"
      path = File.expand_path "#{command_dir}/#{root}.xiki"

      file_existed = File.exists? path

      if file_existed
        treeb = File.read path
        txt = Tree.restore txt, treeb

        DiffLog.save_diffs :patha=>path, :textb=>txt
      end

      txt = txt.unindent

      Dir.mkdir command_dir if ! File.exists? command_dir
      File.open(path, "w") { |f| f << txt }

      View.cursor = orig if orig

      return if options[:no_success_flash]

      # message = "- #{file_existed ? 'Updated' : 'Created'} ~/.xiki/roots/#{root}.xiki"

      # Show "- Updated ~/.xiki/roots/foo.xiki"...

      if file_existed
        # message = "Updated ~/.xiki/roots/#{root}.xiki"
        message = "- Updated command!"
        return View.flash message, :times=>3
      end

      # Show "- Created ~/.xiki/roots/foo.xiki"...

      return message = %`
        > Success!
        | Created the "#{root}" command!
        |
        | Use ^G to update it after editing.
        | Use ^S to share it on XikiHub.
        = ok/
      `

      nil
    end

    @@loaded_already = {}

    def self.load_if_changed file
      return :not_found if ! File.exists?(file)
      previous = @@loaded_already[file]
      recent = File.mtime(file)

      if previous == nil
        load file
        @@loaded_already[file] = recent
        return
      end

      return if recent <= previous

      load file
      @@loaded_already[file] = recent
    end


    # Collapse tree one level.  Assumes line has arrows
    def self.launch_after_collapse

      line = Line.value

      bullet = line[/<+:?/]
      arrows = bullet[/<+/].length

      arrows -= 1 if arrows > 1   # Make << go back just 1, etc.

      line = Line.without_label :line=>line

      skip = line.empty? && arrows - 1

      Line.sub! /^(  +)<+ .+/, "\\1- "   # Delete after bullet to prepare for loop

      arrows.times do |i|
        # If no items left on current line, jump to parent and delete
        if Line =~ /^[ +-]+$/
          Tree.to_parent
          Tree.collapse
          Move.to_end
        end

        unless i == skip   # Remove last item, or after bullet if no items
          Line.sub!(/\/[^\/]+\/$/, '/') || Line.sub!(/^([ =+-]*).*/, "\\1")
        end
      end

      if Line.indent.blank?   # Remove any = at beginning of line
        Line.sub! /^=+ ?/, ''
      end

      # Replace line we're adding and current line

      Line << line unless skip

      Launcher.launch
    end

    def self.launch_after_collapse_root options={}

      # Grab line
      line = options[:line] || Line.value

      arrows = line[/<+/].length
      line.sub!(/ *<+= */, '')

      # Go up to root, and kill under
      arrows.times { Tree.to_root }
      Tree.collapse

      # Insert line, and launch
      old = Line.delete :leave_linebreak
      old.sub! /^( *).+/, "\\1"
      old << "=" if old =~ /^ /   # If any indent, = is needed
      View << "#{old}#{line}"

      Launcher.launch
    end

    # Invoke and then replace in tree
    def self.launch_then_move_output kind

      kind == :item
      kind == :siblings

      # Run in place, grab output, then move higher and show output

      orig = View.line

      # Probably can't call launch, probably expand instead?
      Launcher.launch :no_search=>1

      # If didn't move line, assume it had no output, and it's collapse things itself
      return if orig == View.line

      # If it inserted something
      output = Tree.siblings :cross_blank_lines=>1, :children=>1

      # Shouldn't this be looping like self.collapser_launcher ?
      Tree.to_parent
      Tree.to_parent
      Tree.collapse :no_plus=>1
      Tree << output
    end

    def self.menu_to_hash txt
      txt = File.read txt if txt =~ /\A\/.+\z/   # If 1 line and starts with slash, read file

      txt.gsub(/^\| /, '').split("\n").inject({}) do |o, txt|
        txt = txt.split(/ : /)
        o[txt[0]] = txt[1]
        o
      end

    end


    # Moves item (and siblings) to root of tree (replacing tree), then launches (if appropriate).
    def self.do_as_menu
      prefix = Keys.prefix :clear=>1   # Check for numeric prefix
      launch = false
      line = Line.value

      txt =
        if line =~ /^ +\| +def (self\.)?(\w+)/   # If on a quoted method, construct Foo.bar
          Ruby.quote_to_method_invocation
        elsif line =~ /^ *\|/   # If on quoted line, will grab all quoted siblings and unquote
          Tree.siblings :string=>1
        elsif line =~ /^[ +-]*=/ && Tree.has_child?   # If on ^=... line and there's child on next line...
          # Will grab the whole tree and move it up
          Tree.subtree.unindent.sub(/^[ =+-]+/, '')
        else
          launch = true
          Tree.path.last
        end

      Tree.to_root(:highest=>1)

      launch = false if prefix == :u

      Tree.collapse

      Line.sub! /^([ @=]*).+/, "\\1#{txt}"

      return if ! launch

      Launcher.launch
    end

    def self.do_as_outline

      line = Line.value

      txt = Tree.path.last
      Tree.to_root(:highest=>1)

      Tree.collapse
      Line.sub! /^([ @=]*).+/, "\\1#{txt}"

      Keys.prefix = "outline"
      Launcher.launch
    end


    def self.line_exists? name, pattern #, options={}
      name.gsub! /\W/, '_'
      dir = File.expand_path "~/.xiki/roots3"
      file = File.expand_path "#{dir}/#{name}.menu"
      txt = File.read(file) rescue ""
      txt =~ pattern ? ($1 || $&) : nil   # Return whole string or group
    end

    #
    # Create simple .menu file if it doesn't exist, otherwise add line to it.
    #
    # Command.append_line "menu name", "- text to add"
    #
    def self.append_line name, addition #, options={}

      name.gsub! /\W/, '_'

      # Default to ~/.xiki/roots
      # If menu there, create, otherwise append

      # Get existing
      dir = File.expand_path "~/.xiki/roots"
      Dir.mkdir dir if ! File.exists? dir

      file = File.expand_path "#{dir}/#{name}.menu"
      txt = File.read(file) rescue ""

      if txt =~ /^#{Regexp.escape addition}$/
        return "<* was already there!"
      end

      # Append to end (might be blank)

      txt << "#{addition}\n"

      # Save
      File.open(file, "w") { |f| f << txt }

      "- saved setting!"
    end


    # Has subset of menus that are defined manually.  Usually via
    # Xiki.def.  Most menus don't need to be defined because they
    # exist in a menu dir (in XIKI_PATH).
    # Examples: "ip"=><instance>, "tables"=>"/etc/menus/tables"
    @@defs ||= {}


    # Adds to :expanders if :name is backed by a menu.
    def self.expands? options

      # If no name, maybe an inline menufied path, so set sources...

      # Let it go on...
      # since we now want to handle unrecognized extentsions as text...
      # Will this interfere with Method.

      # An item is a "> Heading", so allow TopicExpander to take control
      items = options[:items]

      # Am I sure I want to intercept all headings? (This comment is in one other place)
      #   - don't have to decide now > just limit to 1st arg!
      # return if items && items.find {|o| o =~ /\A> /}

      name = options[:name]

      return if name.blank? && ! options[:menufied]   # If no name and not menufied, probably begins with a dot

      # Only handle when dot or tilde at beginning

      if name
        return if name !~ /\A`/
      end

      # Extension that looks like domain suffix, so treat as url

      if extension = options[:extension]
        return if [".com", ".org", ".loc", ".net", ".edu"].member?(extension)
      end

      # Double-slash, so "menufied" file path...
      if ! name
        if options[:menufied]
          self.root_sources_from_dir options
          (options[:expanders] ||= []).push self
          return
        end
        return   # Can't handle if wasn't inline and no :name
      end

      # Don't take control if >..., since TopicExpander now handles these
      return if items && items[0] =~ /\A> / && ! TopicExpander.exempt_commands.member?(name)

      # See if name is directly defined...

      if implementation = @@defs[name.sub(/\A`/, '')]
        if implementation.is_a? String
          implementation = Bookmarks[implementation]
          # If file, remove any extension
          if File.file? implementation
            implementation.sub! /\.\w+$/, ''
          end

          options[:menufied] = implementation

          self.root_sources_from_dir options
          return (options[:expanders] ||= []).push self
        end

        kind =
          if implementation.is_a? Proc
            :proc
          elsif implementation.is_a? Class
            :class
          end
        raise "Don't know how to deal with: #{implementation.inspect}" if ! kind   #> :proc
        options[kind] = implementation
        return (options[:expanders] ||= []).push self
      end

      # Try to look name up in PATH env...

      (options[:expanders] ||= []).push(Command) if self.root_sources_from_path_env(options)[:sources]   # Found it if we created :sources


        #
        # > Discussion of future caching
        # TODO: cache output of .root_sources_from_path_env outputs whet it starts to take up time
        # - clear cache when updated by guard - think through guard strategy - probably gurad just builds one big file upon updates, and xiki checks only that file's mod date, and reloads (if Xiki.caching = :optimized
        # caching - punt for now
        # - if :all, store hash with output of .root_sources_from_path_env for each name
        # - if :optimized, check date of /tmp/xiki_path_env_dir_cache
        # - if :off (maybe rails dev mode - not worth setting up guard)
        #

    end

    # Expand thing (represented by the hash) like a menu.  Could be a block, or
    # menufied (has sources).
    def self.expand options

      return self.expand_when_extension options if options[:extension]   # If foo.txt/...

      # Get simple invocations out of the way (block or class)...

      # If it's a proc, just call
      return options[:output] = options[:proc].call(options[:items] || [], options) if options[:proc]

      # If it's a class, just call (or, wait, need to include .menu file - probably no)
      raise "TODO: implement dealing with a class" if options[:class]

      if ! options[:menufied]   # Assume Command.expands? should have pulled this out for now
        raise "Can't do anything if no menufied and no name: #{options.inspect}" if ! options[:name]

        implementation = @@defs[options[:name]]

        raise "Can't do anything if no implentation: #{options.inspect}" if ! implentation
        raise "Don't know how to expand #{implementation}." if ! implementation.is_a? String
      end


      # It's a string...

      # Must be either
        # menufied path? (starts with slash)
        # menu name
        # either: could have items

      return self.expand_menufied options
    end


    # Called by .expand when extension was in path (foo.txt/a/b/)
    def self.expand_when_extension options

      options[:no_slash] = 1

      self.climb_sources options

      extension = options[:extension]
      sources = options[:sources]

      only_one_source = sources.length == 1 && sources[0].length == 1
      file = "#{options[:enclosing_source_dir]}#{sources[-1][0]}"


      # Just delete if as+delete
      if options[:prefix] == "delete"

        return if ! View.confirm "delete?: #{file}!"
        File.delete file
        return View.flash "- deleted: #{file}!"
      end


      # foo.rb/src, so save it ? ...

      if items = options[:items]
        return options[:output] = "| More than one source exists for this menu." if ! only_one_source

        txt = items[0]
        if txt =~ /\A\: .+\z/
          return options[:output] = "- Use pipe quotes, not colons!"
        end

        if items.length != 1 || items[0] !~ /\n/
          return options[:output] = "| Should either be no items or pipe-quoted lines?"
        end

        # Just open if as+open

        return options[:output] = "=open file/#{file}" if options[:prefix] == "open"

        File.open(file, "w") { |f| f << txt }
        return options[:output] = "<* - saved!"
      end

      # foo.rb, so show sources (all if extension doesn't match too)...

      # If . and only one source, replace it with the actual extension (unless C-u)
      if extension == "." && only_one_source && options[:prefix] != :u
        txt = "=replace/line/\n  #{sources[-1][0]}\n#{Tree.quote File.read(file), :indent=>'    ', :char=>'|'}"

      # If multiple sources, or different extension
      elsif extension == "." || !only_one_source || sources[-1][0][/\..+/] != extension
        return options[:output] = "| Different file matches. Try using just a dot with no extension, like:\n|\n| #{options[:name]}." if extension != "."

        txt = Xiki.expand "source", :ancestors=>[options[:path]]
      else

        if File.file? file
          txt = Tree.quote File.read(file), :char=>"|"
        else
          txt = Tree.quote "TODO: grab sample\ncode for extension from @sample_menus"
        end
      end

      options[:output] = txt

    end

    # Does actual invocation.  Finds all sources and delegates to handlers.
    def self.expand_menufied options
      # Probably do caching here, when we get to that point

      # Update :sources to have rest of sources from :containing_dir
      self.climb_sources options

      # Delegates to handlers according to source extensions
      self.handle options
    end


    # Climbs down menu source dir according to path, to find source files
    # eligible to handle the path.
    #
    # Command.climb_sources(:menufied=>"/tmp/foo/a/b", :items=>["a"])
    #   => sources: [["foo/", "foo.rb"], ["a/", "a.rb"]]
    def self.climb_sources options

      path, items, menufied, sources = options[:path], options[:items], options[:menufied], options[:sources]

      raise ":sources=>nil was passed to .climb_sources.  This should never happen.  Probably means no :expanders were found?  Try putting =expanders under the menu." if ! sources



      sources.pop if sources[-1] == :incomplete   # Remove :incomplete, since we're going to grab them all for this path

      # For each item...
      climbed_path = "#{menufied}"
      (items||[]).each do |item|

        break if sources[-1][0] !~ /\/$/   # If last source climbed doesn't have a dir

        # If there is a dir for it, grab files and dir that match

        climbed_path << "/#{item}"
        found = climbed_path !~ /\n/ && self.source_glob(climbed_path)

        break if ! found   # Stop if none found
        sources << found
      end

      options[:enclosing_source_dir] = Command.source_path options

      # Create :args, having :items that weren't sources
      if items
        args = items[sources.length-1..-1]
        options[:args] = args if args.any?
      end
      options
    end

    @@handlers = nil
    @@handlers_order = nil
    def self.handlers
      @@handlers ||= {
        "*"=>[ConfLoadingHandler],   # This should always run
        "conf"=>ConfHandler,   # This should always run

        "xiki"=>XikiHandler,

        "rb"=>RubyHandler,
        "menu"=>MenuHandler,

        "deck"=>DeckHandler,
        "steps"=>StepsHandler,

        "notes"=>NotesHandler,
        "outline"=>NotesHandler,

        "html"=>HtmlHandler,
        "markdown"=>MarkdownHandler,
        "bootstrap"=>BootstrapHandler,
        "txt"=>TxtHandler,
        "py"=>PythonHandler,
        "js"=>JavascriptHandler,
        "coffee"=>CoffeeHandler,
        "sh"=>ShHandler,
        "jpg"=>JpgHandler,
        "pgn"=>PgnHandler,
        "erb"=>ErbHandler,
        "/"=>DirHandler,
      }
    end

    def self.handlers_with_samples
      txt = File.read("#{Xiki.dir}roots/sample_menus/sample_menus_index.menu")
      extensions = txt.scan(/<name>\.(\w+)/).map{|o| o[0]}.uniq
    end

    # {
    #   "*"         => 0,
    #   "conf"      => 1,
    #   "rb"        => 2,
    #   "menu"      => 3,
    def self.handlers_order
      @@handlers_order ||= self.handlers.inject({}){|hash, kv| hash[kv[0]] = hash.length; hash}
    end

    def self.determine_handlers options

      sources = options[:sources][-1]
      options[:handlers] = ex = {}   # {"/"=>"a/", "rb"=>"a.rb"}

      handlers = self.handlers

      sources.each_with_index do |source, i|
        options[:source_index] = i
        key = source[/\w+$|\/$/]

        next if ! key

        if handlers[key]
          ex[key] = source
        else
          # For now > just don't txt handler or any handler > if we don't recognize it
          # Default to txt handler if no other
          # ex["txt"] = source
        end
      end
    end

    # Go through :sources and call appropriate handler for each
    def self.handle options

      sources = options[:sources][-1]

      raise "no sources?" if ! sources

      # Make 'ex' map from extensions to source files

      handlers = self.handlers

      # Always run "*" handlers
      handlers["*"].each do |handler|
        handler.handle options
      end

      self.determine_handlers options

      # TODO: Optimize this - use hash lookup for each extension
      #   Wait until we figure out final-ish way to register handlers
      #   Somehow sort keys based on below order

      handlers_order = self.handlers_order
      extensions = options[:handlers].keys.sort{|a, b| (handlers_order[a]||100000) <=> (handlers_order[b]||100000)}   # Sort by order in hash (it has the correct priority)

      # Call .handle for each applicable handler...

      extensions.each do |o|   #> |||||||||||||||||
        handler = handlers[o]

        # Assume .txt handler if none for this extension yet

        handler.handle options   #> ||||||||||||||||||
      end

      return options[:output] if options[:output] || options[:halt]

      if options[:client] =~ /^editor\b/ && sources.find{|o| o =~ /\.menu$/}

        # TODO: make sure it has actually changed before showing the below message.
        # Look up the menu really quick and compare.  If it's the same, flash something
        # like "no items yet", on return nothing.

        # Note: If you update this text, be sure update the code in =save menu/
        # that deletes it when saving.

        options[:output] = ""

        options[:no_search] = 1
        options[:no_slash] = 1
      end
      nil
    end

    # Returns subset of menus that are defined manually.
    def self.defs
      @@defs
    end

    # Populates :sources option but only those at the root level
    # to serve as a starting point.
    def self.root_sources_from_dir options
      found = self.source_glob options[:menufied]

      raise "Command source file or dir probably doesn't exist. Couldn't find source for: #{options}" if ! found

      # Ol "remove sources!"
      # Ol "found", found

      options[:sources] = [found, :incomplete]
    end


    # Finds the first dir in XIKI_PATH that has this menu.
    #
    # Xiki::Command.root_sources_from_path_env(:name=>"ip")[:sources]
    #   [["ip.rb"], :incomplete]
    #
    # Xiki::Command.root_sources_from_path_env(:name=>"ip")[:sources]
    # Xiki::Command.root_sources_from_path_env(:name=>"dd")[:menufied]
    # Xiki::Command.root_sources_from_path_env(:name=>"drdr")[:sources]   # multi-level
    # Xiki::Command.root_sources_from_path_env(:name=>"red")[:sources]   # none
    def self.root_sources_from_path_env options

      name = options[:name].sub(/^`/, "")

      # For each dir in path env...

      Xiki.xiki_path_dirs.each do |dir|
        # Grab sources if they exist...

        menufied = "#{dir}/#{name}"
        found = self.source_glob menufied

        next if ! found

        # Ol "remove sources!"

        options[:sources] = [found, :incomplete]
        options[:menufied] = menufied

        return options

      end
      options
    end

    def self.source_glob dir

      # Reverted "put notes in commands dir" implementation
      # was_notes = dir.sub!(/:([^\/]+)$/i, "\\1")   # Remove colon if a note

      name = File.basename dir

      dir = dir.gsub ' ', '[ -_]'   # For spaces in menus, match source files with underscores or dashes, etc
      name.gsub! ' ', '[ -_]'

      list = Dir.glob ["#{dir}/", "#{dir}.*", "#{dir}/index.*", "#{dir}/#{name}_index.*",
        "#{dir}/menu.*", "#{dir}/#{name}_menu.*"
      ]

      # :foo, so remove non-.notes sources
      # not :foo, so remove .notes sources
      # list = was_notes ?
      #   list.select{|o| o =~ /\.notes$/} :
      # list = list.select{|o| o !~ /\.notes$/}


      return nil if list.empty?


      containing_dir_length = dir[/.*\//].length
      list.each{|o| o.slice! 0..containing_dir_length-1}   # Chop off paths
      list
    end

    # Constructs path for nth source in :sources.
    # Command.source_path :menufied=>"/tmp/drdr", :sources=>[["drdr/", "drdr.rb"]]
    # Command.source_path :menufied=>"/tmp/drdr", :sources=>[["drdr/", "drdr.rb"], ["a/", "a.rb"]]
    def self.source_path options, nth=-2   # Default to last
      menufied = options[:menufied]

      path = "#{File.dirname menufied}/#{options[:sources][0..nth].map{|o| o[0]}.join("")}"

      path.sub /^\/\/+/, "/"   # Extraneous leading slash can be added when at root (File.dirname adds one, etc.)
    end

    # Returns the source files for a menu
    # Xiki::Command.source_files "ip"
    def self.source_files name
      options = {:name=>name}
      self.root_sources_from_path_env options
      return [] if ! options[:sources]
      self.climb_sources options
      options[:sources][0].map{|o| "#{options[:enclosing_source_dir]}#{o}"}
    end


    def self.format_name name
      result = name.gsub(/[ -]/, '_').downcase

      result.sub!(/\A_/, '-') if name =~ /\A-/   # Restore leading dash, if was there

      result
    end

    def self.completions name=""

      # Check defined menus...

      result = []
      Command.defs.keys.each do |key|
        result << key.gsub("_", ' ') if key =~ /^#{name}/
      end

      # Check XIKI_PATH menus...

      Xiki.xiki_path_dirs.each do |dir|
        start = "#{dir}/#{name}*"
        Dir.glob(start).each do |match|
          result << File.basename(match, ".*").gsub("_", ' ')
        end
      end
      result.sort.uniq
    end


    # Called when literal pattern for defining?
    # +menu
    def self.expand_define path, options

      options.delete :no_slash

      path.sub!(/\A([^\/]*)[*+^~?]/, "\\1")

      options = Expander.expanders path

      path = Path.split(path)

      # +foo/, so show contents...

      if path.length == 1
        # Shows all contents.  Maybe only do this when enter+all.

        # Remove * (at beginning or end)

        # Menu doesn't exist yet, so show message...

        if ! options[:sources]
          return "
            > Command doesn't exist yet
            | Create it by adding some items underneath
            <* example/
            <* other options/
            "
        end

        self.climb_sources options
        sources = options[:sources]

        # Error out if more than one array with more than one item
        only_one_source = sources.length == 1 && sources[0].length == 1
        raise "- More than one source. Probably make it delegate to =source?" if ! only_one_source

        file = "#{options[:enclosing_source_dir]}#{sources[-1][0]}"

        return File.read(file)
      end

      # If doesn't exist and they picked "other options", show them...

      if ! options[:sources] && path[1] == "other options"
        return "
          | Create this menu via
          - code/
          - items/
          - misc/
          | Borrow from old create
          =ziou2/
          "
      end

      # *foo/items, so do as+menu...

      Menu.as_command :no_blink=>1
    end


    def self.lispify tree

      tree = tree.strip

      lines = []
      Tree.traverse(tree) {|a, s| lines << a}
      lines << [:end]

      txt, last_indent = "", 0

      lines.each do |l|

        # Add closing parens and linebreak...

        if txt != ""   # Don't do on 1st one
          # Add a closing paren for each indent we're to the left of lower (plus 1)
          difference = (last_indent - l.length) +1
          difference.times{txt << ")"}
          txt << "\n"
        end

        next if l == [:end]

        item = l[-1].sub(/^[+-] /, '')
        item.sub! /\/$/, ''

        # Add this item...
        txt << "#{'  ' * l.length}(\"#{item}\""

        last_indent = l.length
      end

      txt

    end


  end

end
