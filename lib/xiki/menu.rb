module Menu

  def self.menu
    '
    - history/
      - @log/
      - @last/
    - @all/
    - .create/
      - here/
      - class/
    - .install/
      - gem/
    - .setup/
      - @~/menu/
      - @$x/menu/
      - .reload_menus/
    - .api/
      > Summary
      | How to use ruby code to define menus.
      |
      | You can create sophisticated menus backed by classes, or by using other
      | simple means:
      - .classes/
        - .simple class/
        - .menu with method/
        - .menu with two methods/
      - other/
        - With a string/
          |
          |   Menu.fish :menu=>"- salmon/\n- tuna/\n  - yellow fin/"
          |
          Try it out by typing 1 do_ruby (C-1 Ctrl-d Ctrl-r) while on it, then
          double-clicking on this menu to see what happens:
          |
          @fish/
          |
        - Delegating to an existing menu/
          |
          |   Menu.critters :menu=>"foo/animals"
          |
          @critters/
          |
        - Using a block/
          |
          |   Menu.foo do
          |     "hey/"
          |   end
          |
          The block can optionally take a |path| param to handle multiple levels
          of nesting.
          |
          |   Menu.foo do |path|
          |     "hey/#{path}"
          |   end
          |
        - Extract menu text from somewhere/
          | Tree.children just expects text that is in the form of a menu (lines with
          | 2-space indenting for nesting). So, the text can be pulled from
          | anywhere, such as a part of a larger file:
          |
          | Menu.lawn do |path|
          |   menu = Notes.read_block("/tmp/garage.notes", "> Lawn")
          |   Tree.children menu, Tree.rootless(path)
          | end
          |
      |
      | If you want to create a very simple menu you can do so without code,
      | by just putting the menu in a file such as ~/menu/foo.menu. See:
      |
      << docs/creating/
    - .docs/
      - .using/
      - .creating/
    '
  end

  def self.install *args
    Xiki.dont_search
      '
      > TODO
      | implement this.

      - Look in gem dirs for installed gems with this name
      - Google search for xiki menu on web
        @google/"foo.menu"
      '
  end

  def self.create *args
    type = args[0]

    return self.create_here if type == "here"
    return self.create_class if type == "class"
    return self.create_more(*args.drop(1)) if type == "more"

    "- unknown option #{type} passed to .create!"
  end

  def self.create_here

    # TODO: Handle various use cases
      # "menu/create/here/" at left margin
      # "@menu/create/here/" nested
      # "menu/create/\n  here/" at left margin
      # "@menu/create/\n  here/" nested

    trunk = Xiki.trunk
    if wrapper = trunk[-2]   # If @menu/create/here is nested
      menu = Tree.root wrapper
    else   # If put it under a fake menu

      # What?  This is if it's not nested? - is this used?

      # TODO: Go to left margin and remove menu...

      Tree.to_root

      Tree.kill_under
      menu = "foo"
      Line.sub! /([ +-]*).*/, "\\1#{menu}/"
      # Insert it wherever we are
    end
    Xiki.dont_search

    name_text = menu == "foo" ?
      "and change '#{menu}' to something" :
      "to go under the '#{menu}' menu"

    snake = TextUtil.snake_case menu

    Tree << "
      | Supply a few items here. Then do as+menu (type Ctrl-a Ctrl-m) to create
      | the '#{menu}' menu. Or, just create '~/menu/#{snake}.menu' yourself.
      - example item/
        - another/
      - and another/
      "

    nil

  end

  def self.create_class
    trunk = Xiki.trunk
    if wrapper = trunk[-2]
      # Just do in-line
      menu = TextUtil.snake_case Tree.root(wrapper)
    else
      menu = 'foo'
    end

    Xiki.dont_search

    Tree << %`
      | Update this sample class to your liking. Then do as+update (type
      | Ctrl-a, Ctrl-u) to create the '#{menu}' class file.
      - @~/menu/
        - #{menu}.rb
          | class #{TextUtil.camel_case(menu)}
          |   def self.menu *args
          |     "- sample item/\\n- Args Passed: \#{args.inspect}\\n- Customize me in) @~/menu/#{menu}.rb"
          |   end
          | end
      - more examples) @menu/api/classes/
      `
    nil
  end

  def self.simple_class *args
    root = 'foo'
    trunk = Xiki.trunk
    root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1   # If nested path (due to @), grab root of parent

    %`
    - @~/menu/
      - #{root}.rb
        | class #{TextUtil.camel_case(root)}
        |   def self.menu *args
        |     "- sample item/\\n- Args passed: \#{args.inspect}\\n- Customize me in) @~/menu/#{menu}.rb"
        |   end
        | end
    `
  end

  def self.menu_with_method *args
    root = 'foo'
    trunk = Xiki.trunk
    root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1   # If nested path (due to @), grab root of parent

    %`
    - @~/menu/
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
    trunk = Xiki.trunk
    root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1   # If nested path (due to @), grab root of parent

    %`
    - @~/menu/
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
        | 1: Type "sammiches" on a line (the "@" isn't necessary when the line
        |    isn't indented)
        @sammiches
        |
        | 2: Double-click (or Ctrl-enter) on it to open it. You can try it on
        |    the line above.  It will look like this:
        | @sammiches/
        |   + meat/
        |   + veggie/
        |   + checkout/
        |
        | 3: Double-click to open items. It will look like this:
        | @sammiches/
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
        @http://www.youtube.com/watch?v=bUR_eUVcABg#t=1m30s
        @http://www.youtube.com/watch?v=bUR_eUVcABg#t=1m57s
        |
      - Creating .menu files/
        | You can make menus without code, by just putting "whatever.menu" files
        | in the "menu" dir in your home dir.
        |
        | For example you could create a "drinks.menu" file, like the following.
        | (The "|" characters aren't actually in the file).
        |
        | ~/menu/
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
        | "@" character.  For example:
        |
        | ~/menu/
        |   - foo.menu
        |     | - @other menu/
        |     | - @MyClass.my_method
        |     | @$ shell command
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
        | Create ruby files in ~/menu/ to make dynamic menus.  The .menu class
        | method will be invoked.  Example:
        |
        | ~/menu/
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
        | ~/menu/
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
        @menu/api/
        |
      `

    Tree.children(txt, args.join('/'))
  end

  def self.reload_menus
    Launcher.reload_menu_dirs
    View.flash
    nil
  end

  # Eval the menu path, and return the output (delegates to .call)
  # Examples:
  # @menu/api/other/With a string/
  def self.[] path
    path, rest = path.split '/', 2

    self.call path, rest
  end

  def self.call root, rest=nil
    root = root.gsub /[ +-]/, '_'
    menus = Launcher.menus
    block = menus[0][root] || menus[1][root]
    return if block.nil?
    Tree.output_and_search block, :line=>"#{root}/#{rest}", :just_return=>1
  end

  # TODO: After Unified, don't use this any more for defining methods.
  def self.method_missing *args, &block
    Launcher.method_missing *args, &block
    "- defined!"
  end

  # Deprecated after Unified refactor.
  # Superceded by Path.split
  #
  # Menu.split("a/b").should == ["a", "b"]
  # Menu.split("aa/|b/b/|c/c").should == ["aa", "|b/b", "|c/c"]
  # Menu.split("aa/|b/b/|c/c", :return_path=>1)
  # Menu.split("a/b/@c/d/", :outer=>1).should == ["a/b/", "c/d/"]
  def self.split path, options={}

    # :outer means split based on @'s...

    if options[:outer]

      # If /|... quote, pull it off during split, so quoted "@" won't mean ancestor
      # Deal with quoted | a/@b - split off \| before and add back to last after!"]
      quoted = nil
      if found = path =~ /\/\|/
        path = path.dup
        quoted = path.slice!(found..-1)
      end

      path = path.split /\/@ ?/
      (path.length-1).times {|i| path[i].sub! /$/, '/'}   # Add slashes back

      path[-1] << quoted if quoted

      return path
    end

    # Split based on /...

    path = path.sub /\/$/, ''
    path = Tree.rootless path if options[:return_path]

    return [] if path.empty?

    groups = path.split '/|', -1

    result = groups[0] =~ /^\|/ ?
      [groups[0]] :
      groups[0].split('/', -1)

    result += groups[1..-1].map{|o| "|#{o}"}
  end

  # Probably make this supercede to_menu.
  # Note this works for patterns as well as menus.
  def self.to_menu_unified
    path = Tree.path_unified
    options = Expander.expanders path

    source = Tree.source options

    return View.flash "- no source found!" if ! source

    # If was a string, show tree in new view...

    if source.is_a?(String)
      Launcher.open(source, :no_launch=>1)
      Launcher.enter_all   # Show dir recursively, or file contents in tree
      return
    end

    # Must be [file, line], so open and jump to line...

    View.open source[0]
    View.line = source[1] if source[1]
  end

  def self.to_menu

    prefix = Keys.prefix
    return self.to_menu_unified if prefix != :u

    # Get root...

    line = Line.without_label

    # Take best guess, by looking through dirs for root
    trunk = Xiki.path
    return View.<<("- You weren't on a menu\n  | To jump to a menu's implementation, put your cursor on it\n  | (or type it on a blank line) and then do as+menu (ctrl-a ctrl-m)\n  | Or, look in one of these dirs:\n  - ~/menu/\n  - $xiki/menu/") if trunk[-1].blank?
    root = trunk[0][/^[\w _-]+/]

    # Go to ancestor menu, or leaf menu?...

    # Go to leaf menu if cursor is on it, otherwise go to root
    root = trunk[-1][/^[\w _-]+/] if line =~ /^@/

    root.gsub!(/[ -]/, '_') if root
    root.downcase!

    # Try looking in each menu dir!...

    (["#{Xiki.dir}lib/"]+Launcher::MENU_DIRS).reverse.each do |dir|
      next unless File.directory? dir
      file = Dir["#{dir}/#{root}.*"]
      next unless file.any?

      View.open file[0]

      # Found, so try to find line we were on

      if line
        cursor = View.cursor
        View.to_highest
        line = Search.quote_elisp_regex line
        line.sub! /^(- |\\\+ )/, "[+-] \\.?"   # match if + instead of -, or dot after bullet
        found = Search.forward line
        Line.to_beginning
        View.cursor = cursor if ! found
      end

      return   # Break out of loop, since we found it
    end

    # Try dynamically-loaded procs!...

    #     message = "
    #       - No menu found:
    #         | No \"#{root}\" menu or class file found in these dirs:
    #         @ ~/menu/
    #         @ $x/menu/
    #         ".unindent

    # Should be able to get it right from proc

    proc = Launcher.menus[1][root]

    return View.flash "- Menu 'root' doesn't exist!", :times=>4 if ! proc

    location = proc.source_location # ["./firefox.rb", 739]

    # location[0].sub! /^\.\//, Xiki.dir
    View.open location[0].sub(/^\.\//, Xiki.dir)
    View.line = location[1]

  end

  def self.external menu, options={}

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

  def self.as_menu
    orig = View.cursor

    Tree.to_root

    root, left = Line.value, View.cursor
    root = Line.without_label :line=>root

    root = TextUtil.snake_case(root).sub(/^_+/, '')

    # TODO: Update this for new text after Unified (possibly there probably won't be any text)
    if Line.value(2) =~ /^ +\| Supply a few items here/   # If sample text, remove
      Line.next
      while Line.=~(/^ +\| /)
        Line.delete
      end
      Line.previous
      orig = nil
    end

    Tree.after_children
    right = View.cursor
    View.cursor = left

    # Go until end of paragraph (simple for now)
    Effects.blink :left=>left, :right=>right
    txt = View.txt left, right
    txt.sub! /.+\n/, ''
    txt.gsub! /^  /, ''
    txt.unindent

    return Tree << "| You must supply something to put under the '#{root}' menu.\n| First, add some lines here, such as these:\n- line/\n- another line/\n" if txt.empty?

    path = File.expand_path "~/menu/#{root}.menu"

    file_existed = File.exists? path

    if file_existed
      treeb = File.read path
      txt = Tree.restore txt, treeb

      DiffLog.save_diffs :patha=>path, :textb=>txt
    end

    txt = txt.unindent

    File.open(path, "w") { |f| f << txt }

    View.cursor = orig if orig

    View.flash "- #{file_existed ? 'Updated' : 'Created'} ~/menu/#{root}.menu", :times=>3
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
  def self.collapser_launcher
    line = Line.value
    arrows = line[/<+/].length
    arrows -= 1 if arrows > 1   # Make "<<" go back just 1, etc.

    line = Line.without_label :line=>line

    skip = line.empty? && arrows - 1

    Line.sub! /^(  +)<+ .+/, "\\1- "   # Delete after bullet to prepare for loop

    arrows.times do |i|

      # If no items left on current line, jump to parent and delete
      if Line =~ /^[ +-]+$/
        Tree.to_parent
        Tree.kill_under
        Move.to_end
      end

      unless i == skip   # Remove last item, or after bullet if no items
        Line.sub!(/\/[^\/]+\/$/, '/') || Line.sub!(/^([ @+-]*).*/, "\\1")
      end
    end

    if Line.indent.blank?
      line.sub! /^@ ?/, ''
      Line.sub! /^@ ?/, ''
    end

    Line << line unless skip
    Launcher.launch_unified
  end

  def self.root_collapser_launcher

    View.cursor


    # Grab line
    line = Line.value

    arrows = line[/<+/].length

    line.sub!(/ *<+@ /, '')

    # Go up to root, and kill under
    arrows.times { Tree.to_root }
    Tree.kill_under

    # Insert line, and launch
    old = Line.delete :leave_linebreak
    old.sub! /^( *).+/, "\\1"
    old << "@" if old =~ /^ /   # If any indent, @ is needed
    View << "#{old}#{line}"

    Launcher.launch_unified
  end

  def self.replacer_launcher
    Line.sub! /^( +)<+= /, "\\1+ "

    # Run in place, grab output, then move higher and show output

    orig = View.line
    Launcher.launch_unified :no_search=>1

    # If didn't move line, assume it had no output, and it's collapse things itself
    return if orig == View.line

    # If it inserted something
    # output = Tree.siblings :everything=>1
    output = Tree.siblings :cross_blank_lines=>1, :everything=>1

    # Shouldn't this be looping like self.collapser_launcher ?
    Tree.to_parent
    Tree.to_parent
    Tree.kill_under :no_plus=>1
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

  #   def self.config txt, *args

  #     # TODO: implement
  #       # Args look like sample invocation below
  #       # If not there, create it first, using supplied default
  #       # Insert quoted file contents to be edited

  #     # Sample invocation
  #     #     Menu.config "
  #     #       - @ ~/xiki_config/browser.notes
  #     #         | - default browser:
  #     #         |   - Firefox
  #     #         | - others:
  #     #         |   - Safari
  #     #         |   - Chrome
  #     #       ", *args

  #     "TODO"
  #   end

  # Moves item to root of tree (replacing tree), then launches (if appropriate).
  def self.do_as_menu
    prefix = Keys.prefix :clear=>1   # Check for numeric prefix
    launch = false
    line = Line.value


    txt =
      if line =~ /^ +\| +def (self\.)?(\w+)/   # If on a quoted method, construct Foo.bar
        Ruby.quote_to_method_invocation
      elsif line =~ /^ *\|/   # If on quoted line, will grab all quoted siblings and unquote
        Tree.siblings :string=>1
      elsif line =~ /^[ +-]*@/ && Tree.has_child?   # If on ^@... line and there's child on next line...
        # Will grab the whole tree and move it up
        Tree.subtree.unindent.sub(/^[ @+-]+/, '')
      else
        launch = true
        Tree.path.last
      end

    Tree.to_root(:highest=>1)

    #     Keys.prefix_u ? Tree.to_root : Tree.to_root(:highest=>1)
    launch = false if prefix == :u

    Tree.kill_under

    Line.sub! /^([ @]*).+/, "\\1#{txt}"

    return if ! launch

    Launcher.launch_unified
  end


  # The following 3 methods are for the menu bar
  #   - a different use of the "Menu" class
  # TODO move them into menu_bar.rb ?

  def self.add_menubar_menu *name
    menu_spaces = name.join(' ').downcase
    menu_dashes = name.join('-').downcase
    name = name[-1]

    lisp = %Q<
      (define-key global-map
        [menu-bar #{menu_spaces}]
        (cons "#{name}" (make-sparse-keymap "#{menu_dashes}")))
    >
    $el.el4r_lisp_eval lisp

    menu = $el.elvar.menu_bar_final_items.to_a
    $el.elvar.menu_bar_final_items = menu.push(name.downcase.to_sym)
  end

  def self.add_menubar_item menu, name, function

    menu_spaces = menu.join(' ').downcase
    lisp = "
      (define-key global-map
        [menu-bar #{menu_spaces} #{function}]
        '(\"#{name}\" . #{function}))
    "
    $el.el4r_lisp_eval lisp
  end

  ROOT_MENU = 'Keys'

  def self.init

    return if ! $el

    Mode.define(:menu, ".menu") do
      Notes.mode
    end

    add_menubar_menu ROOT_MENU

    menus = [
      [ROOT_MENU, 'To'],
      [ROOT_MENU, 'Open'],
      [ROOT_MENU, 'Layout'],
      [ROOT_MENU, 'As'],
      [ROOT_MENU, 'Enter'],
      [ROOT_MENU, 'Do'],
      [ROOT_MENU, 'Search']
    ]
    menus.reverse.each do |tuple|
      add_menubar_menu tuple[0], tuple[1]
    end
  end

  #
  # Whether line exists in menu
  #
  # p Menu.line_exists? "menu name", /^- text to add$/
  # p Menu.line_exists? "menu name", /^- text to (.+)$/
  #
  def self.line_exists? name, pattern #, options={}
    name.gsub! /\W/, '_'
    dir = File.expand_path "~/menu3"
    file = File.expand_path "#{dir}/#{name}.menu3"
    txt = File.read(file) rescue ""
    txt =~ pattern ? ($1 || $&) : nil   # Return whole string or group
  end

  #
  # Create simple .menu file if it doesn't exist, otherwise add line to it.
  #
  # Menu.append_line "menu name", "- text to add"
  #
  def self.append_line name, addition #, options={}

    name.gsub! /\W/, '_'

    # Default to ~/menu
    # If menu there, create, otherwise append

    # Get existing
    dir = File.expand_path "~/menu"
    Dir.mkdir dir if ! File.exists? dir

    file = File.expand_path "#{dir}/#{name}.menu"
    txt = File.read(file) rescue ""

    if txt =~ /^#{Regexp.escape addition}$/
      return ".flash - was already there!"
    end

    # Append to end (might be blank)

    txt << "#{addition}\n"

    # Save
    File.open(file, "w") { |f| f << txt }

    "- saved setting!"
  end






  # Unified refactor from here on down...
  # TODO: Probably extract from here on down into MenuExpander!





  # Examples: "ip"=><instance>, "tables"=>"/etc/menus/tables"
  @@defs ||= {}

  #   def self.menu
  #     "
  #     > TODO: add docs here
  #     - todo
  #     > See
  #     << menu path/
  #     << defs/
  #     "
  #   end

  # Adds to :expanders if :name is backed by a menu.
  def self.expands? options

    # If no name, maybe an inline menufied path, so set sources...

    if ! options[:name]
      if options[:menufied]
        self.root_sources_from_dir options
        (options[:expanders] ||= []).push self
        return
      end
      return   # Can't handle if wasn't inline and no :name
    end

    # See if name is directly defined...

    if implementation = @@defs[options[:name]]
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
      raise "Don't know how to deal with: #{implementation.inspect}" if ! kind

      options[kind] = implementation
      return (options[:expanders] ||= []).push self
    end

    # Try to look name up in PATH env...

    (options[:expanders] ||= []).push(Menu) if self.root_sources_from_path_env(options)[:sources]   # Found it if we created :sources


      #
      # > Discussion of future caching
      # TODO: cache output of .root_sources_from_path_env outputs whet it starts to take up time
      # - clear cache when updated by guard - think through guard strategy - probably gurad just builds one big file upon updates, and xiki checks only that file's mod date, and reloads (if Xiki.caching = :optimized
      # caching - punt for now
      # - if :all, store hash with output of .root_sources_from_path_env for each name
      # - if :optimized, check date of /tmp/xiki_path_env_dir_cache
      # - if :off (maybe rails dev mode - not worth setting up guard)
      #

    nil   # We couldn't find anything, so continue onto patterns

  end

  # Expand thing (represented by the hash) like a menu.  Could be a block, or
  # menufied (has sources).
  def self.expand options

    # Get simple invocations out of the way (block or class)...

    # If it's a proc, just call
    return options[:output] = options[:proc].call(options[:items] || [], options) if options[:proc]

    # If it's a class, just call (or, wait, need to include .menu file - probably no)
    raise "TODO: implement dealing with a class" if options[:class]

    if ! options[:menufied]   # Assume Menu.expands? should have pulled this out for now
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
  # Menu.climb_sources(:menufied=>"/tmp/foo/a/b", :items=>["a"])
  #   => sources: [["foo/", "foo.rb"], ["a/", "a.rb"]]
  def self.climb_sources options

    path, items, menufied, sources = options[:path], options[:items], options[:menufied], options[:sources]

    sources.pop   # Remove :incomplete, since we're going to grab them all for this path

    # For each item...

    climbed_path = "#{menufied}"
    (items||[]).each do |item|

      break if sources[-1][0] !~ /\/$/   # If last source climbed doesn't have a dir

      # If there is a dir for it, grab files and dir that match

      climbed_path << "/#{item}"
      found = self.source_glob climbed_path

      break if ! found   # Stop if none found
      sources << found
    end

    options[:last_source_dir] = Menu.source_path options

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
      "rb"=>RubyHandler,
      "menu"=>MenuHandler,
      "notes"=>NotesHandler,
      "html"=>HtmlHandler,
      "markdown"=>MarkdownHandler,
      "boostrap"=>BoostrapHandler,
      "txt"=>TxtHandler,
      "py"=>PythonHandler,
      "js"=>JavascriptHandler,
      "coffee"=>CoffeeHandler,
      "/"=>DirHandler,
      }
  end

  def self.handlers_order
    @@handlers_order ||= self.handlers.inject({}){|hash, kv| hash[kv[0]] = hash.length; hash}
  end

  # Go through :sources and call appropriate handler for each
  def self.handle options
    sources = options[:sources][-1]

    # Make 'ex' map from extensions to source files

    ex = {}   # {"/"=>"a/", "rb"=>"a.rb"}
    sources.each do |source|
      key = source[/\w+$|\/$/]
      ex[key] = source if key
    end

    # TODO: Optimize this - use hash lookup for each extension
    #   Wait until we figure out final-ish way to register handlers
    #   Somehow sort keys based on below order

    self.handlers_order   # Ensure @@ vars are set

    # Always run "*" handlers
    @@handlers["*"].each do |handler|
      handler.handle options, ex
    end

    extensions = ex.keys.sort{|a, b| (@@handlers_order[a]||100000) <=> (@@handlers_order[b]||100000)}   # Sort by order in hash (it has the correct priority)

    extensions.each do |o|
      handler = @@handlers[o]
      next if ! handler
      handler.handle options, ex
    end

    return options[:output] if options[:output] || options[:halt]

    # If we get through everything and just a dir...

    # Old (we're kind of handling this in the invoker now):
    # Maybe we don't need to show no output?
    # - If we do, probably go back to using :silent_output option
    #   - This means .menu_after has to set :silent_output=>1 to avoid seeing "no output"
    #     - Does it necessarily?
    #       - No, we could probably auto-set it when menu_after returns something!
    #     options[:output] = "@flash/- no output!"

    nil
  end

  def self.defs
    @@defs
  end

  def self.root_sources_from_dir options
    found = self.source_glob options[:menufied]

    raise "Couldn't find source for: #{options}" if ! found

    options[:sources] = [found, :incomplete]
  end


  # Finds the first dir in MENU_PATH that has this menu.
  #
  # Menu.root_sources_from_path_env(:name=>"dd")[:sources]
  # Menu.root_sources_from_path_env(:name=>"drdr")[:sources]   # multi-level
  # Menu.root_sources_from_path_env(:name=>"red")[:sources]   # none
  def self.root_sources_from_path_env options

    name = options[:name]

    # For each dir in path env...

    Xiki.menu_path_dirs.each do |dir|
      # Grab sources if they exist...

      menufied = "#{dir}/#{name}"
      found = self.source_glob menufied

      next if ! found

      options[:sources] = [found, :incomplete]
      options[:menufied] = menufied

      return options

    end
    options
  end

  def self.source_glob dir
    list = Dir.glob ["#{dir}/", "#{dir}.*", "#{dir}/index.*"]
    return nil if list.empty?

    containing_dir_length = dir[/.*\//].length
    list.each{|o| o.slice! 0..containing_dir_length-1}   # Chop off paths
    list
  end

  # Constructs path for nth source in :sources.
  # Menu.source_path :menufied=>"/tmp/drdr", :sources=>[["drdr/", "drdr.rb"]]
  # Menu.source_path :menufied=>"/tmp/drdr", :sources=>[["drdr/", "drdr.rb"], ["a/", "a.rb"]]
  def self.source_path options, nth=-2   # Default to last
    menufied = options[:menufied]

    path = "#{File.dirname menufied}/#{options[:sources][0..nth].map{|o| o[0]}.join("")}"

    path.sub /^\/\/+/, "/"   # Extraneous leading slash can be added when at root (File.dirname adds one, etc.)
  end

  def self.format_name name
    name.gsub(/[ -]/, '_').downcase
  end

end

Menu.init   # Define mode
