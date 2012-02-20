class Menu

  def self.menu
    '
    - history/
      - @log/
      - @last/
    - .create/
      - here/
      - class/
    - .setup/
      - @~/menus/
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
      << docs/how_to_create/
    - .docs/
      - .How to use/
      - .How to create/
      - .keys/
        > Summary
        | Helpful keyboard shortcuts when using menus.
        |
        | - as+menu
        |   - Save changes to menu (or create new one)
        | - to+menu
        |   - Jump to file that implements menu
        |
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
      | the '#{menu}' menu. Or, just create '~/menus/#{snake}.menu' yourself.
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
      - @~/menus/
        - #{menu}.rb
          | class #{TextUtil.camel_case(menu)}
          |   def self.menu *args
          |     "- Args passed in: \#{args.inspect}\\n- Customize me in) @ ~/menus/#{menu}.rb"
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
    - @~/menus/
      - #{root}.rb
        | class #{TextUtil.camel_case(root)}
        |   def self.menu *args
        |     "- Args passed in: \#{args.inspect}\n- Customize me in) @ ~/menus/#{menu}.rb"
        |   end
        | end
    `
  end

  def self.menu_with_method *args
    root = 'foo'
    trunk = Xiki.trunk
    root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1   # If nested path (due to @), grab root of parent

    %`
    - @~/menus/
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
    - @~/menus/
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

  def self.how_to_use *args
    %`
    > Summary
    | How to use Xiki menus.  Note this refers to the wiki-style menus, not the menu bar.
    |
    | All menus can be used the same way.  Just type something and double-click
    | on it (or type Ctrl-enter while the cursor is on the line).
    |
    - example/
      | 1: type "foo" on a line (the "@" isn't necessary when the line isn't indented)
      @ foo
      |
      | 2: double-click on it to drill in. You can try it on the line above. It will look like this:
      @ foo/
      |  - sammiches/
      |  - dranks/
      |
      | 3: double-click to drill in further. It will look like this:
      @ foo/
      |  - sammiches/
      |    - ham/
      |    - tofu/
      |  - dranks/
      |
    - using the mouse/
      | You can click on the "bullets" (the - and + at the beginnings of lines)
      | to expand and collapse.  You can also double-click to expand and
      | collapse.
      |
    - search to narrow down/
      | When you double-click a line the cursor turns blue and you can type
      | letters to search and narrow down the list.
      |
    - misc keys/
      | - Return: stops searching and launches (expands file or dir)
      | - Tab: like return but hides others
      | - ;: like return but collapses path
      |
      | - C-g: stops searching
      |
      | - Arrow keys: you can use them to go up and down and expand and collapse
      |
    `
  end

  def self.how_to_create *args
    txt = %q`
      > Summary
      | How to make your own menus in Xiki.  Note this refers to wiki-style
      | menus (such as this one), not the menu bar.
      |
      - Creating .menu files/
        | You can make menus without code, by just put "whatever.menu" files in the
        | "menu/" dir in your home dir.
        |
        | For example you could create a "foo.menu" file with the contents
        | "- sammiches/..." etc:
        |
        - TODO: get these to expand out somehow! - maybe pass another arg to Tree.children below? - probably bad idea
        - ~/menus/
          - foo.menu
            | - sammiches/
            |   - ham/
            |   - tofu/
            | - dranks/
            |   - foty/
        |
      - Delegating/
        | This makes a foo/ menu that you can expand.  Even though these menus
        | don't run code themselves, they can delegate to other menus or run code,
        | like:
        |
        - ~/menus/
          - foo.menu
            | - @mymenu/
            | - @MyClass.my_method
        |
      `

    Tree.children(txt, args.join('/'))
  end

  def self.reload_menus
    Launcher.reload_menu_dirs
    View.flash
    nil
  end

  # Other .init mode defined below
  def self.init
    Mode.define(:menu, ".menu") do
      Notes.mode
    end
  end

  def self.[] path
    root, rest = path.match(/^(.+?)\/(.+)/)[1..2]   # Grab thing to match
    self.call root, rest
  end

  def self.call root, rest=nil
    menus = Launcher.menus
    block = menus[0][root] || menus[1][root]
    return if block.nil?
    Tree.output_and_search block, :line=>"#{root}/#{rest}", :just_return=>1
  end

  def self.method_missing *args, &block
    Launcher.method_missing *args, &block
    "- defined!"
  end

  def self.split path, options={}
    path.sub! /\/$/, ''
    path = Tree.rootless path if options[:rootless]

    return [] if path.empty?

    groups = path.split '/|', -1

    result = groups[0] =~ /^\|/ ?
      [groups[0]] :
      groups[0].split('/', -1)

    result += groups[1..-1].map{|o| "|#{o}"}
  end

  def self.open_related_file
    # Take best guess, by looking through dirs for root
    trunk = Xiki.trunk

    return View.<<("- You weren't on a menu\n  | To jump to a menu's implementation, put your cursor on it\n  | (or type it on a blank line) and then do as+menu (ctrl-a ctrl-m)\n  | Or, look in one of these dirs:\n  - ~/menus/\n  - $xiki/menus/") if trunk[-1].blank?

    root = trunk[0][/^[\w _-]+/]

    root = trunk[-1][/^[\w _-]+/] if ! Keys.prefix_u

    root.gsub!(/[ -]/, '_') if root

    ([Xiki.dir]+Launcher::MENU_DIRS).reverse.each do |dir|
      next unless File.directory? dir
      file = Dir["#{dir}/#{root}.*"]
      next unless file.any?
      return View.open file[0]
    end

    message = "
      - No menu found:
        | No \"#{root}\" menu or class file found in these dirs:
        @ ~/menus/
        @ $x/menus/
        ".unindent

    proc = Launcher.menus[1][root]
    message << "  |\n  > Here's what it does:\n  | #{proc.to_ruby}" if proc

    Tree.<< message, :no_slash=>1
  end

  def self.external menu, options={}

    View.scroll_bars = false

    Window.visibility('high')
    View.message ""

    View.wrap :off

    # IF nothing passed, must want to do tiny search box
    if menu.empty?
      Window.dimensions "presets", "prompt"
      Launcher.open ""
      View.message ""
      View.prompt "Type anything", :timed=>1, :times=>2 #, :color=>:rainbow

      View.hide_others :all=>1
      Window.dimensions "presets", "center"

      $menu_resize = true
      Launcher.launch
    else
      View.hide_others :all=>1
      Window.dimensions "presets", "center"
      Launcher.open menu, options
    end
  end

end

Menu.init   # Define mode



# Below is a different use of the "Menu" class - the "Xiki" menu at the top of emacs

# TODO move into menu_bar.rb



class Menu
  def self.add_menu *name
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

  def self.add_item menu, name, function

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

    add_menu ROOT_MENU

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
      add_menu tuple[0], tuple[1]
    end
  end

  def self.as_menu
    orig = View.cursor

    Move.to_end   # In case we're at root of tree (search would make it go elsewhere)
    Search.backward("^[^ \n]")   # Start at root
    root, left = Line.value, View.cursor
    root = Line.without_label(:line=>root)

    root = TextUtil.snake_case(root).sub(/^_+/, '')

    if Line.value(2) =~ /^ +\| Supply a few items here/   # If sample text, remove
      Line.next
      while Line.=~(/^ +\| /)
        Line.delete
      end
      Line.previous
      orig = nil
    end

    ignore, right = View.paragraph :bounds=>true, :start_here=>true

    # Go until end of paragraph (simple for now)
    Effects.blink :left=>left, :right=>right
    txt = View.txt left, right
    txt.sub! /.+\n/, ''
    txt.gsub! /^  /, ''
    txt.gsub! /^\|$/, ''

    return Tree << "| You must supply something to put under the '#{root}' menu.\n| First, add some lines here, such as these:\n- line/\n- another line/\n" if txt.empty?

    path = File.expand_path "~/menus/#{root}.menu"

    file_existed = File.exists? path

    if file_existed
      treeb = File.read path
      txt = Tree.restore txt, treeb

      DiffLog.save_diffs :patha=>path, :textb=>txt
    end

    File.open(path, "w") { |f| f << txt }

    View.cursor = orig if orig

    require_menu path

    View.flash "- #{file_existed ? 'Updated' : 'Created'} ~/menus/#{root}.menu", :times=>4
    nil
  end

  @@loaded_already = {}

  def self.load_if_changed file
    return :not_found if ! File.exists?(file)
    previous = @@loaded_already[file]
    recent = File.mtime(file)

    if previous == nil
      require file
      @@loaded_already[file] = recent
      return
    end

    return if recent <= previous

    load file
    @@loaded_already[file] = recent
  end

  def self.collapser_launcher

    line = Line.value
    arrows = line[/<+/].length
    arrows -= 1 if arrows > 1   # Make "<<" go back just 1, etc.

    #     line.sub! /(^ +)= /, "\\1< "   # Temporarily get "=" to work too
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
    end

    Line << line unless skip
    Launcher.launch

  end

  def self.config txt, *args

    # TODO: implement
      # Args look like sample invocation below
      # If not there, create it first, using supplied default
      # Insert quoted file contents to be edited

    # Sample invocation
    #     Menu.config "
    #       - @ ~/xiki/config/browser.notes
    #         | - default browser:
    #         |   - Firefox
    #         | - others:
    #         |   - Safari
    #         |   - Chrome
    #       ", *args

    "TODO"
  end

  def self.replacer_launcher
    Line.sub! /^( +)<+= /, "\\1+ "

    # Run in place, then move higher

    Launcher.launch

    output = Tree.siblings(:everything=>1)
    Tree.to_parent
    Tree.to_parent
    Tree.kill_under :no_plus=>1
    Tree << output

  end

end
