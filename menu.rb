class Menu

  def self.menu
    '
    - history/
      - @log/
      - @last/
    - .setup/
      - .create/
        > Summary
        | To create a menu you can either create a .menu file in ~/menus, or a
        | class with a menu method.
        |
        - .menu_file/
        - .class_file/
        |
      - @~/menus/
      - .reload_menus/
    - docs/
      - .how_to_use/
      - .how_to_create/
      - .keys/
        > Summary
        Helpful keyboard shortcuts when using menus.
        |
        > Jump to file that implements menu
        unique_unique_. (Control-u Control-u Control-enter)
        |
        > To reload a menu class (a .rb file) after you change it
        unique_do_run
        |
    - api/
      > Summary
      How to use ruby code to define menus.
      |
      If you want to create a very simple menu you can do so without code,
      by just putting the menu in a file such as ~/menu/foo.menu. See:
      |
      - @menu/docs/how_to_create/
      |
      > Create your own menus
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
      - .make_classes_in_menu_dir/
      |
      | TODO: finish restructuring this
      |
    '
  end

  def self.menu_file
    Xiki.dont_search

    %`
    > 1) Create a menu file in ~/menus
    | Update this example to your liking, then create the file.
    | Do the $_return keyboard shortcut (Ctrl-4, Ctrl-return) while
    | on one of the quoted lines below to create it.
    |
    - @~/menus/
      - animals.menu
        | - dogs/
        |   - labs/
        | - cats/
        |   - siamese/
    |
    > 2) Try it out
    | Now, you can type "animals" (or whatever you named it) on a blank line
    | and double-click (or Ctrl-enter) to use it.  Or, try double-clicking
    | on it here:
    |
    + @animals/
    |
    `
  end

  def self.class_file
    Xiki.dont_search

    root = 'desserts'
    trunk = Xiki.trunk

    # If nested path (due to @), grab root of parent
    root = TextUtil.snake_case(trunk[-2][/^[\w -]+/]) if trunk.length > 1

    %`
    > 1. Create a class in ~/menus
    | Update the below class to your liking. Then do $+return (meaning type
    | Ctrl-4, Ctrl-return) with you cursor on the example to create it.
    |
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
    |
    > 2. Try it out
    | Now, you can type "#{root}" (or whatever you named it) on a blank line
    | and double-click (or Ctrl-enter) to use it.  Or, try double-clicking
    | on it here:
    |
    + @#{root}/
    |
    `
  end



  def self.make_classes_in_menu_dir *args
    %`
    > Using .rb files
    Or, you can make the menu be backed by a class, like this:
    |
    TODO: make 2 examples
    |   - simple one
    |   - more complex one (use the below)
    |     - collapsed by default
    |
    >> 1: make a file that looks something like this
    - @~/menus/
      - foo.rb
        | class Foo
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
    |
    TODO: move this to how_to_use/?
    |   - Or, do we need to leave the "Notice it doesn't show the dots" part here?
    |     - maybe move just some of it
    |
    >> 2: type "foo" on an empty line
    foo
    |
    >> 3: double-click on items to drill in
    | foo/
    |   - sammiches/
    |     - ham/
    |       - buy/
    |         - buying ham sammiches
    |
    ...or...
    |
    | foo/
    |   - checkout/
    |     - credit/
    |       - checking out as credit...
    |
    Notice it doesn't show the dots in the trees to the user (to make it look simpler).  But it still knows the dots mean to call methods.  It routes to methods and passes params appropriately.
    |
    `
  end

  def self.how_to_use *args
    %`
    > Summary
    | How to use Xiki menus.  Note this refers to the wiki-style menus, not the menu bar.
    |
    | All menus can be used the same way.  Just type something and
    | double-click on it (or type control-enter).
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
    %q`
    > Summary
    | How to make your own menus in Xiki.  Note this refers to the wiki-style
    | menus, not the menu bar.
    |
    > Using xiki menus
    | All xiki menus can be used the same way.  Just type something and
    | double-click on it (or type control-enter).
    |
    | For more details, see:
    - @menu/docs/how_to_use/
    |
    |
    | Ways of creating your own menus:
    |
    > Creating .menu files
    | You can make menus without code, by just put "whatever.menu" files in the
    | "menu/" dir in your home dir.
    |
    | For example you could create a "foo.menu" file with the contents
    | "- sammiches/..." etc:
    |
    - ~/menus/
      - foo.menu
        | - sammiches/
        |   - ham/
        |   - tofu/
        | - dranks/
        |   - foty/
    |
    | This makes a foo/ menu that you can expand.  Even though these menus
    | don't run code themselves, they can delegate to other menus or run code,
    | like:
    |
    - ~/menus/
      - foo.menu
        | - @mymenu/
        | - @MyClass.my_method
    |
    | To make menus dynamic, you can add lines like "- @mysql/start/" or
    | "- @Mysql.stop/" in menus like the above ones (to delegate to existing
    | dynamic menus or methods).
    |
    |
    @menu/api/
    |
    `
  end

  def self.reload_menus
    Launcher.reload_menu_dirs
    View.success
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
    block = Launcher.menus[root]
    return if block.nil?
    Tree.output_and_search block, :line=>"#{root}/#{rest}", :just_return=>1
  end

  def self.method_missing *args, &block
    Launcher.method_missing *args, &block
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

    root = trunk[-1][/^[\w _-]+/]
    root.gsub!(/[ -]/, '_') if root

    ([Xiki.dir]+Launcher::MENU_DIRS).reverse.each do |dir|
      next unless File.directory? dir
      file = Dir["#{dir}/#{root}.*"]
      next unless file.any?
      return View.open file[0]
    end

    Tree.<< "
      | No \"#{root}\" menu or class file found in these dirs:
      - @~/menus/
      - @$x/menus/
      ", :no_slash=>1
  end

  def self.external menu, options={}

    View.scroll_bars :off
    View.visibility("h")
    View.message ""

    View.wrap :off

    # IF nothing passed, must want to do tiny search box
    if menu.empty?
      View.dimensions("p")   # Shrink it to 1 line
      Launcher.open ""
      View.message ""
      View.prompt "Type anything", :timed=>1, :times=>2 #, :color=>:rainbow
      View.dimensions("c")
      $menu_resize = true
      Launcher.launch
    else
      View.dimensions("c")
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
    root = TextUtil.snake_case(root).sub(/^_+/, '')
    ignore, right = View.paragraph :bounds=>true, :start_here=>true
    # Go until end of paragraph (simple for now)
    Effects.blink :left=>left, :right=>right
    txt = View.txt left, right
    txt.sub! /.+\n/, ''
    txt.gsub! /^  /, ''

    # Remove help text if still there
    txt.sub!(/.+\n.+\n/, '') if txt =~ /\AModify this sample menu/

    path = File.expand_path "~/menus/#{root}.menu"

    file_existed = File.exists? path

    if file_existed
      treeb = File.read path
      txt = Tree.restore txt, treeb

      DiffLog.save_diffs :patha=>path, :textb=>txt
    end

    File.open(path, "w") { |f| f << txt }

    View.cursor = orig

    require_menu path

    View.success "- #{file_existed ? 'Updated' : 'Created'} ~/menus/#{root}.menu", :times=>4
    nil
  end

  @@loaded_already = {}

  def self.load_if_changed file
    return unless File.exists? file
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
end
