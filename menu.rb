class Menu

  def self.menu
    '
    - reload) @Launcher.reload_menu_dirs
    - dirs/
      - user defined menus) ~/menus/
    - docs/
      - .how_to_use/
      - .how_to_create/
      - .keys/
        | > Summary
        | Helpful keyboard shortcuts when using menus.
        |
        | > Jump to file that implements menu
        | unique_unique_. (Control-u Control-u Control-enter)
        |
        | > To reload a menu class (a .rb file) after you change it
        | unique_do_run
        |
    - api/
      | > Summary
      | How to use the Menu class to define menus.
      |
      | > With a string
      @Menu.fish :menu=>"- tuna/\n- salmon/\n- blowfish/"
      |
      | Try it out by double-clicking on it, then double-clicking on the menu
      | to see what happens:
      |
      @fish/
      |
      |
      | > Delegating to an existing menu
      @Menu.sandwitches :menu=>"food/sammiches"
      |
      |
      | > Using a block
      | Menu.foo do
      |   "hey/"
      | end
      |
      | The block can optionally take a |path| param to handle multiple levels
      | of nesting.
      |
      | Menu.foo do |path|
      |   "hey/#{path}"
      | end
      |
      |
      | TODO: finish restructuring this
      |
      |
    '
  end

  def self.how_to_use *args
    %`
    | > Summary
    | How to make your own menus in Xiki.  Note this refers to the
    | wiki-style menus, not the menu bar.
    |
    | All menus can be used the same way.  Just type something and
    | double-click on it (or type control-enter).
    |
    | TODO
    |
    | >> 2: type "foo" on a line
    | foo
    |
    | >> 3: double-click on it to drill in
    | foo/
    |   - sammiches/
    |   - dranks/
    |
    | >> 4: double-click to drill in further
    | foo/
    |   - sammiches/
    |     - ham/
    |     - tofu/
    |   - dranks/
    |
    | > Using the mouse
    | TODO
    |
    | > Search to narrow down
    | TODO
    |
    | > Misc keys
    | TODO
    | return (launches)
    | tab (hides siblings, then launches)
    | ; (collapses into parent, then launches)
    |
    | > Arrow keys
    | TODO
    `
  end

  def self.how_to_create *args
    %q`
    | > Summary
    | How to make your own menus in Xiki.  Note this refers to the wiki-style menus, not the menu bar.
    |
    | > Using .menu files
    | All menus can be used the same way.  Just type something and double-click on it (or type control-enter).
    |
    | For more details, see:
    + @menu/docs/how_to_use/
    |
    |
    | Ways of creating your own menus:
    |
    | > Using .menu files
    | You can make menus without code, by just put "whatever.menu" files in the "menu/" dir in your home dir.
    |
    | - ~/menus/
    |   - foo.menu
    |     | - sammiches/
    |     |   - ham/
    |     |   - tofu/
    |     | - dranks/
    |     |   - foty/
    |
    | This makes a foo/ menu that you can expand.  Even though these menus
    | don't run code themselves, they can delegate to other menus or run code, like:
    |
    | - ~/menus/
    |   - foo.menu
    |     | - @mymenu/
    |     | - @MyClass.my_method
    |
    | To make menus dynamic, you can add lines like "- @mysql/start/" or "- @Mysql.stop/" in menus like the above ones (to delegate to existing dynamic menus or methods).
    |
    |
    | > Using .rb files
    | Or, you can make the menu be backed by a class, like this:
    |
    | >> 1: make a file that looks something like this
    | - ~/menus/
    |   - foo.rb
    |     | class Foo
    |     |   def self.menu
    |     |     "
    |     |     - sammiches/
    |     |       - ham/
    |     |         - .buy/
    |     |       - tofu/
    |     |         - .buy/
    |     |     - .checkout/
    |     |       - cash/
    |     |       - credit/
    |     |     "
    |     |   end
    |     |   def self.buy category, item
    |     |     "- buying \#{item} \#{category}"
    |     |   end
    |     |   def self.checkout kind
    |     |     "- checking out as \#{kind}..."
    |     |   end
    |     | end
    |
    |
    | TODO: move this to how_to_use/?
    |   - Or, do we need to leave the "Notice it doesn't show the dots" part here?
    |     - maybe move just some of it
    |
    | >> 2: type "foo" on an empty line
    | foo
    |
    | >> 3: double-click on items to drill in
    | foo/
    |   - sammiches/
    |     - ham/
    |       - buy/
    |         - buying ham sammiches
    |
    | ...or...
    |
    | foo/
    |   - checkout/
    |     - credit/
    |       - checking out as credit...
    |
    | Notice it doesn't show the dots in the trees to the user (to make it look simpler).  But it still knows the dots mean to call methods.  It routes to methods and passes params appropriately.
    |
    |
    | > Programmatically, using the Menu class
    | See:
    @menu/api/
    |
    `
  end

  # Other .init mode defined below
  def self.init
    Mode.define(:menu, ".menu") do
      Notes.mode
    end
  end

  def self.[] path
    root = path[/^\w+/]   # Grab thing to match
    block = Launcher.launchers_paths[root]
    return if block.nil?
    Launcher.output_and_search block, :line=>path
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

  def self.init

    add_menu 'Xiki'

    menus = [
      ['Xiki', 'To'],
      ['Xiki', 'Open'],
      ['Xiki', 'Layout'],
      ['Xiki', 'As'],
      ['Xiki', 'Enter'],
      ['Xiki', 'Do'],
      ['Xiki', 'Search']
    ]
    menus.reverse.each do |tuple|
      add_menu tuple[0], tuple[1]
    end
  end

  def self.as_home_menu
    orig = View.cursor

    Move.to_end   # In case we're at root of tree (search would make it go elsewhere)
    Search.backward("^[^ \n]")   # Start at root
    root, left = Line.value, View.cursor
    root = TextUtil.snake_case(root).sub(/^_+/, '')
    ignore, right = View.paragraph :bounds=>true, :start_here=>true
    # Go until end of paragraph (simple for now)
    txt = View.txt left, right
    txt.sub! /.+\n/, ''
    txt.gsub! /^  /, ''

    path = File.expand_path "~/menus/#{root}.menu"

    file_existed = File.exists? path

    if file_existed
      treeb = File.read path
      txt = Tree.restore txt, treeb

      DiffLog.save_diffs :patha=>path, :textb=>txt
    end

    File.open(path, "w") { |f| f << txt }

    View.cursor = orig

    View.success "- #{file_existed ? 'Updated' : 'Created'} menu: #{root}/", :times=>3
    nil
  end

end
