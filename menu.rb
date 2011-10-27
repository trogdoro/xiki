class Menu
  def self.init
    Mode.define(:deck, ".menu") do
      Notes.mode
    end
  end
end

Menu.init   # Define mode




# Below is a different use of the "Menu" class - the "Xiki" menu at the top of emacs





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
    #puts lisp
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
    #puts lisp
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
      #       puts "#{tuple[0]}, #{tuple[1]}"
      add_menu tuple[0], tuple[1]
    end
  end

  def self.menu
    "
    - reload) @Launcher.reload_menu_dirs
    - docs/
      - .how_to_extend/
    "
  end

  def self.how_to_extend *args
    "
    > Summary
    How to make your own menus in Xiki.

    > Using a block
    >> 1. define a launcher
    Launcher.foo do
      \"hey/\"
    end

    >> 2. type \"foo\" on a blank line
    foo

    >> 3. double-click on it to drill in
    foo/
      hey/

    The block can optionally take a |path| param to nest multiple levels.

    > Using .menu files
    To make menus without code, you can now just put \"whatever.menu\" files in your home dir.

    >> 1: make a file like this
    - ~/menus/
      - foo.menu
        | - sammiches/
        |   - ham/
        |   - tofu/
        | - dranks/
        |   - foty/

    >> 2: type \"foo\" on a line
    foo

    >> 3: double-click on it to drill in
    foo/
      - sammiches/
      - dranks/

    >> 4: double-click to drill in further
    foo/
      - sammiches/
        - ham/
        - tofu/
      - dranks/

    To make menus dynamic, you can add lines like \"- @mysql/start/\" or \"- @Mysql.stop/\" in menus like the above ones (to delegate to existing dynamic menus or methods).

    > Using classes
    Or, you can make the menu be backed by a class, like this:

    >> 1: make a file like this
    - /Users/craig/menus/
      - foo.rb
        | class Foo
        |   def self.menu
        |     \"
        |     - sammiches/
        |       - ham/
        |         - .buy/
        |       - tofu/
        |         - .buy/
        |     - .checkout/
        |       - cash/
        |       - credit/
        |     \"
        |   end
        |   def self.buy category, item
        |     \"- buying \#{item} \#{category}\"
        |   end
        |   def self.checkout kind
        |     \"- checking out as \#{kind}...\"
        |   end
        | end

    >> 2: type \"foo\" on an empty line
    foo

    >> 3: double-click on items to drill in
    foo/
      - sammiches/
        - ham/
          - buy/
            - buying ham sammiches

    ...or...

    foo/
      - checkout/
        - credit/
          - checking out as credit...

    Notice it doesn't show the dots in the trees to the user (to make it look simpler).  But it still knows the dots mean to call methods.  It routes to methods and passes params appropriately.

    ".unindent.gsub(/^/, '| ').gsub(/^\| $/, '|')
  end

end
