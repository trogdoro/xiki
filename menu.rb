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
    menu[1] = "Search" if menu[1] == "ISearch"

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

end
