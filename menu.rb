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
    lisp = %Q<
      (define-key global-map
        [menu-bar #{menu_spaces} #{function}]
        '("#{name}" . #{function}))
    >
    #puts lisp
    $el.el4r_lisp_eval lisp
  end

  def self.init
    add_menu 'Xiki'
    add_menu 'Xiki', 'Layout'
    add_menu 'Xiki', 'To'
    add_menu 'Xiki', 'Do'
    add_menu 'Xiki', 'Enter'
    add_menu 'Xiki', 'Open'
    add_menu 'Xiki', 'As'
  end

end

