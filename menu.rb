class Menu

  def self.add_menu name
    lisp = %Q<
      (define-key global-map [menu-bar #{name.downcase}]
        (cons "#{name}" (make-sparse-keymap "#{name}")))
    >
    #puts lisp
    $el.el4r_lisp_eval lisp

    menu = $el.elvar.menu_bar_final_items.to_a
    $el.elvar.menu_bar_final_items = menu.push(name.downcase.to_sym)
  end

  def self.add_item menu, name
    menu = menu.downcase
    name_as_method = name.downcase.gsub(' ', '-')
    lisp = %Q<
      (define-key global-map
        [menu-bar #{menu} #{name_as_method}]
        '("#{name}" . #{menu}-#{name_as_method}))
    >
    #puts lisp
    $el.el4r_lisp_eval lisp
  end

  def self.do_one_prefix prefix
    # Don't continue if no bookmark defined
    return if Bookmarks['$xiki'].empty?
    # Get matches in bindings file
    txt = Bookmarks.read("$xiki/key_bindings.rb")
    items = []
    txt.scan(/^ +Keys\.#{prefix}(.+)# (.+)/) { |l|
      next if l[0] =~ /\(:/  # Skip if a local map
      comment = l[1]
      comment.sub!(/ \*+$/, '')  # Remove asterixes
      comment.sub!(/ \(.+/, '')
      next if comment =~ /[0-9-]/
      comment.sub!(/:.+/, '')
      # Print, omitting details
      menu, item = comment.match(/(.+?) (.+)/)[1..2]
      items << [menu, item]
    }

    items.reverse.each do |i|
      self.add_item i[0], i[1]
    end
  end

  def self.init
    add_menu "As"
    add_menu "Open"
    add_menu "Enter"
    add_menu "Do"
    add_menu "To"
    add_menu "Layout"
  end

end

