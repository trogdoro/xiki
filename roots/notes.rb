class Xiki::Menu::Notes

  MENU = %`
    - docs/
      - summary/
        | This menu lists out the notes in ~/xiki/.  It lets you drill
        | into them and navigate to individual notes and update.
        |
        | It can be used two different ways, by using @notes by itself,
        | or nesting @notes under another menu.
      - nesting/
        - intro/
          | You can nest @notes under other menus, like so:
          = foo/
            = notes/

          | It will use ~/xiki/foo.xiki. This is a convenience mechanism
          | for you to create notes that are associated with menus.
        - example/
          > 1. if you have a file like this
          =~/xiki/foo.xiki
            | > Bar
            | Some notes about bar.
            |
            | > Baz
            | Some notes about baz.

          > 2. When you expand, it will look like this
          = foo/
            = notes/
              > Bar
              > Baz

          | Then you can expand the headings and edit inline.
      - keys/
        > open+menu
        | Prompts for a key or two, and opens the matching note from ~/xiki/
        > as+update
        | Save the note instead of navigating to it
    - api/
      > Turn notes wiki text into html
      =Notes.to_html "> Heading\\njust text\\n"
    `

  def self.menu_after output, *path

    options = yield

    # Prepend parent to items if foo/=menu...

    items, task = options[:items], options[:task]

    # foo/=notes/, so use foo as name...

    if path[0] !~ /\A\w/   # Only if there's not a menu-ish item underneath.

      if parent = Xiki.menuish_parent(options)

        # We're nested under a menu-like item, so use it as the name
        (items||=[]).unshift parent
        output = nil   # Blank out output so we won't get mislead below

      # $ foo/=notes/, so use foo as name...

      elsif parent = options[:ancestors] && options[:ancestors][-1][/\A\$ (\w+)/, 1]

        # We're nested under a menu-like item, so use it as the name
        (items||=[]).unshift parent
        output = nil   # Blank out output so we won't get mislead below
      end

    end

    # /, so list notes at top...

    if ! items
      if task == ["source"]
        Launcher.open "~/xiki/\n  + **\.xiki$/"
        return ""
      end

      # In future, should probably look in all XIKI_PATH dirs > borrow from > =all


      # List .xiki files in ~/xiki/ sorted by date...

      files = FileTree.files_in_dir(File.expand_path("~/xiki/"), :date_sort=>1)[1]

      files.each{|o| o.sub! /.+\//, ''}

      return files.select{|o| o =~ /^\w.*\.xiki$/}.map{|o| "+ #{o.sub(/\.xiki$/, '').gsub(/_/, ' ')}/\n"}.join('')

    end

    # /foo and output output from MENU, so just show it...

    return output if output

    # /foo, so delegate to ~/commands dir...

    name = items.slice!(0)
    path = name.gsub(' ', '_')

    # Call .drill directly

    txt = Notes.drill path, *items, options

    Ol "items", items   # => []

    return txt if txt

  end

end
