class Xiki::Menu::Notes

  MENU = %`
    - docs/
      - summary/
        | This menu lists out the notes in ~/notes/.  It lets you drill
        | into them and navigate to individual notes and update.
        |
        | It can be used two different ways, by using @notes by itself,
        | or nesting @notes under another menu.
      - nesting/
        - intro/
          | You can nest @notes under other menus, like so:
          @foo/
            @notes/

          | It will use ~/notes/foo.notes. This is a convenience mechanism
          | for you to create notes that are associated with menus.
        - example/
          > 1. if you have a file like this
          @~/notes/foo.notes
            | > Bar
            | Some notes about bar.
            |
            | > Baz
            | Some notes about baz.

          > 2. When you expand, it will look like this
          @foo/
            @notes/
              > Bar
              > Baz

          | Then you can expand the headings and edit inline.
      - keys/
        > open+menu
        | Prompts for a key or two, and opens the matching note from ~/notes/
        > as+update
        | Save the note instead of navigating to it
    - api/
      > Turn notes wiki text into html
      @Notes.to_html "> Heading\\njust text\\n"
    `

  def self.menu_after output, *path

    options = yield

    # Prepend parent to items if foo/@menu...

    items = options[:items]

    # foo/@notes/, so use foo as name...

    if parent = Xiki.menuish_parent(options)
      # If we're nested under a menu-like item, use it as name
      (items||=[]).unshift parent
      output = nil   # Blank out output so we won't get mislead below
    end

    # /, so list notes at top...

    if ! items
      if options[:prefix] == "open"   # Or navigate there if open+
        Launcher.open "~/notes/"
        return ""
      end
      return "#{Xiki["~/notes//"]}> This menu\n#{output}"
    end

    # /foo and output, so MENU already handled it...

    return output if output

    # /foo and no output, so we need to handle it...

    Xiki["~/notes//", items]
  end

end
