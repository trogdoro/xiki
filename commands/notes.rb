class Xiki::Menu::Notes

  MENU = %`
    - docs/
      - summary/
        | This menu lists out the notes in ~/xiki/notes/.  It lets you drill
        | into them and navigate to individual notes and update.
        |
        | It can be used two different ways, by using @notes by itself,
        | or nesting @notes under another menu.
      - nesting/
        - intro/
          | You can nest @notes under other menus, like so:
          =foo/
            =notes/

          | It will use ~/notes/foo.notes. This is a convenience mechanism
          | for you to create notes that are associated with menus.
        - example/
          > 1. if you have a file like this
          =~/notes/foo.notes
            | > Bar
            | Some notes about bar.
            |
            | > Baz
            | Some notes about baz.

          > 2. When you expand, it will look like this
          =foo/
            =notes/
              > Bar
              > Baz

          | Then you can expand the headings and edit inline.
      - keys/
        > open+menu
        | Prompts for a key or two, and opens the matching note from ~/xiki/notes/
        > as+update
        | Save the note instead of navigating to it
    - api/
      > Turn notes wiki text into html
      =Notes.to_html "> Heading\\njust text\\n"
    `

  def self.menu_after output, *path

    options = yield

    # Prepend parent to items if foo/=menu...

    items, dropdown = options[:items], options[:dropdown]

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
      # Or navigate there if open+
      if dropdown == ["source"]
        Launcher.open "~/xiki/notes/"
        return ""
      end
      return Xiki["~/xiki/notes//"]
    end

    # /foo and output output from MENU, so just show it...

    return output if output

    # /foo, so delegate to ~/notes dir...
    # Just pass prefix
    txt = Xiki["~/xiki/notes//", items, options.select{|key, value| [:prefix, :dropdown].include?(key)}]

    return txt if txt

    # Nothing returned, so show default text...

    # items
    #   When show initial => ["test2"]
    #   When create file => ["test2", "> Sample heading", ": These notes don't exist yet."]

    if items.length == 1
      return "
        > Sample heading
          | These notes don't exist yet.
          | Edit this text and type Ctrl+D to create
          | (or type Ctrl+D and select 'clear').
          |
        "
    end

    # ~, so show option

    return "~ create\n~ clear" if !dropdown || dropdown == []

    if items.length > 1
      if dropdown == ["create"]

        name, heading, content = items
        return "=beg/siblings/" if content !~ /\n/

        name.gsub!(/ /, '_')
        file = File.expand_path "~/xiki/notes/#{name}.notes"
        content = Tree.unquote content
        File.open(file, "w") { |f| f << "#{heading}\n#{content}" }

        return "<! created!"
      end

      if dropdown == ["clear"]
        # return the code that clears
        return "=replace/siblings/\n  | hi\n  |"
      end

    end

    # File doesn't exist, so allow creating via dropdown...

    #     "
    #     - Note doesn't exist yet!
    #     | Type something here and do as+update
    #     | to create it.
    #     "

  end

end
