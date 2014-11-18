class Xiki::Args

  MENU = %`
    - docs/
      - summary/
        | This menu helps you when creating your own menus.  It
        | lets you try different ways of passing stuff into the
        | menu and see how it gets passed in as args to the menu.
        - more/
          | Usually only menus that have code need to worry about
          | args.  Nonetheless, trying out the examples below can
          | be instructive regarding how items are passed into all
          | menus.
      - examples/
        @args/a/
        @args/a/b/
        - more/
          - items underneath/
            @args/
              - a/
            @args/
              - a/
                - b/
            @args/
              - a/b/
          - multiple lines/
            @args/
              : Colons mean to pass the lines in
              : together, with the colons removed.
            @args/
              | Pipes mean to pass the line in individually. Unless the
              | menu begs for all the lines by returning "=beg/siblings/".
              | See the docs for the "beg" menu for more information.
          - other types of items/
            @args/
              > Headings
            @args/
              | When a nested menu starts with "@", it doesn't invoke
              | the parent menu at all.
              @ip/
          - slashes/
            @args/
              > Headings
            @args/
              | When a nested menu starts with "@", it doesn't invoke
              | the parent menu at all.
              @ip/
      - see/
        @options/
    `


  def self.menu_after output, *args

    # /, so show args and docs
    return "#{Tree.quote args.inspect}#{output}" if args == []

    # /docs/..., so it was already handled
    return nil if output

    # /foo/bar..., so show user what the args are
    Tree.quote args.inspect
  end


end
