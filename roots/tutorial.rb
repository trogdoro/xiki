# Old version of this

class Tutorial

  MENU = %`
    - menu item/
      : Good job! Now use the arrow keys to collapse this menu
      : item, by using the arrow keys to move up to the "menu item"
      : line again and type Ctrl+X.
      :
    - filtering/
      >
      : You can use the arrow keys __or the space key to cancel the filter > or other shortcuts
      :
      : Next topic:
      - editing/
    - editing/
      > Don't need to
      : know emacs
      :
      : Next topic:
      - shell commands/
    - shell commands/
      > Hey
      : Hiya
      :
      : Next topic:
      - shell commands/
    - files/
    - .topics/
    `

  def self.topics topic=nil

    # /topics/, so return root items...

    if ! topic
      txt = Tree.children MENU, ""
      return txt.sub /.+\n/, ''   # Remove 1st item
      return txt
    end

    # /topics/foo, so make the topic appear at the root...

    "=replace/siblings/2/expand/\n  + #{topic}/"

  end

  def self.menu_after output=nil, *args

    # /, so don't handle it

    return nil if args == [] || ["menu item", "topics"].member?(args[0])
  end

  def self.menu_before *args

    options = yield

    # /, so show welcome text

    if args == []

      # ~, so show options

      return "~ topics" if options[:task] == []
      return self.topics if options[:task] == ["topics"]

      return %`
        > Welcome
        : Welcome to the Xiki tutorial!
        :
        : Let's start by expanding some menu items. Use the arrow
        : keys to move the cursor down to the "menu item" line below.
        : Then type Ctrl+X to expand it.
        :
        + menu item/
        :
        : Or, browse all topics:
        - topics/
        :
        :?Note: this tutorial is inprogress, and is unfinished
        `
    end

    # Root / path passed, so show main menu
  end

end
