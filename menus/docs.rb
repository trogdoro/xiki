class Docs
  def self.menu
    %`
    - Introduction/
      | Most of the Xiki documentation is spread out among other menus, under
      | their respective 'docs' menus.
      |
      > Here are some to check out first
      @keys/docs/
      @menu/docs/
      @files/docs/
      @view/docs/
      |
    - Try typing stuff/
      > Try just typing a menu
      | Think of a menu name that might exist and sounds likely to solve your
      | problem.  For example, if you want to do something with images, try the
      | 'images' menu.  Type it on a blank line and double-click, or use
      | open+menu (type Ctrl-O, Ctrl-M).
      |
      | Help us improve the docs, and contribute back to the Xiki project.  If
      | the menu doesn't exist, create it!  If it exists, but could be improved,
      | edit it!  Many menus exist just to direct you to other menus.  It takes
      | only seconds to create them.
      |
      @menu/docs/how_to_create/
      |
    - Other places to look/
      - You may want to check out__
        __
        - for developers
          - code dirs/__
        > All Commands
        @all/
      |
    - FAQ/
      > What is Xiki?
      | Xiki is an environment that can be used in several ways:
      | - Shell terminal on steroids
      | - Development environment for coding rails or node.js apps, etc.
      | - Framework for making lightweight user interfaces
      |
      > What does Xiki stand for?
      | XIKI is short for eXecutable wIKI
      |
      > What are Xiki's main dependencies?
      | - Xiki runs inside of Emacs
      |   - and takes advantage of many powerful Emacs features
      |   - but you don't need to know Emacs to use Xiki
      | - Xiki is implemented mostly in ruby
      |   - but can use other languages like python, javascript, and coffeescript
      |
      > What's the best way to figure out how to do something?
      - @docs/Try typing stuff/
      |
    `
  end
end
