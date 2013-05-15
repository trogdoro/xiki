module Xiki::Menu
  class Xiki

    MENU = %`
      - .tests/
      - .github/
        > files
        @http://github.com/trogdoro/xiki
        > commits
        @https://github.com/trogdoro/xiki/commits/master
      - .setup/
        - install command/
          | Double-click on these lines to add the executable 'xiki' command to
          | your path:
          |
          @#{::Xiki.dir}/
            $ ruby etc/command/copy_xiki_command_to.rb /usr/local/bin/xiki
          |
          | Then you can type 'xiki' on a command line outside of emacs as a
          | shortcut to opening xiki and opening menus, like so:
          |
          |   $ xiki computer
          |
        - .install icon/
          | Double-click on this line to make .xiki files have the xiki 'shark'
          | icon:
          |
          - install/
          |
          | When you right-click on a .xiki file and select "Open With" and
          | choose emacs, the files will be assigned the xiki shark icon.
          |
        - install global shortcut/
          - 1) With cursor on the following, type open+in+os, then click "Install"
            @ #{::Xiki.dir}etc/services/Xiki Menu.workflow

          - 2) In Mac OS, open
            | System Preferences > Keyboard > Keyboard Shortcuts > Services

          - 3) Scroll down to the bottom and give "Xiki Menu" the shortcut
            | Command+Control+X

          - 4) Try it out by typing Command+Control+X from any application
            | It should prompt you to type a xiki menu
        - .process/
          - status/
          - start/
          - stop/
          - restart/
          - log/
        - el4r/
          > Configure
          @#{::Xiki.dir}
            % sudo bash etc/install/el4r_setup.sh

          - docs/
            | This will create/update files in your home directory, which make el4r
            | point to the version of ruby currently active.
            |
            | You can run this multiple times.
        - .misc/
          - .dont show welcome/
        - key shortcuts/
          - enable all/
            | Add this line to enable all xiki keys:
            ~/.el4r/init.rb
              | KeyBindings.keys   # Use default key bindings

              > Todo: show options for more limited key mappings as well?
              | # Only enable Control-return in all files.
              | KeyBindings.map_control_return
              | # Only enable Control-return in .notes files.
              | @define_key(:notes_mode_map, kbd("<C-return>"))  { Launcher.go }
          - minimal/
            | Add this line to enable all xiki keys:
            ~/.el4r/init.rb
              | KeyBindings.minimal__
        @web/
      - api/
        > Summary
        Here are some functions that will always be available to menu classes,
        even external ones.
        |
        | Put pipes at beginning of lines (except bullets etc)
        |   p Xiki.quote "hey\\nyou"
        |
        | Return path to tree's root including current line, will be a list with 1
        | path unless nested.
        |   p Xiki.trunk
        |
      `

    def self.tests *args
      ::Xiki.tests *args # clazz=nil, describe=nil, test=nil, quote=nil
    end

end; end
