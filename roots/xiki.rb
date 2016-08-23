


# Currently not used



module Xiki

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
        @#{self.dir}/
          $ ruby misc/command/copy_xiki_command_to.rb /usr/local/bin/xiki
        |
        | Then you can type 'xiki' on a command line outside of emacs as a
        | shortcut to opening xiki and opening menus, like so:
        |
        |   $ xiki computer
        |
      - install icon/
        =install xiki icon/
      - install global shortcut/
        =install global shortcut/
      - .process/
        - status/
        - start/
        - stop/
        - restart/
        - log/
      - el4r/
        > Configure
        @#{self.dir}
          % sudo bash misc/install/el4r_setup.sh

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
            | KeyShortcuts.keys   # Use default key shortcuts

            > Todo: show options for more limited key mappings as well?
            | # Only enable Control-return in all files.
            | KeyShortcuts.map_control_return
            | # Only enable Control-return in .notes files.
            | @define_key(:notes_mode_map, kbd("<C-return>"))  { Launcher.go }
        - minimal/
          | Add this line to enable all xiki keys:
          ~/.el4r/init.rb
            | KeyShortcuts.minimal__
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
      |   p Tree.path
      |
    - .server/
      - .kill
    `

end
