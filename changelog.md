# Changelog

This changelog likely won't be strictly maintained.  But it covers the big changes for the 1.0.0a release.

## 1.0.0a changes

### Installer

- Improved web-based installer

### Library

- Xiki can be used as a library
   - For example using the Xiki gem in rails app
      - For render html or webservice versions of your menus
      - And for using Xiki as an app building framework

### Web interface

- to try xiki out via the web interface
   - (using it kind of like a web framework)

### Xiki plugins for other editors / IDE's

- Most paths types will now work from other editors and tools
   - (via the 'xiki' shell command)
   - Including
      - dir and file paths with filtering
      - shell commands
- Most text editor dependencies have been removed

### Menus

- You can now creating menus by creating...
   - text files
   - scripts (no longer required to define a class)
      - ruby, python, js, coffee
   - directories
      - directories can contain arbitrarily nested dirs and any of the above files
      - so different types of files can work together to back menus
      - now menus are no longer limited to a single file
         - and can scale to the level of complexity of a sophisticated web app
   - notes (heading will expand as menu items)
   - misc other file types: .bootstrap, .md, .conf
   - in addition to the old Xiki file types
      - menus, classes
- "Handlers" can be created to let other file types be menu source files
- All menus are lazy-loaded
   - for faster startup time and to immediately reflect changes
   - a caching layer should probably be implemented at some point
      - if the reloading every time causes runtime slowness
- MENU_PATH environment var
   - lets you configure dirs to look for menus sources
- Make menus that wrap other menus
   - for convenience
      - like:
      @cheese/2/
- Navigation to the source files within menu dirs
   - that correspond to the item your cursor is on
      - analogous to jumping to the rails action from the path
         - but with fine granuality, so like jumping to the right model for a nested resource
- Any file path can be expanded as though it's a menu
  - menus can be run inline, with the "//" syntax

### Conf files

- Conf files can be edited inline in menus
   - if menus have default conf files, those will be used as starting points
   - for example, the 'mysql' menu will pop up with a default conf file
      - (the first time you use it)

