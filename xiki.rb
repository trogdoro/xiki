XIKI_ROOT = File.dirname(__FILE__)
Dir.chdir(XIKI_ROOT)

# Used by a lot of classes
class Xiki
  @@dir = "#{Dir.pwd}/"   # Store current dir when xiki first launches

  def self.dir
    @@dir
  end
end

# Require some of the core files
require 'rubygems'
require 'ol'
require 'requirer'
require 'text_util'
require 'notes'
require 'launcher'
require 'mode'
require 'menu'

# Launcher.add_class_launchers classes
class Xiki
  def self.menu
    %`
    - .tests/
    - .github/
      - commits/
      - files/
    - .setup/
      - install command/
        | Double-click on these lines to add the executable 'xiki' command to
        | your path:
        |
        @ $ chmod 755 #{Xiki.dir}etc/xiki
        @ $ sudo ln -s #{Xiki.dir}etc/xiki /usr/local/bin/xiki
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
    - api/
      > Summary
      Here are some functions that will always be available to menu classes,
      even external ones.
      |
      | Put pipes at beginning of lines (except bullets etc)
      |   p Xiki.quote "hey\\nyou"
      |
      | Return current chunk of the tree as text, unquoting, including siblings:
      |   p Xiki.branch
      |
      | Return path to tree's root including current line, will be a list with 1
      | path unless nested.
      |   p Xiki.trunk
      |
      Here are some functions that will always be available toxxxxxxxxxxxxxxxxxx
    `
  end

  def self.install_icon arg

    emacs_dir = "/Applications/Emacs.app"

    return "- Couldn't find #{emacs_dir}!" if ! File.exists?("#{emacs_dir}")

    plist_path = "#{emacs_dir}/Contents/Info.plist"

    plist = File.read "#{emacs_dir}/Contents/Info.plist"

    # TODO
    # "Back up plist file - where - xiki root?!
    # "Tell them where it was backed up!
    # "Show diffs of change that was made!

    return "- This file wasn't in the format we expected: #{plist_path}" if plist !~ %r"^\t<key>CFBundleDocumentTypes</key>\n\t<array>\n"

    # TODO
    # .plist
      # if change was already made, say so

    # TODO
    # icon
      # copy over
        # cp "#{Xiki.dir}etc/shark.icns"


    "- finish implementing!"
  end

  def self.path options={}
    Tree.construct_path options
  end

  def self.insert_menu
    return Launcher.insert "- Xiki.menus/" if Keys.prefix_u

    input = Keys.input(:timed => true, :prompt => "Start typing a menu that might exist (* for all): ")
    View << "#{input}"
    Launcher.launch
  end

  def self.open_menu

    return Launcher.open("- last/") if Keys.prefix_u

    input = Keys.input(:timed => true, :prompt => "Start typing a menu that might exist (* for all): ")
    View.to_buffer "menu"
    Notes.mode
    View.kill_all
    View << "#{input}\n"
    View.to_highest
    Launcher.launch
  end

  def self.menus
    CodeTree.menu
  end

  def self.github page
    Firefox.url case page
      when 'files'; "http://github.com/trogdoro/xiki"
      when 'commits'; "https://github.com/trogdoro/xiki/commits/master"
      end
    ".flash - Opened in browser!"
  end

  def self.dont_search
    $xiki_no_search = true
  end

  def self.tests clazz=nil, *test

    if clazz.nil?   # If nothing, list all
      return ["all/"] + Dir["#{Xiki.dir}/spec/*_spec.rb"].entries.map{|o| "#{o[/.+\/(.+)_spec\.rb/, 1]}/"}
    end

    if test.empty?   # If just class, list all tests
      Xiki.dont_search

      if clazz == "all"
        return Keys.prefix_u ?
          Console.run("rspec spec", :dir=>Xiki.dir) :
          Tree.<<(Console.run("rspec spec", :dir=>Xiki.dir, :sync=>1), :escape=>'| ')
      end

      path = Bookmarks["$x/spec/#{clazz}_spec.rb"]
      code = File.read path
      return "- all/\n" + code.scan(/^ *(describe|it) .*"(.+)"/).map{|o|
        o[0] == "describe" ?
          "- #{o[1]}/" :
          "  - #{o[1]}/"
      }.join("\n")
    end

    clazz = TextUtil.snake_case(clazz.name) if ! clazz.is_a? String


    quote = Line[/^ *\| ?(.+)/, 1]

    if ! quote   # If just test

      # If U prefix, just jump to file
      if Keys.prefix_u :clear=>true
        View.open "$x/spec/#{clazz}_spec.rb"
        View.to_highest
        Search.forward "[\"']#{test[-1]}[\"']"
        Line.to_beginning
        return
      end

      # Run it
      command = "rspec spec/#{clazz}_spec.rb"
      command << " -e \"#{test.join ' '}\"" unless test == ["all"]
      result = Console.run command, :dir=>"$x", :sync=>true
      return result.gsub(/^/, '| ').gsub(/ +$/, '')
    end

    # Quoted line, so jump to line number
    file, line = Line.value.match(/([\/\w.]+)?:(\d+)/)[1..2]
    file.sub! /^\.\//, Bookmarks["$x"]
    View.open file
    View.to_line line.to_i
    nil
  end

  def self.trunk
    Tree.construct_path(:all=>1).split(/\/@ ?/)
  end

  def self.branch
    Tree.branch
  end

  def self.quote txt
    Tree.quote txt
  end

  # Other .init mode defined below
  def self.init

    # Get rest of files to require
    classes = Dir["**/*.rb"]
    classes = classes.select{|i|
      i !~ /xiki.rb$/ &&   # Remove self
      i !~ /key_bindings.rb$/ &&   # Remove key_bindings
      i !~ /\// &&   # Remove all files in dirs
      i !~ /tests\// &&   # Remove tests
      i !~ /\// &&   # Remove tests
      i !~ /__/   # Remove __....rb files
    }

    classes.map!{|i| i.sub(/\.rb$/, '')}.sort!

    # Require classes
    Requirer.safe_require classes

    # key_bindings has many dependencies, require it last
    Requirer.safe_require ['key_bindings.rb']

    Launcher.add_class_launchers classes
    Launcher.reload_menu_dirs

    Launcher.add "xiki"

    # Pull out into .define_mode

    Mode.define(:xiki, ".xiki") do

      orig = View.name
      name = orig[/(.+?)\./, 1]

      View.to_buffer "#{name}"
      View.hide_others :all=>1

      Buffers.delete orig

      View.kill_all

      Notes.mode

      View.dimensions("c")
      $menu_resize = true
      View.<< "- #{name}/\n", :dont_move=>1
      Launcher.launch

    end
  end
end
