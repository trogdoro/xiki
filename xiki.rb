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

# Launcher.add_class_launchers classes
class Xiki
  def self.menu
    "
    - .tests/
    - .github/
      - commits/
      - files/
    "
  end

  def self.insert_menu
    # Implement
    Launcher.insert "- Xiki.menus/"
  end

  def self.open_menu
    input = Keys.input(:timed => true, :prompt => "start typing a menu that might exist: ")
    View.to_buffer "menu"
    Notes.mode
    View.kill_all
    #     input = Keys.input(:timed => true, :prompt => "start typing a word you think there might be a menu for: ")
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
    nil
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
        Move.to_line_text_beginning
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
end

Launcher.add_class_launchers classes
Launcher.reload_menu_dirs

Launcher.add "xiki"
