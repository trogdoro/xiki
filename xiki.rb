XIKI_ROOT = File.dirname(__FILE__)
Dir.chdir(XIKI_ROOT)

# Require some of the core files
require 'rubygems'
require 'ol'
require 'requirer'
require 'text_util'
require 'notes'
require 'launcher'

# Get rest of files to require
classes = Dir["**/*.rb"]
classes = classes.select{|i| i !~ /xiki.rb$/}   # Remove self
classes = classes.select{|i| i !~ /key_bindings.rb$/}   # Remove key_bindings

classes = classes.select{|i| i !~ /\//}   # Remove all files in dirs

classes = classes.select{|i| i !~ /tests\//}   # Remove tests
classes = classes.select{|i| i !~ /\//}   # Remove tests
classes = classes.select{|i| i !~ /__/}   # Remove __....rb files

classes.map!{|i| i.sub(/\.rb$/, '')}.sort!

# Require classes
Requirer.safe_require classes

# key_bindings has many dependencies, require it last
Requirer.safe_require ['key_bindings.rb']

# Launcher.add_class_launchers classes
class Xiki
  @@dir = Dir.pwd   # Store current dir when xiki first launches
  def self.dir
    @@dir
  end

  @@dir = Dir.pwd   # Store current dir when xiki first launches

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

  def self.menu
    [
      ".tests/",
      ".all_tests/",
      '.visit_github_page/',
      '.visit_github_commits/',
    ]
  end

  def self.visit_github_page
    Firefox.url "http://github.com/trogdoro/xiki"
    nil
  end

  def self.visit_github_commits
    Firefox.url "https://github.com/trogdoro/xiki/commits/master"
    nil
  end

  def self.tests clazz=nil, test=nil, *quoted
    if clazz.nil?   # If no class, list all
      return [".all/"] + Dir.new(Bookmarks["$x/spec/"]).entries.grep(/^[^.]/) {|o| "#{o[/(.+)_spec\.rb/, 1]}/"}
    end

    if test.nil?   # If no test, list all
      path = Bookmarks["$x/spec/#{clazz}_spec.rb"]
      code = File.read path
      return ['all/'] + code.scan(/^ *(describe|it) .*"(.+)"/).map{|o| "#{o[1]}/".sub(/^#(.+)\/$/, ".\\1:")}
    end

    clazz = TextUtil.snake_case(clazz.name) if ! clazz.is_a? String
    # If U prefix, just jump to file
    if Keys.prefix_u :clear=>true
      View.open "$x/spec/#{clazz}_spec.rb"
      View.to_highest
      Search.forward "[\"']#{test}[\"']"
      Move.to_line_text_beginning
      return
    end

    if quoted.blank?   # If no quoted, run test

      command = "rspec spec/#{clazz}_spec.rb"
      command << " -e \"#{test}\"" unless test == "all"

      result = Console.run command, :dir=>"$x", :sync=>true
      return result.gsub(/^/, '| ').gsub(/ +$/, '')
    end

    quoted = quoted.join "/"

    # Quoted line, so jump to it
    file, line = Line.value.match(/([\/\w.]+)?:(\d+)/)[1..2]
    file.sub! /^\.\//, Bookmarks["$x"]
    View.open file
    View.to_line line.to_i
    nil
  end

  def self.all
    Console.run "rspec spec", :dir=>"$x"
  end
end

Launcher.add_class_launchers classes
Launcher.reload_menu_dirs

Launcher.add "xiki"
