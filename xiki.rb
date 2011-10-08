XIKI_ROOT = File.dirname(__FILE__)
Dir.chdir(XIKI_ROOT)

# Require some of the core files
require 'rubygems'
require 'ol'
require 'requirer'
require 'text_util'
require 'notes'
require 'line_launcher'

# Get rest of files to require
classes = Dir["**/*.rb"]
classes = classes.select{|i| i !~ /xiki.rb$/}   # Remove self
classes = classes.select{|i| i !~ /key_bindings.rb$/}   # Remove key_bindings
classes = classes.select{|i| i !~ /tests\//}   # Remove tests
classes = classes.select{|i| i !~ /spec\//}   # Remove tests
classes = classes.select{|i| i !~ /__/}   # Remove __....rb files

classes.map!{|i| i.sub(/\.rb$/, '')}.sort!

# Require classes
Requirer.safe_require classes

# key_bindings has many dependencies, require it last
Requirer.safe_require ['key_bindings.rb']

Launcher.add_class_launchers classes

class Xiki
  def self.insert_menu
    # Implement
    CodeTree.insert_menu "- Xiki.menus/"
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
      ".run_tests",
      '.test Search, "should convert case correctly"',
      '.visit_github_page',
      '.visit_github_commits',
    ]
  end

  def self.visit_github_page
    Firefox.url "http://github.com/trogdoro/xiki"
  end

  def self.visit_github_commits
    Firefox.url "https://github.com/trogdoro/xiki/commits/master"
  end

  def self.test clazz, test, quoted=nil
    clazz = TextUtil.snake_case(clazz.name)

    # If U prefix, just jump to file
    if Keys.prefix_u :clear=>true
      View.open "$x/spec/#{clazz}_spec.rb"
      View.to_highest
      Search.forward "[\"']#{test}[\"']"
      Move.to_line_text_beginning
      return
    end

    if quoted.nil?   # If no quoted, run test
      command = "rspec spec/#{clazz}_spec.rb -e \"#{test}$\""
      result = Console.run command, :dir=>"$x", :sync=>true

      return result.gsub(/^/, '| ').gsub(/ +$/, '')
    end

    # Quoted line, so jump to it
    file, line = Line.value.match(/\.\/(.+):(.+):/)[1..2]
    View.open "$x/#{file}"
    View.to_line line.to_i
    nil
  end

  def self.run_tests
    Console.run "rspec spec", :dir=>"$x"
  end
end
