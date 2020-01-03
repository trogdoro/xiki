XIKI_DIR = File.expand_path "#{File.dirname(__FILE__)}/.." if ! defined?(XIKI_DIR)
XIKI_SERVER_MODE = false if ! defined?(XIKI_SERVER_MODE)

require 'xiki/core/environment'

# Used by a lot of classes
module Xiki

  @@loaded_already = nil

  # TODO Just use XIKI_DIR from above?

  def self.dir
    "#{XIKI_DIR}/"
  end

  if Environment.gui_emacs   # Not defined yet
    $el.el4r_lisp_eval '(ignore-errors (kill-buffer "Issues Loading Xiki"))'
  end

end

$el.set_process_query_on_exit_flag($el.get_buffer_process("*el4r:process*"), nil) if $el

# Require some of the core files
require 'rubygems'
require 'xiki/core/ol'
require 'xiki/core/requirer'
require 'xiki/core/text_util'

require 'xiki/core/launcher'
require 'xiki/core/mode'
require 'xiki/core/menu'

# Launcher.add_class_launchers classes
module Xiki

  $el.elvar.xiki_loaded_once = nil if $el && ! $el.boundp(:xiki_loaded_once)

  def self.menus
    CodeTree.menu
  end

  def self.quote_spec txt
    txt.
      gsub(/^/, '| ').
      gsub(/ +$/, '').
      gsub(/^\|(        )([+-])/) {|o| "|#{$2 == '-' ? '+' : '-'}#{$1}"}   # Show "expected" correct value as green, and the incorrect "got" value as red
  end

  def self.tests clazz=nil, describe=nil, test=nil, quote=nil

    options = yield

    prefix = Keys.prefix :clear=>1

    # Handle option items
    return "
      * navigate
    " if options[:task] == []
    prefix = "open" if options[:task] == ["navigate"]

    return if self.nav_to_line   # If on line to navigate to, just navigate

    # If no class, list all classes...

    if clazz.nil?
      files = Dir["#{Xiki.dir}spec/*_spec.rb"].entries

      # Sort, handling error when certain type of file
      files = files.sort{|a, b|
        a_time = File.mtime(a) rescue nil
        b_time = File.mtime(b) rescue nil
        next 0 if ! a_time || ! b_time
        b_time <=> a_time
      }

      txt = ["all/"] + files.map{|o| "#{o[/.+\/(.+)_spec\.rb/, 1]}/"}
      return txt.join("\n")
    end

    # If /class, list describes...

    path = Bookmarks["%xiki/spec/#{clazz}_spec.rb"]

    sync_options = prefix == :u ? {} : {:sync=>1}

    if describe.nil?
      return View.open path if prefix == "open"

      if clazz == "all"   # Run all specs
        return self.quote_spec( #prefix == :u ?
          Shell.run("rspec spec", sync_options.merge(:dir=>Xiki.dir))
          )
      end

      txt = File.read path
      return "- all/\n" + txt.scan(/^ *describe .*"(.+)"/).map{|o|
        "- #{o.first}/"
      }.join("\n")
    end

    # If /class/describe, list tests...

    if test.nil?

      return self.nav_to path, describe if prefix == "open"

      if describe == "all"   # Run whole test
        return self.quote_spec(
          Shell.run("rspec spec/#{clazz}_spec.rb", sync_options.merge(:dir=>Xiki.dir))
          )
      end

      txt = File.read path

      is_match = false
      return "- all\n" + txt.scan(/^ *(describe|it) .*"(.+)"/).map{|o|
        next is_match = o[1] == describe if o[0] == "describe"   # If describe, set whether it's a match
        next if ! is_match
        "- #{o[1]}"
      }.select{|o| o.is_a? String}.join("\n")

    end

    # If /class/describe/test, run test...

    if ! quote

      if test == "all"   # Run all for describe
        return self.quote_spec(
          Shell.run("rspec spec/#{clazz}_spec.rb -e \"#{describe}\"", sync_options.merge(:dir=>Xiki.dir))
          )
      end

      # If U prefix, just jump to file
      if prefix == "open"
        return self.nav_to path, describe, test if prefix == "open"
      end

      # Run it
      command = "rspec spec/#{clazz}_spec.rb -e \"#{describe} #{test}\""
      result = Shell.run command, :dir=>"%xiki", :sync=>true

      if result =~ /^All examples were filtered out$/
        TextUtil.title_case! clazz
        describe.sub! /^#/, ''

        return %`
          > Test doesn't appear to exist.  Create it?
          =#{path}
            : describe #{clazz}, "##{describe}" do
            :   it "#{test}" do
            :     #{clazz}.#{describe}.should == "hi"
            :   end
            : end
        `
      end

      return self.quote_spec result
    end

    # Quoted line, so jump to line number

    nil
  end

  def self.nav_to_line
    match = Line.value.match(/([\/\w.]+)?:(\d+)/)
    return if ! match

    file, line = match[1..2]
    file.sub! /^\.\//, Bookmarks["%xiki"]
    View.open file
    View.to_line line.to_i

    return true   # Did navigate
  end

  def self.nav_to path, *searches
    View.open path
    View.to_highest
    searches.each { |s| Search.forward "[\"']#{$el.regexp_quote s}[\"']" }
    Move.to_axis
    nil
  end

  def self.quote txt
    Tree.quote txt
  end

  # TODO: needs updating.
  # Run this when opening .xiki files.
  def self.on_open
    orig = View.name
    name = orig[/(.+?)\./, 1]

    file = View.file

    # Figure out whether menu or class
    txt = File.read file
    kind = txt =~ /^class / ? "class" : "menu"
    require_menu file, :force_as=>kind

    View.kill

    Buffers.delete name if View.buffer_open? name

    View.to_buffer name
    Notes.mode

    View.dir = "/tmp/"

    View.<< "- #{name}/\n", :dont_move=>1
    Launcher.launch

  end

  # Invoked by Xiki.init after the fork.
  def self.reload
    $el.el4r_kill_and_restart
    ""
  end

  def self.upon_reload

    # Do stuff here to load on top of existing fork

    Keys.map_reset

    FileTree.define_styles
    Color.define_styles
    Notes.define_styles
    Notes.init
    Notes.keys  # Define local keys
    Effects.define_styles

  end

  # Invoked by environment when Xiki starts up.
  # If already loaded, skips loading and delegates to .upon_reload.
  #
  # Xiki.init
  # Xiki.init :minimal=>1   # Don't do yaml or awesome_print conf that might interfere with some ruby environments (for embedded case).
  def self.init options={}

    # If we're reloading on top of a process after a fork, delegate to .upon_reload...

    return self.upon_reload if @@loaded_already
    @@loaded_already = true

    # Not loaded yet, so call .init methods of many classes...

    # Get rest of files to require

    classes = Dir["#{Xiki.dir}lib/xiki/{handlers,core}/*.rb"]

    classes = classes.select{|i|
      i !~ /\/ol.rb$/ &&   # Don't load Ol twice
      i !~ /\/xiki.rb$/ &&   # Remove self
      i !~ /\/key_shortcuts.rb$/ &&   # Remove key_shortcuts
      i !~ /__/   # Remove __....rb files
    }

    classes.map!{|i| i.sub(/\.rb$/, '')}.sort!

    Requirer.require_classes classes
    # key_shortcuts has many dependencies, require it last
    Requirer.require_classes ["#{Xiki.dir}lib/xiki/core/key_shortcuts.rb"] if Xiki.environment != 'web'
    Launcher.add_class_launchers classes.map{|o| o[/.*\/(.+)/, 1]}
    Launcher.load_tools_dir
    Launcher.add "xiki"
    Launcher.add "ol"

    self.init_patterns
    if ! XIKI_SERVER_MODE
      self.copy_over_default_home_xiki_files
    end

    if ! options[:minimal]
      self.awesome_print_setup
    end

    # If the first time we've loaded, open =welcome (if not xsh)...

    if $el

      if ! $el.getenv("XSH") &&
         (! $el.boundp(:xiki_loaded_once) || ! $el.elvar.xiki_loaded_once) &&
         ! Command.line_exists?("misc config", /^- don't show welcome$/) &&
         ! View.buffer_visible?("Issues Loading Xiki")

        Launcher.open("welcome", :no_search=>1)
      end
      $el.elvar.xiki_loaded_once = true
    end

    self.init_misc
  end

  def self.init_misc
    Topic.init_cache
  end


  # Invoked by self.init
  def self.init_patterns

    # load "#{Xiki.dir}roots/patterns/core_patterns.rb"
    load "#{Xiki.dir}misc/core_patterns.rb"

    # TODO
    # - Better name for core_patterns.rb?
    # - Load core_patterns.rb in ~/.xiki/roots as well
    # - Later, pre-load all files in
    #   @:xiki/menu/patterns/
    #   @~/.xiki/roots/patterns/
    #     | (just grab Xiki.xiki_path_custom_dir)
    #   - be sure to load core_patterns.rb first!

  end

  # Creates notes.notes etc. files from templates, if not there yet.
  def self.copy_over_default_home_xiki_files

    dest_dir = File.expand_path("~/xiki")

    # Do quick check for one of the files, and do nothing if not there
    return if File.exists? "#{dest_dir}/tutorial.xiki"

    FileUtils.mkdir_p dest_dir
    source_dir = "#{Xiki.dir}misc/default_home_xiki/"

    # Copy default items over files...

    Dir.entries(source_dir).select{|o| o !~ /^\.+$/}.each_with_index do |f, i|
      source, dest = "#{source_dir}/#{f}", "#{dest_dir}/#{f}"
      next if File.exists?(dest)   # Don't alter if already there
      FileUtils.cp(source, dest) rescue nil
    end
  end

  def self.process action

    case action
    when "status"
      "- #{`xiki status`}"
    when "stop"
      response = `xiki stop`
      response = "apparently it wasn't running" if response.blank?
      response.gsub /^/, '- '
    when "restart"
      response = `xiki restart`
      response = "apparently it wasn't running" if response.blank?
      response.gsub /^/, '- '
    when "log"
      "@/tmp/xiki_process.rb.output"
    when "start"
      result = `xiki`
      "- started!"
    end
  end

  def self.dont_show_welcome
    Command.append_line "misc config", "- don't show welcome"
  end

  def self.finished_loading?
    @@finished_loading
  end





  # > Unified Refactor > In progress...



  # > Scenarios
  # | Xiki.children "/tmp/", "a"   # /tmp/a... file
  # | Xiki.children "/tmp/", "a/b"   # /tmp/a... file with path "b"
  # | Xiki.children "/tmp/", ["a", "b"]   # /tmp/a... file with path "b"
  # | Xiki.children "/tmp//a"   # .children "/tmp/", "a"
  # | Xiki.children "a"   # .children "~/.xiki/roots/", "a"  # (or wherever in XIKI_PATH "a" is first found)
  # | Xiki.children "a/b"   # .children "~/.xiki/roots/", "a/b"  # (or wherever in XIKI_PATH "a" is first found)
  # | Xiki.children "a/\n  b/", "a"   # "b/"
  # | Xiki.children "/tmp/"   # delegate to file tree
  # | Xiki.children "/tmp/a.menu//"   # recurse to "/tmp//a/" ?
  # | Xiki.children "/tmp/foo.rb"   # delegate to file tree   ____________really?
  # | Xiki.children "/tmp//foo", args   # recurse to Xiki.children "/tmp/", ["foo"] + args
  # | Xiki.children Bar, "a"   # delegate to something else (whatever internal class will handle this, without worrying about dirs and .notes files, probably)
  # |   # or, will there be a case where notes text needs to be passed in
  # |     # programatically, or by grabbing .notes file from the disk?
  #
  # > For @?
  # | Xiki.children "/tmp/@rails"   # recurse to Xiki.children ["/tmp/", "rails"]   # then recurse to Xiki.children "rails", :ancestors=>["/tmp/"]
  #
  # | Xiki.children "/tmp/@rails"   # recurse to Xiki.children ["/tmp/", "rails"]   # then recurse to Xiki.children "rails", :ancestors=>["/tmp/"]
  #
  # | Xiki.children "/tmp/@rails", "a"   # recurse to Xiki.children ["/tmp/", ["rails", "a"]]   # ___?
  #
  # > Ancestors vs multiple sources (not implemented yet)
  # | Xiki.children array   # Ancestors (eg ["/tmp/d", "rails"])
  # | Xiki.children array, string   # Multiple sources (eg ["~/.xiki/rootss1/", "~/.xiki/rootss2/"], "foo")   # could be confused with: Xiki.children ancestors, path, so maybe one has to be an option
  #
  # > More thought
  # @/docs/todo/
  #   - todo.notes
  #     | > Unified > best idea for menus!:


  # Part of xiki @unified refactor
  def self.children
    # > TODO: delegate to one of these maybe
    # - Which one?
    # Tree.children2
    # Command.children2
    # Launcher.children2
    # Tree[]
    # Menu[]
    # Launcher[]
    Command.children2
  end

  def self.[] *args
    Expander.expand *args
  end

  def self.expand *args
    Expander.expand *args
  end

  # Make pull in menu, to be accessible via Xiki[]
  # Also define global 'xiki_register' for convenience
  #
  # Registers "foo" menu as...
  # Xiki.register "foo"   # class named "Foo" (assumes it's loaded)
  # Xiki.register "/tmp/foo.menu"
  # Xiki.register "/tmp/foo/"   # this dir
  #
  # Xiki.register "/tmp/foo//"   # adds dir to XIKI_PATH (makes all menus in the dir be exectable)
  def self.register
    "TODO"
  end

  def self.def *args, &block
    Expander.def *args, &block
  end

  # Just a placeholder for now
  def self.caching

    # Think through guard strategy - probably gurad just builds one big file upon updates, and xiki checks only that file's mod date, and reloads (if Xiki.caching = :optimized
    # clear cache when updated by guard -

    "TODO"
  end

  # This dir is where user xiki puts user-created menus.
  # Users can add other menu dirs to the XIKI_PATH env var, but ~/.xiki/roots is always added for now.
  # See TODO:... comment below for an improvement.
  def self.xiki_path_custom_dir
    File.expand_path("~/.xiki/roots")
  end

  def self.xiki_path_core_dir
    Bookmarks["%s/roots"]
  end

  # Return the XIKI_PATH environment var, plus ~/.xiki/roots/ and :xiki/roots.
  def self.xiki_path_dirs

    # How many times called? - memo-ize this based on XIKI_PATH value
    # Worry about this later, when it gets slow.

    list = (ENV['XIKI_PATH'] || "").split ":"
    list = list.select{|o| o.any?}   # Remove blanks

    list.map!{|o| File.expand_path o}
    list = [self.xiki_path_custom_dir, list, self.xiki_path_core_dir].flatten
    list.uniq

    # TODO:
    #   - When user hasn't set XIKI_PATH
    #     - auto-add ~/xiki/roots to the beginning
    #   - Else
    #     - assume user has added ~/xiki/roots (or the equivalent) to the beginning
  end

  def self.menuish_parent options
    ancestors = options[:ancestors]
    ancestors && ancestors[-1][/\A[*^~?]?([\w ]+)\/$/, 1]
  end

  #   def self.yaml_setup
  #     Kernel.require 'yaml'
  #
  #     YAML::ENGINE.yamler='syck' if Xiki.environment != 'web'
  #   end

  def self.awesome_print_setup
    begin
      gem "awesome_print"
    rescue Exception=>e
      return   # If it's not installed, move on silently
    end

    Kernel.require "awesome_print"
    AwesomePrint.defaults = {
      #   :indent => 2,   # right-align hash keys
      :indent => -2,   # left-align hash keys
      :index => false,
      :multiline => true,
      :plain => true,
    }
  end

  # Xiki.web is shortcut for calling Xiki.expand "foo", :client=>"web"
  #
  # Xiki.web "ip"
  # Xiki.web "ip", :no_slash=>1
  def self.web *args

    # Add or merge :client=>"web" into options
    if args[-1].is_a? Hash
      args[-1][:client] = "web"
    else
      args << {:client=>"web"}
    end

    self.expand *args
  end

  def self.kill

    # Get pid...

    txt = Shell.sync "ps -eo pid,args"
    txt = txt.split("\n").grep(/xsh forker/)

    return "<* not running" if txt == []

    pid = txt[0][/\d+/]

    # kill it...

    Shell.sync "kill #{pid}"

    "<* killed!"
  end

  def self.init_in_client
    Clipboard.init_in_client
    Tree.init_in_client
    Themes.init_in_client
    History.init_in_client
    Deck.init_in_client
    Ruby.init_in_client

    # Just invoke this, to make sure elisp var is set
    Keys.noob_mode

  end

  class << self
    attr_accessor :environment
  end

end


# Sure we want to do this globally?
def X *args
  Xiki.expand *args
end
