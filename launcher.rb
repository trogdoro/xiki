require 'effects'
require 'ruby_console'
require 'xiki'
gem 'httparty'; require 'httparty'
gem 'activesupport'; require 'active_support/ordered_hash'

class Launcher
  extend ElMixin

  CLEAR_CONSOLES = [
    "*output - tail of /tmp/output_ol.notes",
    "*output - tail of /tmp/ds_ol.notes",
    "*visits - tail of /tmp/visit_log.notes",
    "*console app",
    ]

  MENU_DIRS = [
    "#{Xiki.dir}/menus",
    File.expand_path("~/menus"),
    ]

  @@log = File.expand_path("~/.emacs.d/path_log.notes")

  # Set this to true to just see which launcher applied.
  # Look in /tmp/output.notes
  @@just_show = false
  #   @@just_show = true

  @@launchers ||= ActiveSupport::OrderedHash.new
  @@launchers_procs ||= []
  @@launchers_parens ||= {}
  @@menus ||= [{}, {}]

  def self.menus
    @@menus
  end

  def self.menu_keys
    (@@menus[0].keys + @@menus[1].keys).uniq.sort #.select do |possibility|
  end

  def self.menu
    %`
    - docs/
      > Summary
      Launcher is the class that handles "launching" things (double-clicking on a
      line, or typing Ctrl-enter).
      |
      > See
      @launcher/api/
      |
    - api/
      > Summary
      A few methods on the Launcher class.
      |
      > invoke
      @ p Launcher.invoke 'Computer', 'computer/ip/'
      |
    `
  end

  def self.last path, options={}

    path = path.sub /^last\/?/, ''

    log = IO.read(self.log_file)
    paths = log.split("\n")

    # If nothing passed, just list all roots

    if path.empty?
      paths.map!{|o| o.sub /\/.+/, '/'}   # Cut off after path
      return paths.reverse.uniq.join("\n")+"\n"
    end

    paths = paths.select{|o| o =~ /^- #{Notes::LABEL_REGEX}#{path}/}

    # If root passed, show all matching
    if options[:exclude_path]
      paths.each{|o| o.sub! /^- #{path}\//, '- '}
      paths = paths.select{|o| o != "- "}
    else
      paths = paths.map{|o| o.sub /^- #{Notes::LABEL_REGEX}/, '\\0@'}
    end
    paths.reverse.uniq.join("\n")+"\n"
  end

  def self.log options={}
    log = IO.read(self.log_file)
    result = log.split("\n")
    result = result.map{|o| o.sub /^- #{Notes::LABEL_REGEX}/, '\\0@'} if options[:at]
    result.reverse.uniq.join("\n")+"\n"
  end

  def self.log_file
    @@log
  end

  def self.add *args, &block
    arg = args.shift

    raise "Launcher.add called with no args and no block" if args == [nil] && block.nil?

    if arg.is_a? Regexp   # If regex, add
      @@launchers[arg] = block
    elsif arg.is_a? Proc   # If proc, add to procs
      @@launchers_procs << [arg, block]
    elsif arg.is_a?(String)
      self.add_menu arg, args[0], block
    else
      raise "Don't know how to launch this"
    end
  end

  def self.add_menu root, hash, block
    if hash.nil? && block   # If just block, just define
      return @@menus[1][root] = block
    elsif hash.is_a?(Hash) && hash[:menu_file] && block   # If just block, just define
      return @@menus[0][root] = block
    end

    # If just root, define class with that name
    if block.nil? && (hash.nil? || hash[:class])
      clazz = hash ? hash[:class] : root
      clazz.sub!(/(\w+)/) {TextUtil.snake_case $1} if hash
      self.add root do |path|
        # Make class me camel case, and change Launcher.invoke to Menu.call
        Launcher.invoke clazz, path
      end
      return
    end
    menu = hash[:menu]
    if menu
      if menu =~ /\A\/.+\.\w+\z/   # If it's a file (1 line and has extension)
        require_menu menu
        return
      elsif menu =~ /\A[\w \/-]+\z/   # If it's a menu to delegate to
        self.add root do |path|
          Menu.call menu, Tree.rootless(path)
        end
        return
      end

      self.add root do |path|   # If text of the actual menu
        # Different from Menu[...] or .drill?
        Tree.climb menu, Tree.rootless(path)
      end
      return
    end

    raise "Don't know how to deal with: #{root}, #{hash}, #{block}"
  end

  def self.launch_or_hide options={}
    # If no prefixes and children exist, delete under
    if ! Keys.prefix and ! Line.blank? and CodeTree.children? # and ! Line.matches(/^ *\|/)
      Tree.minus_to_plus
      Tree.kill_under
      return
    end

    # Else, launch
    self.launch options
  end

  # Call the appropriate launcher if we find one, passing it line
  def self.launch options={}
    Tree.plus_to_minus

    Line.sub! /^[@*-]$/, '...'   # Expand "." to auto-complete all paths
    Line.sub! /^\.$/, './'   # Expand "." to auto-complete all paths
    Line.sub! /^~$/, '~/'   # Expand "." to auto-complete all paths

    # Maybe don't blink when in $__small_menu_box!"
    Effects.blink(:what=>:line) if options[:blink]
    line = options[:line] || Line.value   # Get paren from line

    label = Line.label(line)

    # Special hooks for specific files and modes
    return if self.file_and_mode_hooks

    View.bar if Keys.prefix == 7

    $xiki_no_search = options[:no_search]   # If :no_search, disable search

    is_root = false

    if line =~ /^( *)[+-] [^\n\(]+?\) (.+)/   # Split off label, if there
      line = $1 + $2
    end
    if line =~ /^( *)[+-] (.+)/   # Split off bullet, if there
      line = $1 + $2
    end
    if line =~ /^ *@ ?(.+)/   # Split off @ and indent if @ exists
      is_root = true
      line = $1
    end

    @@launchers.each do |regex, block|   # Try each potential regex match
      # If we found a match, launch it
      if line =~ regex

        group = $1

        # Run it
        if @@just_show
          Ol << "- regex: #{regex.to_s}\n- group: #{group}"
        else
          block.call line
        end
        $xiki_no_search = false
        return true
      end
    end

    # If current line is indented and not passed recursively yet, try again, passing tree

    if Line.value =~ /^ / && ! options[:line] && !is_root   # If indented, call .launch recursively
      Tree.plus_to_minus

      # Use Xiki.branch here? (breaks up by @'s)

      # merge together (spaces if no slashes) and pass that to launch
      list = Tree.construct_path(:list=>true)   # Get path to pass to procs, to help them decide
      found = list.index{|o| o =~ /^@/} and list = list[found..-1]   # Remove before @... node if any
      merged = list.map{|o| o.sub /\/$/, ''}.join('/')
      merged << "/" if list[-1] =~ /\/$/
      return self.launch options.slice(:no_search).merge(:line=>merged)
    end

    if self.launch_by_proc   # Try procs (currently all trees)
      return $xiki_no_search = false
    end

    if @@just_show
      return $xiki_no_search = false
    end

    # If nothing found so far, don't do anything if...
    if line =~ /^\|/
      View.beep
      return View.message "Don't know what to do with this line"
    end

    # See if it matches path launcher

    return if self.try_menu_launchers line

    # Try to auto-complete based on menu-launchers

    if line =~ /^([\w -]*)(\.\.\.)?\/?$/

      # TODO just check for exact match in dir, and load it if no launcher yet!

      root = $1
      root.gsub!(/[ -]/, '_') if root
      matches = self.menu_keys.select do |possibility|
        possibility =~ /^#{root}/
      end
      if matches.any?
        if matches.length == 1
          Line.sub! /^([ +-]*).*/, "\\1#{matches[0]}"
          Launcher.launch
          return
        end
        Line.sub! /\b$/, "..."

        View >> matches.sort.map{|o| "#{o}"}.join("\n")
        return
      end
    end

    # If child of "..." autocomplete line

    if line =~ /^[\w -]*\.\.\.\/(\w+)$/
      Tree.to_parent
      Tree.kill_under
      Line.sub! /([ @+-]*).+/, "\\1#{$1}"
      Launcher.launch
      return
    end

    # If just root line, load any unloaded launchers this completes and relaunch

    if line =~ /^([\w -]+)\/?$/
      root = $1
      root.gsub!(/[ -]/, '_') if root

      ["~/menus", Bookmarks["$x/menus"]].each do |dir|

        matches = Dir[File.expand_path("#{dir}/#{root}*")]
        Ol << "matches: #{matches.inspect}"

        if matches.any?
          matches.sort.each do |file|
            iroot = file[/\/(\w+)\./, 1]
            next if @@menus[0][root] || @@menus[1][root]   # Skip if already loaded
            require_menu(file)  # if File.exists? file
          end
          return self.launch   # options.slice(:no_search).merge(:line=>merged)
        end

      end
    end

    if root = line[/^[\w -]+/]
      Xiki.dont_search
      # Maybe make the following print out optionally, via a 'help_last' block?
      Tree << "
        | There's no \"#{root}\" menu yet. Create it?
        |
        | You can just start making the menu.
        + @get me started/
        |
        | Or, use a template to create a menu file or class directly.
        + @menu/setup/create/
        "
    else
      View.glow "- No launcher matched!"
    end

    $xiki_no_search = false
  end

  def self.try_menu_launchers line
    root = line[/^[\w -]+/]   # Grab thing to match
    root.gsub!(/[ -]/, '_') if root

    block_dot_menu = @@menus[0][root]
    block_other = @@menus[1][root]

    return false if block_dot_menu.nil? && block_other.nil?

    self.append_log line
    trunk = Xiki.trunk

    if trunk.size > 1 && FileTree.matches_root_pattern?(trunk[-2])
      orig_pwd = Dir.pwd
      Dir.chdir trunk[-2] rescue nil
    end

    # If there was a menu, call it

    out = nil
    if block_dot_menu
      begin
        out = Tree.output_and_search block_dot_menu, :line=>line  #, :dir=>file_path
      ensure
        Dir.chdir orig_pwd if file_path
      end

      # If .menu file matched but had no output, and no other block to delegate to, say we handled it so it will stop looking
      if ! out && ! block_other
        View.glow "- This menu has no child items!"
        return true
      end

      return true if out   # If there was no output, continue on and try class
    end

    if block_other
      begin
        Tree.output_and_search block_other, :line=>line  #, :dir=>file_path
      ensure
        Dir.chdir orig_pwd if file_path
      end

      return true
    end

    false   # No match, keep looking
  end

  def self.launch_by_proc list=nil
    list = Tree.construct_path(:list=>true)   # Get path to pass to procs, to help them decide

    # Try each proc
    @@launchers_procs.each do |launcher|   # For each potential match
      condition_proc, block = launcher
      if found = condition_proc.call(list)   # If we found a match, launch it
        if @@just_show
          Ol << condition_proc.to_ruby
        else
          block.call list[found..-1]
        end
        return true
      end
    end
    return false
  end

  def self.init_default_launchers

    self.add /^h$/ do   # $ run command in shell
      log = self.log.strip
      Line.sub! /.+/, log

      left, right = View.paragraph :bounds=>true
      Tree.search :left=>left, :right=>left+log.length+1
    end

    self.add /^ *\$ / do   # $ run command in shell
      Console.launch_dollar
    end

    self.add(/^ *[+-]? *(http|file).?:\/\/.+/) do   # url
      line = Line.content

      Launcher.append_log "- urls/#{line}"

      prefix = Keys.prefix
      Keys.clear_prefix

      url = line[/(http|file).?:\/\/.+/]
      if prefix == 8
        Tree.under RestTree.request("GET", url), :escape=>'| '
        next
      end
      url.gsub! '%', '%25'
      url.gsub! '"', '%22'
      prefix == :u ? $el.browse_url(url) : Firefox.url(url)
    end

    self.add(/^[ +-]*\$[^#*!\/]+$/) do |line|   # Bookmark
      View.open Line.without_indent(line)
    end

    self.add(/^ *p /) do |line|
      CodeTree.run line
    end

    self.add(/^ *pp /) do |line|
      CodeTree.run line
    end

    self.add(/^ *puts /) do |line|
      CodeTree.run line
    end

    self.add(/^ *print\(/) do |line|
      Javascript.launch
    end

    self.add(/^[^|-]+\*\*.+\//) do |line|  # **.../: Tree grep in dir
      FileTree.launch
    end

    self.add(/^[^|]+##.+\//) do |line|  # ##.../: Tree grep in dir
      FileTree.launch
    end

    self.add "google" do |line|
      line.sub! /^google\/?/, ''
      line.sub! /\/$/, ''

      if line.blank?   # If no path, pull from history
        next Launcher.last "google", :exclude_path=>1
      end
      url = line.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      $el.browse_url "http://www.google.com/search?q=#{url}"
      nil
    end

    self.add(/^ *$/) do |line|  # Empty line: insert CodeTree menu
      View.beep
      View.message "There was nothing on this line to launch."
    end

    self.add(/^\*/) do |line|  # *... buffer
      name = Line.without_label.sub(/\*/, '')
      View.to_after_bar
      View.to_buffer name
    end

    self.add(/^ *!/) do |l|   # ! shell command inline
      Console.launch :sync=>true
    end

    self.add(/^[^\|@:]+[\/\w\-]+\.\w+:\d+/) do |line|  # Stack traces, etc

      # Match again (necessary)
      line =~ /([$\/.\w\-]+):(\d+)/
      path, line = $1, $2

      # If relative dir, prepend current dir
      if path =~ /^\w/
        path = "#{View.dir}/#{path}"
        path.sub! "//", "/"   # View.dir sometimes ends with slash
      end

      View.open path
      View.to_line line.to_i
    end

    # Xiki protocol to server
    self.add(/^[a-z-]{2,}\.(com|net|org|loc|in|edu|gov|uk)(\/|$)/) do |line|  # **.../: Tree grep in dir
      Line << "/" unless Line =~ /\/$/
      url = "http://#{line}"
      url.sub! /\.\w+/, "\\0/xiki"
      url.gsub! ' ', '+'
      response = HTTParty.get(url)
      View.under response.body
    end

    # Menu launchers

    Launcher.add "log" do
      Launcher.log :at=>1
    end

    Launcher.add "last" do |path|
      Launcher.last path
    end

    # ...Tree classes

    # RestTree
    condition_proc = proc {|list| RestTree.handles? list}
    Launcher.add condition_proc do |list|
      RestTree.launch :path=>list
    end

    # FileTree
    condition_proc = proc {|list| FileTree.handles? list}
    Launcher.add condition_proc do |list|
      FileTree.launch list
    end

    # CodeTree
    condition_proc = proc {|list| CodeTree.handles? list}
    Launcher.add condition_proc do |list|
      CodeTree.launch :path=>list
    end

    # UrlTree
    condition_proc = proc {|list| UrlTree.handles? list}
    Launcher.add condition_proc do |list|
      UrlTree.launch :path=>list
    end


  end

  def self.file_and_mode_hooks
    if View.mode == :dired_mode
      filename = $el.dired_get_filename
      # If dir, open tree
      if File.directory?(filename)
        FileTree.ls :dir=>filename
      else   # If file, do full file search?
        History.open_current :all => true, :paths => [filename]
      end
      return true
    end
    if View.name =~ /_ol\.notes$/   # If in an ol output log file
      Code.ol_launch
      Effects.blink(:what=>:line)
      return true
    end
    return false
  end

  def self.do_last_launch
    orig = View.index

    CLEAR_CONSOLES.each do |buffer|
      View.clear buffer
    end

    prefix = Keys.prefix :clear=>true

    if prefix ==:u
      View.to_nth orig
    else
      Move.to_window 1
    end

    line = Line.value

    # Go to parent and collapse, if not at left margin
    if line =~ /^ / #&& line !~ /^ *[+-] /  # and not a bullet
      Tree.to_parent
    end
    Tree.kill_under

    Launcher.launch_or_hide :blink=>true, :no_search=>true
    View.to_nth orig
  end

  def self.urls
    txt = File.read File.expand_path("~/.emacs.d/url_log.notes")
    txt = txt.split("\n").reverse.uniq.join("\n")
  end

  def self.enter_last_launched
    Launcher.insert self.last_launched_menu
  end

  def self.last_launched_menu
    bm = Keys.input(:timed => true, :prompt => "bookmark to show launches for (* for all): ")

    menu =
      if bm == "8" || bm == " "
        "- search/.launched/"
        #         "- Search.launched/"
      elsif bm == "."
        "- Search.launched '#{View.file}'/"
      elsif bm == "3"
        "- Search.launched '#'/"
      elsif bm == ";" || bm == ":" || bm == "-"
        "- Search.launched ':'/"
      else
        "- search/.launched/$#{bm}/"
      end
  end


  def self.do_as_launched
    txt = "- #{View.file_name}\n    | #{Line.value}"
    txt.sub!("| ", "- #{Keys.input(:prompt => "enter label: ")}: | ") if Keys.prefix_u
    Search.append_log "#{View.dir}/", txt
  end

  def self.invoke clazz, path
    default_method = "menu"
    # If dot extract it as method
    if clazz =~ /\./
      clazz, default_method = clazz.match(/(.+)\.(.+)/)[1..2]
    end

    # Allow clazz to be a .tree file as well?

    if clazz.is_a? String
      # Require it to be camel case (because .invoke will be Menu.call "Class"
      # if lower case, will assume Menu.call "path"
      camel = TextUtil.camel_case clazz
      clazz = $el.el4r_ruby_eval(camel) rescue nil
    elsif clazz.is_a? Class
      camel = clazz.to_s
    end

    snake = TextUtil.snake_case camel

    raise "No class '#{clazz}' found in launcher" if clazz.nil?

    # reload 'path_to_class'
    Menu.load_if_changed File.expand_path("~/menus/#{snake}.rb")

    args = path.split "/"
    args.shift

    txt = nil

    # Maybe call .menu with no args to actionize and get child menus

    method = clazz.method(default_method) rescue nil
    if method && method.arity == 0
      code = "#{camel}.#{default_method}"
      returned, out, exception = Code.eval code
      return CodeTree.draw_exception exception, code if exception
      return nil if returned.nil?
      txt = CodeTree.returned_to_s returned   # Convert from array into string, etc.
    end

    # Maybe use .menu file

    if txt.nil?
      if File.exists?((filename = File.expand_path("~/menus/#{snake}.menu")) || (filename = File.expand_path("#{Xiki.dir}/menus/#{snake}.menu")))
        # Test getting .menu files from ~ and $x ?
        txt = File.read filename
      end
    end

    # Error if no menu method or file
    raise "#{clazz} doesn't seem to have a .#{default_method} method, and there's no #{TextUtil.snake_case camel}.menu file in ~/menus!" if method.nil? && txt.nil?


    # Maybe use .route method to just actionify path (if .menu existed with args)
    #     if txt.nil?
    #       # They didn't want .menu to be arbitrarily invokeable, but if self.route, try that
    #       route_method = clazz.method('route') rescue nil
    #       if route_method && route_method.arity == 0
    #         code = "#{camel}.route"
    #         returned, out, exception = Code.eval code
    #         return CodeTree.draw_exception exception, code if exception
    #         raise "- We invoked #{camel}.menu with no args, but it didn't return anything." if returned.nil?
    #         txt = CodeTree.returned_to_s returned   # Convert from array into string, etc.
    # Ol << "txt from .route: #{txt.inspect}"
    #       end
    #     end

    # If got routable menu text, use it to routify

    if txt
      txt = txt.unindent if txt =~ /\A[ \n]/
      raise "#{code} returned nil, but is supposed to return something when it takes no arguments" if txt.nil?

      txt = Tree.routify! txt, args
      return txt if txt && txt != "- */\n"

    end   # Else, continue on to run it based on routified path

    # Figure out which ones are actions

    # TODO Maybe extract this out into another method, so trees can call it internally

    # Find last .foo item
    actions, variables = args.partition{|o|
      o =~ /^\./
      # Also use result of .menu to determine
    }
    action = actions.last || ".#{default_method}"
    action.gsub! /[ -]/, '_'

    # TODO: Do we want to always group quotes, or should menus optionally internally call Tree.leaf?!"

    # Decided to let users call Xiki.leaves
    #     if args[-1] =~ /^ *\|/
    #       args[-1].replace(Tree.leaf args[-1])
    #     end

    args = variables.map{|o| "\"#{CodeTree.escape o}\""}.join(", ")

    code = "#{camel}#{action} #{args}".strip
    txt, out, exception = Code.eval code
    txt = CodeTree.returned_to_s(txt)   # Convert from array into string, etc.
    txt = txt.unindent if txt =~ /\A[ \n]/

    return CodeTree.draw_exception exception, code if exception

    txt = Tree.quote(txt) if txt

    txt
  end

  def self.add_class_launchers classes
    classes.each do |clazz|
      next if clazz =~ /\//

      # Why is this line causing an error??
      #       clazz = $el.el4r_ruby_eval(TextUtil.camel_case clazz) rescue nil
      #       method = clazz.method(:menu) rescue nil
      #       next if method.nil?

      self.add clazz do |path|
        Launcher.invoke clazz, path
      end
    end
  end

  def self.append_log path

    return if View.name =~ /_log.notes$/

    path = path.sub /^[+-] /, ''   # Remove bullet
    path = "#{path}/" if path !~ /\//   # Append slash if just root without path

    return if path =~ /^(log|last)(\/|$)/

    path = "- #{path}"
    File.open(@@log, "a") { |f| f << "#{path}\n" } rescue nil
  end

  def self.insert txt
    View.insert txt
    $el.open_line(1)
    Launcher.launch
  end

  def self.open menu, options={}
    View.bar if Keys.prefix == 1

    dir = View.dir

    # For buffer name, handle multi-line strings
    buffer = "*CodeTree " + menu.sub(/.+\n[ -]*/m, '').gsub(/[.,]/, '')
    View.to_buffer(buffer, :dir=>dir)

    View.clear
    $el.notes_mode
    View.wrap :off

    txt = menu

    if txt.blank?
      return View.insert("\n", :dont_move=>1)
    end

    dir = options[:dir] and txt = "- #{dir.sub /\/$/, ''}/\n  - #{txt}"

    View << txt

    $el.open_line 1

    if options[:choices]
      View.to_highest
      Tree.search
      return
    end

    Launcher.launch options
  end

  def self.method_missing *args, &block

    arg = args.shift

    if block.nil?
      if args == []   # Trying to call menu with no args
        return Menu.call arg.to_s
      end
      if args.length == 1 && args[0].is_a?(String)   # Trying to call menu with args / path?
        return
      end
    end

    raise "Menu.#{arg} called with no block and no args" if args == [] && block.nil?
    self.add arg.to_s, args[0], &block
  end

  def self.wrapper
    path = Tree.construct_path #(:list=>true)
    path.sub /^\//, ''
    dir, stem = File.dirname(path), File.basename(path)
    self.wrapper_rb dir, stem
  end

  def self.wrapper_rb dir, stem

    output = Console.run "ruby #{Xiki.dir}/etc/wrapper.rb #{stem}", :sync=>1, :dir=>dir
    Tree << output
  end

  def self.reload_menu_dirs

    MENU_DIRS.each do |dir|
      next unless File.directory? dir

      Files.in_dir(dir).each do |f|
        next if f !~ /^[a-z]/   # Skip ^. files
        path = "#{dir}/#{f}"
        stem = f[/[^.]*/]
        self.add stem, :menu=>path
      end
    end
    "- reloaded!"
  end

  def self.search_like_menu

    txt = Search.stop
    return if txt.nil?

    menu = Keys.input :timed => true, :prompt => "Enter menu to pass '#{txt}' to: "

    matches = Launcher.menu_keys.select do |possibility|
      possibility =~ /^#{menu}/
    end

    if matches.length == 1
      return Launcher.open("- #{matches[0]}/#{txt}")
    end

    Launcher.open(matches.map{|o| "- #{o}/#{txt}\n"}.join(''), :choices=>1)
  end
end

def require_menu file
  stem = file[/(\w+)\./, 1]

  # As .menu

  if file =~ /\.menu$/
    Launcher.add stem, :menu_file=>1 do |path|
      next View.glow("- Xiki couldn't find: #{file}", :times=>6) if ! File.exists?(file)
      Tree.climb File.read(file), Tree.rootless(path)
    end
    return
  end

  # As class, so require and add launcher

  Menu.load_if_changed file

  Launcher.add stem
end

Launcher.init_default_launchers
