require 'effects'
require 'ruby_console'
require 'xiki'
gem 'httparty'; require 'httparty'
gem 'activesupport'; require 'active_support/ordered_hash'

class Launcher
  extend ElMixin

  CLEAR_CONSOLES = [
    "*ol",
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
    result = IO.readlines self.log_file
    result = result.map{|o| o.sub /^- #{Notes::LABEL_REGEX}/, '\\0@'} if options[:at]
    result.reverse.uniq.join
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
        Tree.children menu, Tree.rootless(path)
      end
      return
    end

    raise "Don't know how to deal with: #{root}, #{hash}, #{block}"
  end

  def self.launch_or_hide options={}
    # If no prefixes and children exist, delete under
    if ! Keys.prefix and ! Line.blank? and Tree.children?
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

          begin
            block.call line
          rescue RelinquishException
            next   # They didn't want to handle it, keep going
          end

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
        | There's no \"#{root}\" menu yet. Create it? You can start by adding items
        | right here, or you can create a class.
        <= @menu/create/here/
        <= @menu/create/class/
        "
    else
      View.flash "- No launcher matched!"
    end

    $xiki_no_search = false
  end

  def self.try_menu_launchers line
    root = line[/^[\w -]+/]   # Grab thing to match
    root.gsub!(/[ -]/, '_') if root

    self.append_log line
    trunk = Xiki.trunk

    orig_pwd = nil
    if trunk.size > 1 && closest_dir = Tree.closest_dir

      # TODO: we're only doing trunk[-2] - should we find the closest file path?? - Tree.closest_dir

      orig_pwd = Dir.pwd   # Where ruby pwd was before

      begin
        Dir.chdir closest_dir
      rescue
        if root == "mkdir"
          Dir.chdir "/tmp/"
        else
          Tree.<<("| Directory '#{closest_dir}' doesn't exist. Create it?\n- @mkdir/")
          return true
        end
      end
    end

    # If there was a menu, call it

    out = nil
    if block_dot_menu = @@menus[0][root]
      begin
        out = Tree.output_and_search block_dot_menu, :line=>line  #, :dir=>file_path
      ensure
        Dir.chdir orig_pwd if orig_pwd
      end

      # If .menu file matched but had no output, and no other block to delegate to, say we handled it so it will stop looking

      if ! out
        require_menu File.expand_path("~/menus/#{root}.rb"), :ok_if_not_found=>1
        if ! @@menus[1][root]
          Tree << "
            | This menu item does nothing yet.  You can update the .menu file to
            | give it children or create a class to give it dynamic behavior:
            <= @menu/create/class/
            "
          return true
        end
      end

      return true if out   # Output means we handled it, otherwise continue on and try class
    end

    if block_other = @@menus[1][root]   # If class menu
      begin
        Tree.output_and_search block_other, :line=>line  #, :dir=>file_path
      ensure
        Dir.chdir orig_pwd if orig_pwd
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

    self.add /^h$/ do   # experiment - ^h to insert history - keep this?
      log = self.log.strip
      Line.sub! /.+/, log

      left, right = View.paragraph :bounds=>true
      Tree.search :left=>left, :right=>left+log.length+1
    end

    self.add(/^ *\$ /) do |l|   # $ shell command inline (sync)
      Console.launch :sync=>true
    end

    self.add /^ *%( |$)/ do   # $ shell command (async)
      Console.launch_async
    end

    self.add(/^ *[+-]? *(http|file).?:\/\/.+/) do   # url
      line = Line.content

      Launcher.append_log "- urls/#{line}"

      prefix = Keys.prefix
      Keys.clear_prefix

      url = line[/(http|file).?:\/\/.+/]
      if prefix == "all"
        Tree.under RestTree.request("GET", url), :escape=>'| '
        next
      end
      url.gsub! '%', '%25'
      url.gsub! '"', '%22'
      prefix == :u ? $el.browse_url(url) : Firefox.url(url)
    end

    self.add(/^[ +-]*\$[^ #*!\/]+$/) do |line|   # Bookmark
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

    self.add(/^ *$/) do |line|  # Empty line
      View.beep
      View.message "There was nothing on this line to launch."
    end

    self.add(/^\*/) do |line|  # *... buffer
      name = Line.without_label.sub(/\*/, '')
      View.to_after_bar
      View.to_buffer name
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

    Launcher.add /^  +<+ .+/ do
      Menu.collapser_launcher
    end

    Launcher.add /^  +<+= .+/ do
      Menu.replacer_launcher
    end

    Launcher.add /^[a-z]+\+[a-z+]+\/?$/ do |path|
      Tree << %`
        | If you were told to "type #{path}", it is meant that you should
        | "type the acronym" while holding down control. This means Meaning
        | you should type:
        |
        |   #{Keys.human_readable(path)}
        `
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
    if View.name =~ /^\*ol/   # If in an ol output log file
      OlHelper.launch
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

  # Used any more? - should be replaced by menu log - delete this
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
        "- search/launched/"
        #         "- Search.launched/"
      elsif bm == "."
        "- Search.launched '#{View.file}'/"
      elsif bm == "3"
        "- Search.launched '#'/"
      elsif bm == ";" || bm == ":" || bm == "-"
        "- Search.launched ':'/"
      else
        "- search/launched/$#{bm}/"
      end
  end


  def self.do_as_launched
    txt = "- #{View.file_name}\n    | #{Line.value}"
    txt.sub!("| ", "- #{Keys.input(:prompt => "enter label: ")}: | ") if Keys.prefix_u
    Search.append_log "#{View.dir}/", txt
  end

  def self.invoke clazz, path, options={}
    default_method = "menu"
    # If dot, extract it as method
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

    args = path.is_a?(Array) ?
      path :
      path.split("/")[1..-1]

    # Maybe call .menu_before

    method = clazz.method("menu_before") rescue nil
    self.set_env_vars args
    if method
      code = "#{camel}.menu_before *#{args.inspect}"
      returned, out, exception = Code.eval code

      return CodeTree.draw_exception exception, code if exception
      if returned != nil   # Only a nil returned means don't call .menu

        # TODO: call .unset_env_vars before this and other below places we return

        return if ! returned.is_a?(String)

        returned = returned.unindent if returned =~ /\A[ \n]/
        return returned
      end

    end


    # Maybe call .menu with no args to actionize and get child menus

    txt = options[:tree]

    if txt.nil?
      method = clazz.method(default_method) rescue nil
      if method && method.arity == 0
        code = "#{camel}.#{default_method}"
        returned, out, exception = Code.eval code
        return CodeTree.draw_exception exception, code if exception
        return nil if returned.nil?
        txt = CodeTree.returned_to_s returned   # Convert from array into string, etc.
      end
    end

    # Error if no menu method or file
    raise "#{clazz} doesn't seem to have a .#{default_method} method!" if method.nil? && txt.nil?

    # If got routable menu text, use it to routify

    if txt
      txt = txt.unindent if txt =~ /\A[ \n]/
      raise "#{code} returned nil, but is supposed to return something when it takes no arguments" if txt.nil?

      txt = Tree.routify! txt, args
      if txt && txt != "- */\n"

        # TODO Call .menu_after!"
        # Pass in output of menu as either...
          # ENV['output']
          # 1st parameter: .menu_after output, *args

        return txt
      end
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

    args = variables.map{|o| "\"#{CodeTree.escape o}\""}.join(", ")

    # TODO: use adapter here, so we can call .js file?


    # TODO .menu_after: Check for arity - if mismatch, don't call, but go straight to .menu_after!
    # We could probably not worry about this for now?


    code = "#{camel}#{action.downcase} #{args}".strip
    txt, out, exception = Code.eval code
    txt = CodeTree.returned_to_s(txt)   # Convert from array into string, etc.

    self.unset_env_vars

    txt = txt.unindent if txt =~ /\A[ \n]/
    return CodeTree.draw_exception exception, code if exception

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
    View.to_after_bar if View.in_bar?

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

  def self.as_update
    Keys.prefix = "save"
    Launcher.launch
  end

  def self.as_destroy
    Keys.prefix = "destroy"
    Launcher.launch
  end

  def self.enter_all
    return FileTree.enter_lines(/.*/) if Line.blank?

    Keys.prefix = "all"
    Launcher.launch
  end

  def self.enter_outline
    return FileTree.enter_lines if Line.blank?

    Keys.prefix = "outline"
    Launcher.launch
  end

  def self.set_env_vars args

    ENV['prefix'] = Keys.prefix.to_s

    if args[-1] !~ /^ *\|/
      return ENV['txt'] = args[-1]
    end

    # Quoted lines

    txt = Tree.leaf(args[-1])
    ENV['txt'] = txt.length > 1_000_000 ? "*too long to put into env var*" : txt
    # Put into file instead?
  end

  def self.unset_env_vars
    ENV['prefix'] = nil
    ENV['txt'] = nil
  end
end

def require_menu file, options={}
  if ! options[:ok_if_not_found]
    raise "File Not Found" if !File.exists?(file)
  end

  stem = file[/(\w+)\./, 1]

  # As .menu

  if file =~ /\.menu$/
    Launcher.add stem, :menu_file=>1 do |path|
      next View.flash("- Xiki couldn't find: #{file}", :times=>5) if ! File.exists?(file)
      Tree.children File.read(file), Tree.rootless(path)
    end
    return
  end

  # As class, so require and add launcher

  result = Menu.load_if_changed file
  return if result == :not_found

  Launcher.add stem if ! Launcher.menus[1][stem]
end

Launcher.init_default_launchers
