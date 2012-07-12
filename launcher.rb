require 'effects'

require 'requirer'

# require 'xiki'
Requirer.require_gem 'httparty'   # Not super-important
Requirer.require_gem 'activesupport', :name2=>'active_support/ordered_hash'
Requirer.require_gem 'haml', :optional=>1

class Launcher
  extend ElMixin

  CLEAR_CONSOLES = [
    "*ol",
    "*output - tail of /tmp/ds_ol.notes",
    "*visits - tail of /tmp/visit_log.notes",
    "*console app",
    ]

  MENU_DIRS = [
    "#{Xiki.dir}menus",
    File.expand_path("~/menus"),
    ]

  @@log = File.expand_path("~/.emacs.d/menu_log.notes")

  # Use @launcher/options/show or launch/ to enable.
  # Look in /tmp/output.notes
  @@just_show = false

  @@launchers ||= ActiveSupport::OrderedHash.new
  @@launchers_procs ||= []
  @@launchers_parens ||= {}
  @@menus ||= [{}, {}]   # [.menu files, .rb files]

  def self.menus
    @@menus
  end

  def self.menu_keys
    (@@menus[0].keys + @@menus[1].keys).sort.uniq #.select do |possibility|
  end

  def self.menu
    %`
    - .options/
      > Toggle Temporarily just showing the launcher that matched
      - .show or launch/
    - docs/
      > Summary
      Launcher is the class that handles "launching" things (double-clicking on a
      line, or typing Ctrl-enter).

      > See
      @launcher/api/
    - api/
      > Open menu in new buffer
      @ Launcher.open "computer"

      > Insert monu
      @ Launcher.insert "computer"   # Assumes you're on a blank line

      > Invoke (used behind the scenes?)
      @ p Launcher.invoke 'Computer', 'computer/ip/'
    `
  end

  def self.show_or_launch
    @@just_show = ! @@just_show
    View.flash "- Will #{@@just_show ? 'just show' : 'actually launch'}!"
  end

  def self.last path=nil, options={}
    path = path.sub /^last\/?/, '' if path

    paths = IO.readlines self.log_file

    # If nothing passed, just list all roots

    if path.blank?
      paths.map!{|o| o.sub /\/.+/, '/'}   # Cut off after path
      paths = paths.reverse.uniq
      paths.delete "- #{options[:omit]}/\n" if options[:omit]
      return paths.join
    end

    # Root passed, so show all matches

    paths = paths.select{|o| o =~ /^- #{Notes::LABEL_REGEX}#{path}\/./}

    bullet = options[:quoted] ? "|" : "-"

    if options[:exclude_path]
      paths.each{|o| o.sub! /^- (#{Notes::LABEL_REGEX})#{path}\//, "#{bullet} \\1"}
      paths = paths.select{|o| o != "#{bullet} "}
    else
      paths = paths.map{|o| o.sub /^- #{Notes::LABEL_REGEX}/, '\\0@'}
    end
    paths = paths.reverse.uniq
    paths.delete_if{|o| o == "| \n"}
    paths.join
  end

  def self.log

    lines = IO.readlines self.log_file

    # If parent, narrow down to just it
    trunk = Xiki.trunk
    if trunk.length > 1 && trunk[-2] != "menu/history"   # Show all if under this menu
      lines = lines.select {|o| o.start_with? "- #{trunk[-2]}"}
    end

    lines.reverse.uniq.map{|o| o.sub /^- /, '<< '}.join
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
    elsif hash.is_a?(Hash) && hash[:menu_file] && block
      return @@menus[0][root] = block
    end

    # If just root, we'll use use class with that name
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

  def self.hide
    Tree.kill_under
  end

  # Call the appropriate launcher if we find one, passing it line
  def self.launch options={}

    # Add linebreak at end if at end of file and none
    Line.<<("\n", :dont_move=>1) if Line.right == View.bottom

    Tree.plus_to_minus unless options[:leave_bullet]

    Line.sub! /^\.$/, './'
    Line.sub! /^~$/, '~/'

    # Maybe don't blink when in $__small_menu_box!"
    Effects.blink(:what=>:line) if options[:blink]
    line = options[:line] || Line.value   # Get paren from line
    label = Line.label(line)

    if line =~ /^ *@$/
      matches = Launcher.menu_keys
      Tree.<< matches.sort.map{|o| "<< #{o.sub '_', ' '}/"}.join("\n"), :no_slash=>1
      return
    end

    # Special hooks for specific files and modes
    return if self.file_and_mode_hooks

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

    # Special case to turn launchers back on
    return self.show_or_launch if line == "launcher/options/show or launch/"

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
          rescue Exception=>e
            Tree.<< CodeTree.draw_exception(e, block.to_ruby), :no_slash=>ENV['no_slash']
          end

        end
        $xiki_no_search = false
        return true
      end
    end

    # If current line is indented and not passed recursively yet, try again, passing tree

    if Line.value =~ /^ / && ! options[:line] && !is_root   # If indented, call .launch recursively

      # Use Xiki.branch here? (breaks up by @'s)

      # merge together (spaces if no slashes) and pass that to launch

      list = Tree.construct_path :list=>true, :ignore_ol=>1   # Get path to pass to procs, to help them decide

      found = list.index{|o| o =~ /^@/} and list = list[found..-1]   # Remove before @... node if any
      merged = list.map{|o| o.sub /\/$/, ''}.join('/')
      merged << "/" if list[-1] =~ /\/$/

      # Recursively call again with full path
      return self.launch options.merge(:line=>merged)

      # What was this doing, did we mean to only pass on :no_search??
      #       return self.launch options.slice(:no_search).merge(:line=>merged)
    end

    if self.launch_by_proc   # Try procs (currently all trees)
      return $xiki_no_search = false
    end

    # If nothing found so far, don't do anything if...
    if line =~ /^\|/
      View.beep
      return View.message "Don't know what to do with this line"
    end

    # See if it matches path launcher

    self.set_env_vars line

    result = self.try_menu_launchers line, options
    self.unset_env_vars
    return if result

    if line =~ /^([\w -]*)$/ || line =~ /^([\w -]*)\.\.\.\/?$/

      #     if line =~ /^([\w -]*)(\.\.\.)?\/?$/
      # TODO just check for exact match in dir, and load it if no launcher yet!

      root = $1
      root.gsub!(/[ -]/, '_') if root
      matches = self.menu_keys.select do |possibility|
        possibility =~ /^#{root}/
      end
      if matches.any?
        if matches.length == 1
          match = matches[0].gsub '_', ' '
          Line.sub! /^([ @+-]*).*/, "\\1#{match}"
          Launcher.launch
          return
        end

        Line.sub! /\b$/, "..."

        View.under matches.sort.map{|o| "<< #{o.sub '_', ' '}/"}.join("\n")
        return
      end
    end

    # If just root line, load any unloaded launchers this completes and relaunch

    # Failed attempt to not auto-complete if slash
      # It's tough because we still want to load!
    # Don't do if ends with slash? - does this mean it won't load unloaded?

    if line =~ /^([\w -]+)\/?$/ && ! options[:recursed]
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
          return self.launch :recursed=>1   # options.slice(:no_search).merge(:line=>merged)
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
        <= @menu/install/gem/
        "
    else
      View.flash "- No launcher matched!"
    end
    $xiki_no_search = false
  end

  def self.try_menu_launchers line, options={}

    # If there's a /@ in the path, cut it off
    line.sub! /.+\/@/, ''

    root_orig = root = line[/^[\w -]+/]   # Grab thing to match
    root = TextUtil.snake_case root if root

    self.append_log line
    trunk = Xiki.trunk

    # If menu nested under dir or file, chdir first

    orig_pwd = nil
    if trunk.size > 1 && closest_dir = Tree.closest_dir
      orig_pwd = Dir.pwd   # Where ruby pwd was before

      if root == "mkdir"
        Dir.chdir "/tmp/"
      elsif File.directory?(closest_dir) || is_file = File.file?(closest_dir)   # If dir path
        closest_dir = File.dirname closest_dir if is_file

        Dir.chdir closest_dir

        # If file, make path only have dir
        # remove file

      else   # If doesn't exist
        View.beep "- Dir doesn't exist: #{closest_dir}"
        return true
      end
    end

    # If there is a matching .menu, use it

    out = nil
    if block_dot_menu = @@menus[0][root]

      if @@just_show
        Ol.line "Maps to .menu file, for menu: #{root}\n - #{block_dot_menu}\n - #{block_dot_menu.to_ruby}"
        View.flash "- Showed launcher in $o", :times=>4
        return true   # To make it stop trying to run it
      end

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

    # If there is a matching .rb for the menu, use it

    if block_other = @@menus[1][root]   # If class menu

      if @@just_show
        Ol.line << "Maps to class or other block, for menu: #{root}\n - #{block_other}\n - #{block_other.to_ruby}"
        View.flash "- Showed launcher in $o", :times=>4
        return true   # To make it stop trying to run it
      end

      begin
        Tree.output_and_search block_other, options.merge(:line=>line)  #, :dir=>file_path
      ensure
        Dir.chdir orig_pwd if orig_pwd
      end

      return true
    end

    # If uppercase, try invoking on in-memory class
    if root_orig =~ /^[A-Z]/

      if @@just_show
        Ol.line << "Maps to in-memory class for: #{root}"
        View.flash "- Showed launcher in $o", :times=>4
        return true   # To make it stop trying to run it
      end

      begin

        lam = lambda do |path|
          Launcher.invoke root_orig, path
        end

        #         Launcher.invoke__
        #         do |path|
        #           # Make class me camel case, and change Launcher.invoke to Menu.call
        #         end

        Tree.output_and_search lam, options.merge(:line=>line)  #, :dir=>file_path
      ensure
        Dir.chdir orig_pwd if orig_pwd
      end

      return true
    end

    # Pull into other function?
      # re-use code that calls class wrapper

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

    self.add(/^\$ /) do |l|   # $ shell command inline (sync)
      Console.launch :sync=>true
    end

    self.add /^%( |$)/ do   # % shell command (async)
      Console.launch_async
    end

    self.add /^&( |$)/ do   # % shell command in iterm
      Console.launch_async :iterm=>1
    end

    # %\n  | multiline\n  | commands
    Launcher.add /^\%\// do   # For % with nested quoted lines
      path = Tree.construct_path :list=>1

      next if path[-1] !~ /^\| /

      txt = Tree.siblings :string=>1

      orig = Location.new
      Console.to_shell_buffer
      View.to_bottom
      Console.enter txt
      orig.go
    end

    self.add(/^(http|file).?:\/\/.+/) do |path|
      Launcher.append_log "- urls/#{path}"

      prefix = Keys.prefix
      Keys.clear_prefix

      url = path[/(http|file).?:\/\/.+/]
      if prefix == "all"
        Tree.under RestTree.request("GET", url), :escape=>'| ', :no_slash=>1
        next
      end
      url.gsub! '%', '%25'
      url.gsub! '"', '%22'
      prefix == :u ? $el.browse_url(url) : Firefox.url(url)
    end

    self.add(/^\$[^ #*!\/]+$/) do |line|   # Bookmark
      View.open Line.without_indent(line)
    end

    self.add(/^(p )?[A-Z][A-Za-z]+\.(\/|$)/) do |line|
      line.sub! /^p /, ''
      Code.launch_dot_at_end line
    end

    self.add(/^p /) do |line|
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

    self.add(/^ *$/) do |line|  # Empty line
      View.beep
      View.message "There was nothing on this line to launch."
    end

    self.add(/^\*/) do |line|  # *... buffer
      name = Line.without_label.sub(/\*/, '')
      View.to_after_bar
      View.to_buffer name
    end

    # Must have at least 2 slashes!
    self.add(/^[^\|@:]+\/\w+\/[\/\w\-]+\.\w+:\d+/) do |line|  # Stack traces, etc
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
      self.web_menu line
    end

    self.add(/^localhost:?\d*(\/|$)/) do |line|
      self.web_menu line
    end

    self.add(/^ *(Ol\.line|Ol << )/) do
      View.layout_output :called_by_launch=>1
    end

    # Example code in method comments
    #   /tmp/foo.rb
    #     class Foo
    #       # Control-enter to run this line
    #       # Foo.bar
    #       def self.bar
    Launcher.add /^class (\w+)\/\#.+/ do |path|
      # Remove comment and run
      txt = Line.value.sub /^ +# /, ''
      result = Code.eval(txt)
      next Tree.<<(CodeTree.draw_exception(result[2], txt), :no_search=>1) if result[2]
      next Tree.<< result[0].to_s, :no_slash=>1 if result[0]   # Returned value
      Tree.<< result[1].to_s, :no_slash=>1 if result[1].any?   # Stdout
    end

    Launcher.add /^class (\w+)\/def self.menu\/(.+)/ do |path|
      clazz, path = path.match(/^class (\w+)\/def self.menu\/(.+)/)[1..2]

      path = "#{TextUtil.snake_case clazz}/#{path}".gsub("/.", '/')

      Tree << Menu[path]
    end

    Launcher.add /^  +<+@ .+/ do
      Menu.root_collapser_launcher
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

    Launcher.add "log" do # |path|
      Launcher.log# Tree.rootless(path)
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

  def self.do_last_launch options={}
    orig = View.index

    CLEAR_CONSOLES.each do |buffer|
      View.clear buffer
    end

    prefix = Keys.prefix :clear=>true

    if prefix ==:u || options[:here]
      View.to_nth orig
    else
      Move.to_window 1
    end

    line = Line.value

    # Go to parent and collapse, if not at left margin, and buffer modified (shows we recently inserted)
    if ! Color.at_cursor.member?("color-rb-light")   #&& line !~ /^ *[+-] /  # and not a bullet
      if line =~ /^ /
        Tree.to_parent
      end
      Tree.kill_under
    end

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

  def self.invoke clazz, path, options={}

    default_method = "menu"
    # If dot, extract it as method
    if clazz =~ /\./
      clazz, default_method = clazz.match(/(.+)\.(.+)/)[1..2]
    end

    if clazz.is_a? String
      # Require it to be camel case (because .invoke will be Menu.call "Class"
      # if lower case, will assume Menu.call "path"
      camel = TextUtil.camel_case clazz
      clazz = $el.instance_eval(camel, __FILE__, __LINE__) rescue nil

    elsif clazz.is_a? Class
      camel = clazz.to_s
    end

    snake = TextUtil.snake_case camel

    raise "No class '#{clazz || camel}' found in launcher" if clazz.nil?

    # reload 'path_to_class'
    Menu.load_if_changed File.expand_path("~/menus/#{snake}.rb")

    args = path.is_a?(Array) ?
      path : Menu.split(path, :rootless=>1)

    # Call .menu_before if there...

    method = clazz.method("menu_before") rescue nil

    self.set_env_vars path

    if method
      code = "#{camel}.menu_before *#{args.inspect}"
      returned, out, exception = Code.eval code

      return CodeTree.draw_exception exception, code if exception
      if returned

        # TODO: call .unset_env_vars before this and other below places we return

        returned = returned.unindent if returned =~ /\A[ \n]/
        return returned
      end
    end

    menu_arity = nil
    txt = options[:tree]

    # Call .menu with no args to get child menus or route to other method...

    if txt.nil?
      method = clazz.method(default_method) rescue nil
      if method && method.arity == 0
        menu_arity = 0
        code = "#{camel}.#{default_method}"
        returned, out, exception = Code.eval code
        return CodeTree.draw_exception exception, code if exception
        txt = CodeTree.returned_to_s returned   # Convert from array into string, etc.
      end
    end

    # Error if no menu method or file
    if method.nil? && txt.nil? && ! args.find{|o| o =~ /^\./}

      cmethods = clazz.methods - Class.methods
      return cmethods.sort.map{|o| ".#{o}/"}
    end


    # If got routable menu text, use it to route (get children or dotify)...

    if txt
      txt = txt.unindent if txt =~ /\A[ \n]/
      raise "#{code} returned nil, but is supposed to return something when it takes no arguments" if txt.nil?

      tree = txt

      txt = Tree.children tree, args

      if txt && txt != "- */\n"
        # Pass in output of menu as either:
          # ENV['output']
          # 1st parameter: .menu_after output, *args
        return self.invoke_menu_after clazz, txt, args
      end

      # Copy dots onto args, so last dotted one will be used as action

      Tree.dotify! tree, args

      # TODO: when to invoke this?
        # Maybe invoke even if there was no .menu method
          # Is that happening now?

      # If .menu_hidden exists, dotify based on its output as well...
      method = clazz.method("menu_hidden") rescue nil
      if method
        returned, out, exception = Code.eval "#{camel}.menu_hidden"

        if returned && returned.is_a?(String)
          returned = returned.unindent
          Tree.dotify! returned, args
        end
      end

    end
    # Else, continue on to run it based on routified path


    # TODO: Maybe extract this out into .dotified_to_ruby ?

    # Figure out which ones are actions

    # Last .dotted one is the action, and non-dotted are variables to pass
    actions, variables = args.partition{|o| o =~ /^\./ }
    action = actions.last || ".#{default_method}"
    action.gsub! /[ -]/, '_'
    action.gsub! /[^\w.]/, ''

    # Call .menu_after if appropriate...

    if action == ".menu" && txt == nil && menu_arity == 0
      return self.invoke_menu_after clazz, txt, args
    end

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

    txt = self.invoke_menu_after clazz, txt, args

    self.unset_env_vars

    txt
  end

  def self.invoke_menu_after clazz, txt, args
    camel = clazz.to_s
    method = clazz.method("menu_after") rescue nil
    return txt if method.nil?

    code = "#{camel}.menu_after #{txt.inspect}, *#{args.inspect}"
    returned, out, exception = Code.eval code

    return CodeTree.draw_exception exception, code if exception
    if returned

      # TODO: call .unset_env_vars before this and other below places we return

      returned = returned.unindent if returned =~ /\A[ \n]/
      return returned
    end

    txt   # Otherwise, just return output!"

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

    return if path =~ /^(h|log|last)\//

    path = "- #{path}"
    File.open(@@log, "a") { |f| f << "#{path}\n" } rescue nil
  end

  #
  # Insert menu right here and launch it
  #
  # Launcher.open "computer"
  #
  def self.insert txt, options={}
    View.insert txt
    $el.open_line(1)
    Launcher.launch options
  end

  def self.show menu, options={}
    self.open menu, options.merge(:no_launch=>1)
  end

  #
  # Open new buffer and launch the menu in it
  #
  # Launcher.open "computer"
  #
  def self.open menu, options={}

    $el.sit_for 0.25 if options[:delay] || options[:first_letter]   # Delay slightly, (avoid flicking screen when they type command quickly)

    View.to_after_bar if View.in_bar? && !options[:bar_is_fine]

    dir = View.dir

    # For buffer name, handle multi-line strings
    buffer = menu.sub(/.+\n[ -]*/m, '').gsub(/[.,]/, '')
    buffer = "@" + buffer.sub(/^[+-] /, '')
    View.to_buffer buffer, :dir=>dir

    View.clear
    Notes.mode
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

    if options[:no_launch]
      View.to_highest
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

  def self.wrapper path

    # If starts with bookmark, expand as file (not dir)

    path = Bookmarks.expand path, :file_ok=>1

    # TODO: make generic
    # TODO: load all the adapters and construct the "rb|js" part of the regex
    match = path.match(/(.+\/)(\w+)\.(rb|js|coffee|py|notes|menu|haml)\/(.*)/)
    if match
      dir, file, extension, path = match[1..4]
      # TODO: instead, call Launcher.invoke JsAdapter(dir, path), path
      self.send "wrapper_#{extension}", dir, "#{file}.#{extension}", path
      return true   # Indicate we handled it
    end

    # For matches to filename instead of extensions?
    match = path.match(/(.+\/)(Rakefile)\/(.*)/)
    if match
      dir, file, path = match[1..4]
      # TODO: instead, call Launcher.invoke JsAdapter(dir, path), path
      self.send "wrapper_#{file.downcase}", dir, file, path
      return true   # Indicate we handled it
    end

    return false

  end

  def self.wrapper_rb dir, file, path
    output = Console.run "ruby #{Xiki.dir}/etc/wrappers/wrapper.rb #{file} \"#{path}\"", :sync=>1, :dir=>dir

    # Sensible thing for now is to just do literal output
    #     output = Tree.children output, path if path !~ /^\./

    # How to know when to do children?!
      # Because it called .menu, and menu had no args
        # Make it set env var?

    #     output = Tree.children output, path

    Tree << output
  end

  def self.wrapper_js dir, file, path
    output = Console.run "node #{Xiki.dir}etc/wrappers/wrapper.js \"#{dir}#{file}\" \"#{path}\"", :sync=>1, :dir=>dir
    output = Tree.children output, path
    Tree << output
  end

  def self.wrapper_coffee dir, file, path
    txt = CoffeeScript.to_js("#{dir}#{file}")
    tmp_file = "/tmp/tmp.js"
    File.open(tmp_file, "w") { |f| f << txt }

    output = Console.run "node #{Xiki.dir}etc/wrappers/wrapper.js \"#{tmp_file}\" \"#{path}\"", :sync=>1, :dir=>dir
    output = Tree.children output, path
    Tree << output
  end

  def self.wrapper_notes dir, file, path
    if match = path.match(/^(\| .+)(\| .*)/)
      heading, content = match[1..2]
      # [nil, nil])[1..2]
    else
      heading, content = [path, nil]
    end

    heading = nil if heading.blank?

    #     heading, content = (path.match(/^(\| .+)(\| .*)/) || [nil, nil])[1..2]

    dir = "#{dir}/" if dir !~ /\/$/
    output = Notes.drill "#{dir}#{file}", heading, content
    Tree << output
  end

  def self.wrapper_menu dir, file, path
    heading, content = (path.match(/^(\| .+)(\| .*)?/) || [nil, nil])[1..2]

    #     output = Menu.drill "#{dir}/#{file}", heading, content

    #     output = Tree.children File.read(file), Tree.rootless(path)
    output = Tree.children File.read("#{Bookmarks[dir]}/#{file}"), path

    Tree << output
  end

  def self.wrapper_py dir, file, path
    output = Console.run "python #{Xiki.dir}etc/wrappers/wrapper.py \"#{dir}#{file}\" \"#{path}\"", :sync=>1, :dir=>dir
    output = Tree.children output, path if path !~ /^\./
    Tree << output
  end

  def self.wrapper_haml dir, file, path

    engine = Haml::Engine.new(File.read "#{dir}#{file}")

    foos = ["foo1", "foo2", "foo3"]
    o = Object.new
    o.instance_eval do
      @foo = "Foooo"
      @foos = foos
    end

    txt = engine.render(o, "foo"=>"Fooooooo", "foos"=>foos)

    Tree << Tree.quote(txt)
  end

  def self.wrapper_rakefile dir, file, path

    # If just file passed, show all tasks

    if path.blank?
      txt = Console.sync "rake -T", :dir=>dir

      txt = txt.scan(/^rake (.+?) *#/).flatten

      Tree << txt.map{|o| "- #{o}/\n"}.join
      return
    end

    # Task name passed, so run it

    path.sub! /\/$/, ''
    Console.run "rake #{path}", :dir=>dir
    nil

  end

  def self.reload_menu_dirs

    MENU_DIRS.each do |dir|
      next unless File.directory? dir

      Files.in_dir(dir).each do |f|
        next if f !~ /^[a-z].*\..*[a-z]$/ || f =~ /__/
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

    menu = Keys.input :timed=>true, :prompt=>"Enter menu to pass '#{txt}' to (space if menu): "

    return Launcher.open txt if menu == " "

    matches = Launcher.menu_keys.select do |possibility|
      possibility =~ /^#{menu}/
    end

    if matches.length == 1
      return Launcher.open("- #{matches[0]}/#{txt}")
    end

    Launcher.open(matches.map{|o| "- #{o}/#{txt}\n"}.join(''), :choices=>1)
    Tree.search
  end

  def self.as_update
    Keys.prefix = "update"
    Launcher.launch :leave_bullet=>1
  end

  def self.as_delete
    Keys.prefix = "delete"
    Launcher.launch
  end

  def self.as_open
    Keys.prefix = "open"
    Launcher.launch
  end

  def self.enter_all
    return FileTree.enter_lines(/.*/) if Line.blank?

    Keys.prefix = "all"
    Launcher.launch
  end

  def self.enter_outline
    return FileTree.enter_lines if Line.blank?   # Prompts for bookmark

    # If there's a numeric prefix, add it
    Keys.add_prefix "outline"
    Launcher.launch
  end

  def self.set_env_vars path
    ENV['prefix'] = Keys.prefix.to_s

    args = path.is_a?(Array) ?
      path : Menu.split(path, :rootless=>1)

    # ?? If any has ^|, then make sure current line has slash

    quoted = args.find{|o| o =~ /^\|( |$)/}

    if ! quoted
      return ENV['txt'] = args[-1]
    end

    # Quoted lines

    txt = Tree.leaf("|")   # Cheat to make it grab quoted
    ENV['txt'] = txt.length > 1_000_000 ? "*too long to put into env var*" : txt
  end

  def self.web_menu line
    Line << "/" unless Line =~ /\/$/
    url = "http://#{line}"
    url.sub! /\.\w+/, "\\0/xiki"
    url.gsub! ' ', '+'

    begin
      response = HTTParty.get(url)
      Tree << response.body
    rescue Exception=>e
      Tree << "- couldn't connect!"
    end
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

  # As .menu...

  if file =~ /\.menu$/ || options[:force_as] == "menu"
    Launcher.add stem, :menu_file=>1 do |path|
      next View.flash("- Xiki couldn't find: #{file}", :times=>5) if ! File.exists?(file)
      Tree.children File.read(file), Tree.rootless(path)
    end
    return
  end

  # As class, so require and add launcher...

  result = :not_found
  begin
    result = Menu.load_if_changed file
  rescue LoadError => e
    gem_name = Requirer.extract_gem_from_exception e.to_s
    Requirer.show "The file #{file} wants to use the '#{gem_name}' gem.\n% sudo gem install #{gem_name}\n\n"
  rescue Exception=>e
    txt = CodeTree.draw_exception e
    Requirer.show "The file #{file} had this exception:\n#{txt}\n\n"
  end

  return if result == :not_found

  Launcher.add stem if ! Launcher.menus[1][stem]
end

Launcher.init_default_launchers
