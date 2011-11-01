require 'effects'
require 'ruby_console'
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

  @@log = File.expand_path("~/.emacs.d/path_log.notes")

  # Set this to true to just see which launcher applied.
  # Look in /tmp/output.notes
  @@just_show = false
  #   @@just_show = true

  @@launchers ||= ActiveSupport::OrderedHash.new
  @@launchers_procs ||= []
  @@launchers_parens ||= {}
  @@launchers_paths ||= {}

  def self.launchers_paths
    @@launchers_paths
  end

  def self.last path, options={}

    path = path.sub /^last\/?/, ''

    log = IO.read(Launcher.log)
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

  def self.log
    @@log
  end

  def self.add *args, &block
    arg = args.shift

    if arg.is_a? Regexp   # If regex, add
      @@launchers[arg] = block
    elsif arg.is_a? Proc   # If proc, add to procs
      @@launchers_procs << [arg, block]
    elsif arg.is_a?(String)
      self.add_path arg, args[0], block
    elsif arg.is_a?(Hash)
      @@launchers_parens[arg[:paren]] = block
    else
      raise "Don't know how to launch this"
    end
  end

  def self.add_path root, hash, block
    # If just block, just define
    if hash.nil? && block
      return @@launchers_paths[root] = block
    end
    # If just root, define class with that name
    if block.nil? && (hash.nil? || hash[:class])
      clazz = hash ? hash[:class] : root
      clazz.sub!(/(\w+)/) {TextUtil.snake_case $1} if hash
      self.add root do |path|
        Launcher.invoke(clazz, path)
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
          Menu["#{menu}/#{Tree.rootless path}"]
        end
        return
      end

      self.add root do |path|   # If text of the actual menu
        Tree.climb menu, path[%r"\/(.*)"]
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

    Effects.blink(:what=>:line) if options[:blink]
    line = options[:line] || Line.value   # Get paren from line

    label = Line.label(line)
    paren = label[/\((.+)\)/, 1] if label

    # Special hooks for specific files and modes
    return if self.file_and_mode_hooks

    View.bar if Keys.prefix == 7

    $xiki_no_search = options[:no_search]   # If :no_search, disable search

    is_root = false

    if line =~ /^( *)[+-] [^\n\(]+?\) (.+)/   # Split off label, if there
      line = $1 + $2
    end
    # Deprecated:
    if line =~ /^( *)[+-] .+?: (.+)/   # Split off label, if there
      line = $1 + $2
    end
    if line =~ /^( *)[+-] (.+)/   # Split off bullet, if there
      line = $1 + $2
    end
    if line =~ /^ *@(.+)/   # Split off @ and indent if @ exists
      is_root = true
      line = $1
    end

    if paren && @@launchers_parens[paren]   # If try each potential paren match
      if @@just_show
        Ol << paren
      else
        @@launchers_parens[paren].call
      end
      return $xiki_no_search = false
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

    Ol << "Should we be doing this after self.try_launcher_paths line?!"
    if Line.value =~ /^ / && ! options[:line] && !is_root
      Tree.plus_to_minus

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

    Ol << "Try moving this up to under @@launchers.each ?!"
    return if self.try_launcher_paths line

    # Try to auto-complete based on path-launchers

    if line =~ /^(\w+)(\.\.\.)?$/
      stem = $1
      matches = @@launchers_paths.keys.select do |possibility|
        possibility =~ /^#{stem}/
      end
      if matches.any?
        if matches.length == 1
          Line.sub! /^([ +-]*).*/, "\\1#{matches[0]}"
          Launcher.launch
          return
        end
        Line.sub! /\b$/, "..."

        View >> matches.map{|o| "#{o}"}.join("\n")
        return
      end
    end

    if line =~ /^\w+\.\.\.\/(\w+)$/
      Tree.to_parent
      Tree.kill_under
      Line.sub! /([ @+-]*).+/, "\\1#{$1}"
      Launcher.launch
      return
    end

    if line =~ /^([\w-]+)\/?$/   # If blank line, see if launcher not loaded
      root = $1
      [".menu", ".rb"].each do |extension|
        file = File.expand_path "~/menus/#{root}#{extension}"
        break require_menu(file) if File.exists? file
      end

      return if self.try_launcher_paths line
    end
    View.beep
    Message << "No launcher matched!"

    $xiki_no_search = false
  end

  def self.try_launcher_paths line
    root = line[/^\w+/]   # Grab thing to match
    if block = @@launchers_paths[root]
      self.append_log line
      self.output_and_search block, line
      return true
    end
    false
  end

  def self.output_and_search block_or_string, line=nil

    buffer_orig = View.buffer
    orig = Location.new
    orig_left = View.cursor
    error_happened = nil

    output =
      if block_or_string.is_a? String
        block_or_string
      else   # Must be a proc
        begin
          block_or_string.call line
        rescue Exception=>e
          error_happened = true
          backtrace = e.backtrace[0..8].join("\n").gsub(/^/, '  ') + "\n"
          "- error evaluating:\n#{Code.to_ruby(block_or_string).gsub(/^/, '  ')}\n- message: #{e.message}\n" +
            "- backtrace:\n" + e.backtrace[0..8].map{|i| "  #{i}\n"}.join('') + "  ...\n"
        end
      end

    return if output.blank?

    buffer_changed = buffer_orig != View.buffer   # Remember whether we left the buffer

    ended_up = Location.new
    orig.go   # Go back to where we were before running code

    Line << "/" if Line !~ /(^ *\||\/$)/ and output !~ /\A *\|/   # Add slash at end if line not a quote and there was non-quote output

    indent = Line.indent
    Line.to_left
    Line.next
    left = View.cursor

    # Move what they printed over to left margin initally, in case they haven't
    output = TextUtil.unindent(output) if output =~ /\A[ \n]/
    # Remove any double linebreaks at end
    output = CodeTree.returned_to_s output
    output.sub!(/\n\n\z/, "\n")
    output = "#{output}\n" if output !~ /\n\z/

    output.gsub!(/^/, "#{indent}  ")

    View << output  # Insert output
    right = View.cursor

    orig.go   # Move cursor back
    ended_up.go   # End up where script took us

    if !error_happened && !$xiki_no_search && !buffer_changed && View.cursor == orig_left
      Tree.search_appropriately left, right, output#, original_indent
    else
      Move.to_line_text_beginning(1)
    end

    #       $xiki_no_search = false   # Is this handled elsewhere?
  end

  def self.launch_by_proc list=nil
    # Should be able to remove
    list = Tree.construct_path(:list=>true)   # Get path to pass to procs, to help them decide

    # Try each proc
    @@launchers_procs.each do |launcher|   # For each potential match
      condition_proc, block = launcher
      if condition_proc.call list   # If we found a match, launch it
        if @@just_show   # Run it
          Ol << condition_proc.to_ruby
        else
          block.call list
        end
        return true
      end
    end
    return false
  end

  def self.init_default_launchers
    self.add :paren=>"o" do  # - (t): Insert "Test"
      orig = Location.new
      txt = Line.without_label  # Grab line
      View.to_after_bar  # Insert after bar
      View.insert txt
      command_execute "\C-m"
      orig.go
    end

    self.add :paren=>"th" do   # - (th): thesaurus.com
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      $el.browse_url "http://thesaurus.reference.com/browse/#{url}"
    end

    self.add :paren=>"twitter" do   # - (twitter): twitter search
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      $el.browse_url "http://search.twitter.com/search?q=#{url}"
    end

    self.add :paren=>"dic" do   # - (dic): dictionary.com lookup
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      $el.browse_url "http://dictionary.reference.com/browse/#{url}"
    end

    self.add :paren=>"wp" do |line|
      url = "http://en.wikipedia.org/wiki/#{Line.without_label}"
      Keys.prefix_u ? $el.browse_url(url) : Firefox.url(url)
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

    self.add :label=>/^google$/ do |line|  # - google:
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      $el.browse_url "http://www.google.com/search?q=#{url}"
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

    #     self.add(/^ *[$\/][^:\n]+!/) do |l|   # /dir!shell command inline
    #       Console.launch :sync=>true
    #     end
    self.add(/^ *!/) do |l|   # !shell command inline
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
    self.add(/^[a-z-]{2,}\.[a-z-]{2,4}(\/|$)/) do |line|  # **.../: Tree grep in dir
      #       line.sub(/\/$/, '')
      Line << "/" unless Line =~ /\/$/
      url = "http://#{line}"
      url.sub! /\.\w+/, "\\0/xiki"
      url.gsub! ' ', '+'
      response = HTTParty.get(url)
      View.under response.body
    end

    # Path launchers

    Launcher.add "technologies" do
      "- TODO: pull out from $te"
    end

    Launcher.add "log" do
      log = IO.read(Launcher.log)
      log.split("\n").map{|o| o.sub /^- #{Notes::LABEL_REGEX}/, '\\0@'}.reverse.uniq.join("\n")+"\n"
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
      FileTree.launch :path=>list
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

    if Keys.prefix_u :clear=>true
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
      camel = TextUtil.camel_case clazz
      clazz = $el.el4r_ruby_eval(camel) rescue nil
    elsif clazz.is_a? Class
      camel = clazz.to_s
    end

    raise "No class '#{clazz}' found in launcher" if clazz.nil?

    args = path.split "/"
    args.shift

    method = clazz.method default_method rescue raise "#{clazz} doesn't seem to have a .#{default_method} method!"

    if method.arity == 0
      code = "#{camel}.#{default_method}"
      output, out, exception = Code.eval code
      output = CodeTree.returned_to_s(output)   # Convert from array into string, etc.
      output = output.unindent if output =~ /\A[ \n]/
      raise "#{code} returned nil, but is supposed to return something when i takes no arguments" if output.nil?

      output = Tree.routify! output, args

      if output
        if output == "- */\n"
        else
          return Tree << output
        end
      end
      # Else, continue on to run it based on routified path
    end
    # Figure out which ones are actions

    # Find last .foo item
    actions, variables = args.partition{|o|
      o =~ /^\./
      # Also use result of .menu to determine
    }
    action = actions.last || ".#{default_method}"

    # TODO: Do we want to always group quotes, or should menus optionally internally call Tree.leaf?!"

    if args[-1] =~ /^ *\|/
      args[-1].replace(Tree.leaf args[-1])
    end

    args = variables.map{|o| "\"#{o.gsub('"', '\\"')}\""}.join(", ")

    code = "#{camel}#{action} #{args}".strip
    output, out, exception = Code.eval code
    output = CodeTree.returned_to_s(output)   # Convert from array into string, etc.
    output = output.unindent if output =~ /\A[ \n]/

    if exception
      backtrace = exception.backtrace[0..8].join("\n").gsub(/^/, '  ') + "\n"
      return "- tried to run: #{code}\n- error: #{exception.message}\n- backtrace:\n#{backtrace}"
    end

    output
  end

  def self.add_class_launchers classes
    classes.each do |clazz|
      next if clazz =~ /\//

      # Why is this line causing an error??
      #       clazz = $el.el4r_ruby_eval(TextUtil.camel_case clazz) rescue nil
      #       method = clazz.method(:menu) rescue nil
      #       next if method.nil?

      Launcher.add clazz do |path|
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

    View.insert "#{menu}"
    $el.open_line 1
    Launcher.launch options
  end

  def self.method_missing *args, &block
    arg = args.shift
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

  def self.remove root
    @@launchers_paths.delete root
  end

  def self.reload_menu_dirs

    dirs = [
      "#{Xiki.dir}/menus",
      File.expand_path("~/xiki/menus"),
      File.expand_path("~/menus")
    ]

    dirs.each do |dir|
      next unless File.directory? dir

      Files.in_dir(dir).each do |f|
        path = "#{dir}/#{f}"
        stem = f[/[^.]*/]
        self.add stem, :menu=>path
      end
    end
    "- reloaded!"
  end

end

def require_menu file
  stem = file[/(\w+)\./, 1]

  # As .menu

  if file =~ /\.menu$/
    Launcher.add stem do |path|
      Tree.climb File.read(file), path[%r"\/(.*)"]
    end
    return
  end

  # As class, so require and add launcher

  require file
  Launcher.add stem
end

Launcher.init_default_launchers
