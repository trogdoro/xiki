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
  # @@just_show = true

  @@launchers = ActiveSupport::OrderedHash.new
  @@launchers_procs = []
  @@launchers_parens = {}
  @@launchers_paths = {}

  def self.log
    @@log
  end

  def self.add arg, &block
    if arg.is_a? Regexp   # If regex, add
      @@launchers[arg] = block

    elsif arg.is_a? Hash   # If hash, must be paren
      (@@launchers_parens)[arg[:paren]] = block
    elsif arg.is_a? Proc   # If proc, add to procs
      @@launchers_procs << [arg, block]
    elsif arg.is_a? String
      @@launchers_paths[arg] = block
    else
      raise "Don't know how to launch this"
    end
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

    if Line.value =~ /^ / && ! options[:line] && !is_root
      Tree.plus_to_minus

      # merge together (spaces if no slashes) and pass that to launch
      list = Tree.construct_path(:list=>true)   # Get path to pass to procs, to help them decide
      found = list.index{|o| o =~ /^@/} and list = list[found..-1]   # Remove before @... node if any

      merged = list.map{|o| o.sub /\/$/, ''}.join('/')
      merged << "/" if list[-1] =~ /\/$/
      # It's adding the slash, which is good
        # But add it on end if there was one on end!

      return self.launch :line=>merged
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

    # Grab thing to match
    root = line[/^\w+/]
    if block = @@launchers_paths[root]

      self.append_log line

      output = block_call_safely block, line
      if output
        if output !~ /\A *\|/
          Line << "/" unless Line =~ /\/$/
        end
        output = output.unindent if output =~ /\A[ \n]/
        View >> output
      end
      return
    end

    # Try to auto-complete based on path-launchers

    if line =~ /^(\w+)(\.\.\.)?$/
      stem = $1
      matches = @@launchers_paths.keys.select do |possibility|
        #       matches = (@@launchers_paths.keys + (@@launchers_classes||[])).select do |possibility|
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

    View.beep
    Message << "No launcher matched!"

    $xiki_no_search = false
  end

  def self.block_call_safely block, line
    begin
      block.call line
    rescue Exception=>e
      backtrace = e.backtrace[0..8].join("\n").gsub(/^/, '  ') + "\n"
      "- error: #{e.message}\n- backtrace:\n#{backtrace}"
    end
  end

  def self.launch_by_proc
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
    @@launchers = ActiveSupport::OrderedHash.new
    @@launchers_procs = []

    self.add :paren=>"o" do  # - (t): Insert "Test"
      orig = Location.new
      txt = Line.without_label  # Grab line
      View.to_after_bar  # Insert after bar
      insert txt
      command_execute "\C-m"
      orig.go
    end

    self.add :paren=>"th" do   # - (th): thesaurus.com
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      browse_url "http://thesaurus.reference.com/browse/#{url}"
    end

    self.add :paren=>"twitter" do   # - (twitter): twitter search
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      browse_url "http://search.twitter.com/search?q=#{url}"
    end

    self.add :paren=>"dic" do   # - (dic): dictionary.com lookup
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      browse_url "http://dictionary.reference.com/browse/#{url}"
    end

    self.add :paren=>"click" do
      txt = CodeTree.line_or_children

      # If starts with number like "2:edit", extract it
      nth = txt.slice! /^\d+:/
      nth = nth ? (nth[/\d+/].to_i - 1) : 0

      Firefox.run("$('a, *[onclick]').filter(':contains(#{txt}):eq(#{nth})').click()")
    end

    self.add :paren=>"blink" do
      txt = CodeTree.line_or_children

      # If starts with number like "2:edit", extract it
      nth = txt.slice! /^\d+:/
      nth = nth ? (nth[/\d+/].to_i - 1) : 0

      Firefox.run("$('a, *[onclick]').filter(':contains(#{txt}):eq(#{nth})').blink()")
    end

    self.add :paren=>"click last" do   # - (js): js to run in firefox
      Firefox.run("$('a:contains(#{CodeTree.line_or_children}):last').click()")
    end

    self.add :paren=>"js" do   # - (js): js to run in firefox
      Firefox.run(CodeTree.line_or_children.gsub('\\', '\\\\\\'))
    end
    self.add :paren=>"jsp" do   # - (js): js to run in firefox
      txt = CodeTree.line_or_children.gsub('\\', '\\\\\\')
      txt = txt.strip.sub(/;$/, '')   # Remove any semicolon at end
      Firefox.run("p(#{txt})")
    end
    self.add "jsc" do |line|   # - (js): js to run in firefox
      Firefox.run("console.log(#{line[/\/(.+)/, 1]})")
      nil
    end

    self.add :paren=>"jso" do   # - (js): js to run in firefox
      Tree.under Firefox.value(CodeTree.line_or_children)
    end

    self.add :paren=>"dom" do   # Run in browser
      js = %`$.trim($("#{Line.content}").html()).replace(/^/gm, '| ');`
      html = Firefox.run js
      if html =~ /\$ is not defined/
        Firefox.load_jquery
        next View.under "- Jquery loaded, try again!"
      end
      html = html.sub(/\A"/, '').sub(/"\z/, '')
      View.under "#{html.strip}\n"
    end

    self.add :paren=>"html" do   # Run in browser
      file = Line.without_label  # Grab line
      if Keys.prefix_u?
        View.open file

      else
        browse_url file
        browse_url "#{View.dir}#{file}"
      end
    end


    self.add :paren=>"rc" do  # - (rc): Run in rails console

      line = Line.without_label

      # Make it go to rext if in bar
      if View.in_bar?
        View.to_after_bar
      end
      # Go to console
      View.to_buffer "*console"
      erase_buffer
      end_of_buffer
      insert "reload!"
      Console.enter
      insert line
      Console.enter

      Move.top
    end

    # - (r): Ruby code
    self.add :paren=>"r" do
      returned, stdout = Code.eval(Line.without_label)
      message returned.to_s
      #insert stdout if stdout
    end

    # - (irb): Merb console
    self.add :paren=>"irb" do
      out = RubyConsole.run(Line.without_label)
      Tree.indent(out)
      Tree.insert_quoted_and_search out  # Insert under
    end

    self.add :paren=>"ro" do  # - (ro): Ruby code in other window
      # Make it go to rext if in bar
      if View.in_bar?
        View.to_after_bar
      end

      returned, stdout = Code.eval(Line.without_label)
      message returned.to_s
      insert stdout
    end

    Launcher.add :paren=>"rails" do  # - (gl): Run in rails console
      out = RubyConsole[:rails].run(Line.without_label)
      Tree.indent(out)
      Tree.insert_quoted_and_search out  # Insert under
    end


    # - (u): Ruby code under
    self.add :paren=>"u" do
      returned, stdout = Code.eval(Line.without_label)
      message returned.to_s

      # Insert under
      indent = Line.indent
      Line.start
      started = point
      Line.next

      # If first line is "- raw:", don't comment
      if stdout =~ /\A- raw:/
        stdout.sub!(/.+?\n/, '')
        stdout.gsub!(/^/, "#{indent}  ")
      else
        stdout.gsub!(/^/, "#{indent}  |")
        # Get rid of lines that are bullets
        #stdout.gsub!(/^(  +)\|( *- .+: )/, '\\1\\2')
      end

      insert stdout
      goto_char started
    end

    self.add :paren=>"line" do
      line, path = Line.without_label.split(', ')

      View.open path
      View.to_line line
    end


    self.add :paren=>'elisp' do |line|   # Run lines like this: - foo (elisp): (bar)
      Line.to_right
      eval_last_sexp nil
    end

    self.add :paren=>'ruby' do |line|   # - (ruby)
      message el4r_ruby_eval(line)
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
        Tree.under RestTree.request("GET", url)
        next
      end
      url.gsub! '%', '%25'
      prefix == :u ? browse_url(url) : Firefox.url(url)
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
      browse_url "http://www.google.com/search?q=#{url}"
    end

    self.add "google" do |line|
      url = line[/\/(.+)/, 1].sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      browse_url "http://www.google.com/search?q=#{url}"
      nil
    end

    self.add(/^ *$/) do |line|  # Empty line: insert CodeTree menu
      View.beep
      View.message "There was nothing on this line to launch."
    end

    self.add(/^\*/) do |line|  # *... buffer
      #return $el.insert "hey"
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

    # Menus

    self.add "db" do |line|
      "
      - @riak/
      - @mysql/
      - @couchdb/
      "
    end

    # Path launchers

    Launcher.add "shopping" do
      "- eggs/\n- bananas/\n- milk/\n"
    end

    Launcher.add "shapes" do |path|
      "- circle/\n- square/\n- triangle/\n"
    end

    Launcher.add "tables" do |path|
      args = path.split('/')[1..-1]
      #       if path =~ /\/fields$/
      #       return Mysql.run('homerun_dev', "desc #{table}"), :escape=>'| '
      #       end
      Mysql.tables(*args)
    end

    Launcher.add "rows" do |path|
      args = path.split('/')[1..-1]
      Mysql.tables(*args)
    end

    Launcher.add "columns" do |path|
      args = path.split('/')[1..-1]
      if args.size > 0
        next Mysql.run('homerun_dev', "desc #{args[0]}").gsub!(/^/, '| ')
      end
      Mysql.tables(*args)
    end

    Launcher.add "technologies" do
      "- TODO: pull out from $te"
    end

    # Trees

    # Let trees try to handle it
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
    CodeTree.insert_menu self.last_launched_menu
  end

  def self.last_launched_menu
    bm = Keys.input(:timed => true, :prompt => "bookmark to show launches for (* for all): ")

    menu =
      if bm == "8" || bm == " "
        "- Search.launched/"
      elsif bm == "."
        "- Search.launched '#{View.file}'/"
      elsif bm == "3"
        "- Search.launched '#'/"
      elsif bm == ";" || bm == ":" || bm == "-"
        "- Search.launched ':'/"
      else
        "- Search.launched '$#{bm}'/"
      end
  end


  def self.do_as_launched
    txt = "- #{View.file_name}\n    | #{Line.value}"
    txt.sub!("| ", "- #{Keys.input(:prompt => "enter label: ")}: | ") if Keys.prefix_u
    Search.append_log "#{View.dir}/", txt
  end

  def self.invoke clazz, path
    camel = TextUtil.camel_case clazz
    clazz = $el.el4r_ruby_eval(camel) rescue nil

    raise "No class '#{clazz}' found in launcher" if clazz.nil?

    args = path.split "/"
    args.shift

    # If no args yet, pass in empty list
    # if clazz.method("menu").arity != 1
    args = args.map{|o| "\"#{o}\""}.join(", ")

    code = "#{camel}.menu #{args}"
    returned, out, exception = Code.eval code
    output = returned
    output = output.unindent if output =~ /\A[ \n]/

    if exception
      backtrace = exception.backtrace[0..8].join("\n").gsub(/^/, '  ') + "\n"
      return "- error: #{exception.message}\n- backtrace:\n#{backtrace}"
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
    #     path = "- path" if path !~ /^[+-] /

    File.open(@@log, "a") { |f| f << "#{path}\n" } rescue nil
  end
end

Launcher.init_default_launchers

Launcher.add "log" do
  log = IO.read(Launcher.log)
  log.split("\n").map{|o| o.sub /^- /, '- @'}.reverse.uniq.join("\n")+"\n"
end

Launcher.add "last" do |path|
  path = path.sub /^last\/?/, ''

  log = IO.read(Launcher.log)
  paths = log.split("\n")

  # If nothing passed, just list all roots

  if path.empty?
    paths.map!{|o| o.sub /\/.+/, '/'}   # Cut off after path
    next paths.reverse.uniq.join("\n")+"\n"
  end

  # If root passed, show all matching
  paths = paths.select{|o| o =~ /- #{path}/}.map{|o| o.sub /^- /, '- @'}

  paths.reverse.uniq.join("\n")+"\n"

  # Be sure to include labels
    # Just labels at root?

end
