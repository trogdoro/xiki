require 'line'
require 'effects'
require 'ruby_console'

class LineLauncher
  extend ElMixin

  CLEAR_CONSOLES = [
    "*output - tail of /tmp/output_ol.notes",
    "*output - tail of /tmp/ds_ol.notes",
    "*visits - tail of /tmp/visit_log.notes",
    "*console app",
    ]

  # Set this to true to just see which launcher applied.
  # Look in /tmp/output.notes
  @@just_show = false
  # @@just_show = true

  @@launchers = []
  @@launchers_regexes = {}   # Only tracks whether we've added it yet
  @@launchers_procs = []
  @@paren_launchers = {}
  @@label_launchers = {}

  def self.launchers; @@launchers; end

  def self.add_paren match, block
    @@paren_launchers[match] = block
  end
  def self.add_label match, block
    @@label_launchers[match] = block
  end

  def self.add arg, &block
    # If regex, add
    if arg.class == Regexp
      self.add_regex arg, block

    # If hash, must be paren
    elsif arg.class == Hash
      if arg[:paren]
        self.add_paren(arg[:paren], block)
      elsif arg[:label]
        self.add_label(arg[:label], block)
      end
    # If proc, add to procs
    elsif arg.class == Proc
      @@launchers_procs << [arg, block]
    end
  end

  def self.add_regex regex, block
    if @@launchers_regexes[regex]  # If already there, add at at begining
      @@launchers.unshift [regex, block]
    else  # If not there yet, add
      @@launchers << [regex, block]
      @@launchers_regexes[regex] = true
    end
  end

  def self.launch_or_hide options={}
    # If no prefixes and children exist, delete under
    if ! Keys.prefix and ! Line.blank? and CodeTree.children? and ! Line.matches(/^ *\|/)
      FileTree.kill_under
      return
    end

    # Else, launch
    self.launch options
  end

  # Call the appropriate launcher if we find one, passing it line
  def self.launch options={}

    Effects.blink(:what=>:line) if options[:blink]
    line = options[:line] || Line.value   # Get paren from line
    label = Line.label(line)
    paren = label[/\((.+)\)/, 1] if label

    # Special hooks for specific files and modes
    return if self.file_and_mode_hooks

    View.bar if Keys.prefix == 7

    $xiki_no_search = options[:no_search]   # If :no_search, disable search

    if line =~ /^( *)[+-] .+?: (.+)/   # Split label off, if there
      line = $1 + $2
    end

    if paren && @@paren_launchers[paren]   # If try each potential paren match
      if @@just_show
        Ol << paren
      else
        @@paren_launchers[paren].call
      end
      return $xiki_no_search = false
    end

    @@launchers.each do |launcher|   # Try each potential regex match
      regex, block = launcher
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

    @@label_launchers.each do |launcher|   # Try each potential label match
      regex, block = launcher
      # If we found a match, launch it
      if label =~ regex
        # Run it
        if @@just_show
          Ol << regex.to_s
        else
          block.call label
        end
        $xiki_no_search = false
        return true
      end
    end

    if self.launch_by_proc   # Try procs (currently all trees)
      return $xiki_no_search = false
    end

    if @@just_show
      Ol << "Nothing matched.  Just passing to FileTree."
      return $xiki_no_search = false
    end

    # If nothing found so far, don't do anything if...
    if line =~ /^\|/
      View.beep
      return View.message "Don't know what to do with this line"
    end

    FileTree.launch   # Default to FileTree
    $xiki_no_search = false
  end

  def self.launch_by_proc

    list = FileTree.construct_path(:list=>true)   # Get path to pass to procs, to help them decide

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
    @@launchers = []
    @@launchers_procs = []

    self.add /^  +[+-]?\|/ do |line|  # | FileTree quoted text
      self.launch_by_proc
    end

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
    self.add :paren=>"jsc" do   # - (js): js to run in firefox
      Firefox.run("console.log(#{CodeTree.line_or_children.gsub('\\', '\\\\\\')})")
    end

    self.add :paren=>"jso" do   # - (js): js to run in firefox
      FileTree.insert_under Firefox.value(CodeTree.line_or_children)
    end

    self.add :paren=>"dom" do   # Run in browser
      js = %`$.trim($("#{Line.content}").html()).replace(/^/gm, '| ');`
      html = Firefox.run js
      if html =~ /\$ is not defined/
        Firefox.load_jquery
        next View.insert_under "- Jquery loaded, try again!"
      end
      html = html.sub(/\A"/, '').sub(/"\z/, '')
      View.insert_under "#{html.strip}\n", :escape=>''
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
      FileTree.indent(out)
      FileTree.insert_quoted_and_search out  # Insert under
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

    LineLauncher.add :paren=>"rails" do  # - (gl): Run in rails console
      out = RubyConsole[:rails].run(Line.without_label)
      FileTree.indent(out)
      FileTree.insert_quoted_and_search out  # Insert under
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

    self.add /^ *[$\/].+\$ / do   # $ run command in shell
      Console.launch_dollar
    end

    self.add /^ *\$ / do   # $ run command in shell
      Console.launch_dollar
    end

    self.add(/^ *[+-]? *(http|file).?:\/\/.+/) do   # url
      line = Line.content

      Files.append "~/.emacs.d/url_log.notes", line

      prefix = Keys.prefix
      Keys.clear_prefix

      url = line[/(http|file).?:\/\/.+/]

      if prefix == 8
        FileTree.insert_under RestTree.request("GET", url)
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

    self.add(/^ *$/) do |line|  # Empty line: insert CodeTree menu
      View.beep
      View.message "There was nothing on this line to launch."
    end

    self.add(/^ *\*/) do |line|  # *... buffer
      #return $el.insert "hey"
      name = Line.without_label.sub(/\*/, '')
      View.to_after_bar
      View.to_buffer name
    end

    self.add(/^ *[$\/][^:\n]+!/) do |l|   # /dir!shell command inline
      Console.launch :sync=>true
    end
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

    # Let trees try to handle it
    # RestTree
    condition_proc = proc {|list| RestTree.handles? list}
    LineLauncher.add condition_proc do |list|
      RestTree.launch :path=>list
    end

    # FileTree
    condition_proc = proc {|list| FileTree.handles? list}
    LineLauncher.add condition_proc do |list|
      FileTree.launch :path=>list
    end

    # CodeTree
    condition_proc = proc {|list| CodeTree.handles? list}
    LineLauncher.add condition_proc do |list|
      CodeTree.launch :path=>list
    end

    # UrlTree
    condition_proc = proc {|list| UrlTree.handles? list}
    LineLauncher.add condition_proc do |list|
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
      FileTree.to_parent
    end
    FileTree.kill_under

    LineLauncher.launch_or_hide :blink=>true, :no_search=>true
    View.to_nth orig
  end

  def self.urls
    txt = File.read File.expand_path("~/.emacs.d/url_log.notes")
    txt = txt.split("\n").reverse.uniq.join("\n")
  end

end
Launcher = LineLauncher   # Temporary alias until we rename it

LineLauncher.init_default_launchers
