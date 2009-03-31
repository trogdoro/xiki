require 'line'
require 'effects'
require 'ruby_console'

class LineLauncher
  extend ElMixin

  # Set this to true to just see which launcher applied.
  # Look in /tmp/output.notes
  @@just_show = false
  #@@just_show = true

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

    if line =~ /^( *)[+-] .+?: (.+)/   # Split label off, if there
      line = $1 + $2
    end
    # If try each potential paren match
    if paren && @@paren_launchers[paren]
      if @@just_show
        Ol << paren
      else
        @@paren_launchers[paren].call
      end
      return
    end

    # Try each potential regex match
    @@launchers.each do |launcher|
      regex, block = launcher
      # If we found a match, launch it
      if line =~ regex
        # Run it
        if @@just_show
          Ol << regex.to_s
        else
          block.call line
        end
        return true
      end
    end

    # Try each potential label match
    @@label_launchers.each do |launcher|
      regex, block = launcher
      # If we found a match, launch it
      if label =~ regex
        # Run it
        if @@just_show
          Ol << regex.to_s
        else
          block.call label
        end
        return true
      end
    end

    return if self.launch_by_proc   # Try procs (currently all trees)

    if @@just_show
      Ol << "Nothing matched.  Just passing to FileTree."
      return
    end

    FileTree.launch   # Default to FileTree
  end

  def self.launch_by_proc

    # Get path to pass to procs, to help them decide
    list = FileTree.construct_path(:list => true)

    # Try each proc
    @@launchers_procs.each do |launcher|   # For each potential match
      condition_proc, block = launcher
      # If we found a match, launch it
      if condition_proc.call list
        # Run it
        if @@just_show
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

    self.add :paren=>"dic" do   # - (th): thesaurus.com
      url = Line.without_label.sub(/^\s+/, '').gsub('"', '%22').gsub(':', '%3A').gsub(' ', '%20')
      browse_url "http://dictionary.reference.com/browse/#{url}"
    end

    self.add :paren=>"click" do   # - (js): js to run in firefox
      Firefox.run("$('a:contains(#{CodeTree.line_or_children}):first').click()")
    end

    self.add :paren=>"click last" do   # - (js): js to run in firefox
      Firefox.run("$('a:contains(#{CodeTree.line_or_children}):last').click()")
    end

    self.add :paren=>"js" do   # - (js): js to run in firefox
      Firefox.run(CodeTree.line_or_children)
    end

    self.add :paren=>"jso" do   # - (js): js to run in firefox
      FileTree.insert_under Firefox.run(CodeTree.line_or_children)
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
      #insert el4r_ruby_eval(line).to_s
    end

    self.add /^ *[$\/].+\$ / do   # $ run command in shell
      Console.launch_dollar
    end
    self.add /^ *\$ / do   # $ run command in shell
      Console.launch_dollar
    end

    self.add /\(other\)/ do |line|   # - (other)
      other_window 1
      insert line
      command_execute "\C-m"
      other_window -1
    end


    self.add(/^ *-? *(http|file).?:\/\/.+/) do |line|   # url
      browse_url line[/(http|file).?:\/\/.+/]
    end

    self.add(/^ *\$[^#*!]+[^\/]$/) do |line|   # Bookmark
      View.open(line)
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

    self.add(/^ *$/) do |line|  # Empty line: open dir
      CodeTree.insert_menu "- CodeTree.menu/"
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
      line =~ /([\/.\w\-]+):(\d+)/
      path, line = $1, $2
      # If it doesn't have path, add it
      #       path = "#{View.dir}#{path}" if path !~ /^\//
      View.open path
      goto_line line.to_i
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
    if View.name =~ /#{Ol.file_path}$/   # If in output log
      Code.ol_launch
      Effects.blink(:what=>:line)
      return true
    end
    return false
  end

  def self.do_last_launch
    orig = View.index
    Move.to_window(1)
    LineLauncher.launch_or_hide(:blink => (true))
    View.to_nth orig
  end

end
LineLauncher.init_default_launchers
