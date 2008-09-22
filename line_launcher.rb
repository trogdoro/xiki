require 'line'
require 'effects'
require 'ruby_console'

class LineLauncher
  extend ElMixin

  @@just_show = false
  #@@just_show = true

  @@launchers = []
  @@launchers_regexes = {}
  def self.launchers; @@launchers; end
  @@paren_launches = {}

  def self.add_paren match, &block
    @@paren_launches[match] = block
  end

  def self.add regex, label_regex=nil, &block
    if @@launchers_regexes[regex]  # If already there, add at end
      @@launchers.unshift [regex, label_regex, block]
    else  # If not there yet, add
      @@launchers << [regex, label_regex, block]
      @@launchers_regexes[regex] = true
    end
  end

  # Call the appropriate launcher if we find one, passing it line
  def self.launch options={}
    line = options[:line] || Line.value   # Get paren from line
    label = Line.label(line)
    paren = label[/\((.+)\)/, 1] if label

    # Special hooks for specific files
    return if self.file_hooks

    View.bar if Keys.prefix == 9   # Effects.blink

    if line =~ /^( *)- .+?: (.+)/   # Split label off, if there
      line = $1 + $2
    end

    # If this line has parens, run launcher if one exists
    if paren && @@paren_launches[paren]
      @@paren_launches[paren].call
      return
    end

    @@launchers.each do |launcher|   # For each potential match
      regex, label_regex, block = launcher
      # If we found a match, launch it
      if (!regex || line =~ regex) && (!label_regex || (label && label =~ label_regex))
        # Run it
        if @@just_show
          return Ol << regex.to_s + label_regex.to_s
        else
          block.call line
        end
        return true
      end
    end
    self.launch_code_or_ls options
  end

  def self.launch_code_or_ls options={}
    # It must be code_tree node or tree_ls node
    # Still don't know which, so check root
    is_tree_ls_path = TreeLs.is_tree_ls_path
    if @@just_show   # Means we're temporarily disabled for debugging
      return Ol << "is_tree_ls_path: #{is_tree_ls_path}"
    end

    # If code tree, chop it off and run it
    if is_tree_ls_path   # Delegate to Tree
      TreeLs.launch
    else   # && line =~ /^ *- /  # Must have bullet to be code_tree
      CodeTree.launch options
    end
  end

  def self.init_default_launchers
    @@launchers = []

    self.add /^  +[+-]?\|/ do |line|  # | TreeLs quoted text
      self.launch_code_or_ls
    end

    self.add_paren("o") do  # - (t): Insert "Test"
      txt = Line.without_label  # Grab line
      View.to_after_bar  # Insert after bar
      insert txt
      command_execute "\C-m"
    end

    self.add_paren("js") do   # - (js): js to run in firefox
      txt = Line.without_label  # Grab line
      Firefox.eval(txt)
    end

    self.add_paren("html") do   # Run in browser
      file = Line.without_label  # Grab line
      if Keys.prefix_u?
        View.open file

      else
        browse_url file
        browse_url "#{View.dir}#{file}"
      end
    end


    self.add_paren("rc") do  # - (rc): Run in rails console

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
      Shell.enter
      insert line
      Shell.enter

      Move.top
    end

    # - (r): Ruby code
    self.add_paren("r") do
      returned, stdout = Code.eval(Line.without_label)
      message returned.to_s
      #insert stdout if stdout
    end

    # - (irb): Merb console
    self.add_paren("irb") do
      out = RubyConsole.run(Line.without_label)
      TreeLs.indent(out)
      TreeLs.insert_quoted_and_search out  # Insert under
    end

    self.add_paren("ro") do  # - (ro): Ruby code in other window
      # Make it go to rext if in bar
      if View.in_bar?
        View.to_after_bar
      end

      returned, stdout = Code.eval(Line.without_label)
      message returned.to_s
      insert stdout
    end

    LineLauncher.add_paren("rails") do  # - (gl): Run in rails console
      out = RubyConsole[:rails].run(Line.without_label)
      TreeLs.indent(out)
      TreeLs.insert_quoted_and_search out  # Insert under
    end


    # - (u): Ruby code under
    self.add_paren("u") do
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

    self.add_paren("line") do
      line, path = Line.without_label.split(', ')

      View.open path
      View.to_line line
    end



    self.add nil, /\(elisp\)/ do |line|  # Run lines like this: - foo (elisp): (bar)
      end_of_line
      eval_last_sexp nil
    end

    self.add nil, /\(ruby\)/ do |line|  # - (ruby)
      message el4r_ruby_eval(line)
      #insert el4r_ruby_eval(line).to_s
    end

    self.add /\(other\)/ do |line|  # - (other)
      other_window 1
      insert line
      command_execute "\C-m"
      other_window -1
    end


    self.add(/(http|file).?:\/\/.+/) do |line| # url
      browse_url line[/(http|file).?:\/\/.+/]
    end

    self.add(/^ *p /) do |line|  # url
      CodeTree.run line
    end

    self.add(/^ *puts /) do |line|  # url
      CodeTree.run line
    end

    self.add(/^[^\|@]+[\/\w\-]+\.\w+:\d+/) do |line|  # Stack traces, etc

      # Match again (necessary)
      line =~ /([\/.\w\-]+):(\d+)/
      path, line = $1, $2

      # If it doesn't have path, add it
      path = "#{View.dir}#{path}" if path !~ /^\//
      View.open path
      goto_line line.to_i

    end

    self.add(/^[^|-]+\*\*/) do |line|  # **.../: Tree grep in dir
      TreeLs.launch
    end

    self.add(/^[^|]+##/) do |line|  # ##.../: Tree grep in dir
      TreeLs.launch
    end

    self.add(nil, /google/) do |line|  # - google:
      url = Line.without_label.sub(/^\s+/, '')
      url.gsub!('"', '%22')
      url.gsub!(':', '%3A')
      url.gsub!(' ', '%20')
      browse_url "http://www.google.com/search?q=#{url}"
    end

    self.add(/\[\[.+\]\]/) do |line|  # Redmine wiki links
      name = line[/\[\[(.+?)\]\]/, 1]
      name.gsub!(/ /, "_")
      Redmine.open(name)
      #View.open Line.without_indent
    end

    self.add(/^ *$/) do |line|  # Empty line: open dir
      #       Line.to_right
      #       CodeTree.insert_menus
      View.insert("- CodeTree.menu/")
      LineLauncher.launch
    end

    self.add(/^ *\*/) do |line|  # *... buffer
      #return $el.insert "hey"
      name = Line.without_label.sub(/\*/, '')
      View.to_after_bar
      View.to_buffer name
    end

    LineLauncher.add(nil, /^\(\$.+\)$/) do  # - ($bm): command
      bm = Line.label[/^\(\$(.+)\)$/, 1]  # Get bookmark
      dir = Bookmarks["$#{bm}"]
      out = Shell.run CodeTree.line_or_children, :dir => dir, :sync => true
      TreeLs.indent(out)
      TreeLs.insert_quoted_and_search out
    end

    LineLauncher.add(nil, /^\(\/.*\)$/) do  # - (/dir): command
      # Get bookmark
      dir = Line.label[/^\((.+)\)$/, 1]
      out = Shell.run CodeTree.line_or_children, :dir => dir, :sync => true
      out = "(no output)\n" unless out
      TreeLs.indent(out)
      TreeLs.insert_quoted_and_search out
    end

    self.add(/^ *[$\/].+!!/) do |l|   # !shell command
      Shell.launch
    end
    self.add(/^ *!!/) do |l|   # !shell command
      Shell.launch
    end

    self.add(/^ *[$\/].+!/) do |l|   # !shell command inline
      Shell.launch :sync=>true
    end
    self.add(/^ *!/) do |l|   # !shell command inline
      Shell.launch :sync=>true
    end
  end

  def self.file_hooks
    if View.name =~ /#{Bookmarks['$o']}$/   # If in output log
      return true
    end

    return false
    #TODO return true if handled
  end

end
LineLauncher.init_default_launchers
