require 'line'
require 'effects'
require 'ruby_console'

class LineLauncher
  extend ElMixin

  @@just_show = false

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
    # Get paren from line
    line = options[:line] || Line.value
    label = Line.label(line)
    paren = label[/\((.+)\)/, 1] if label

    # Effects.blink
    View.bar if Keys.prefix == 9

    # Split label off, if there
    if line =~ /^( *)- .+?: (.+)/
      line = $1 + $2
    end

    # If this line has parens, run launcher if one exists
    if paren && @@paren_launches[paren]
      @@paren_launches[paren].call
      return
    end

    # For each potential match
    @@launchers.each do |launcher|
      regex, label_regex, block = launcher
      # If we found a match, launch it
      if (!regex || line =~ regex) && (!label_regex || (label && label =~ label_regex))
        # Run it
        if @@just_show
          return insert(regex.to_s + label_regex.to_s)
        else
          block.call line
        end
        return true
      end
    end


    # It must be code_tree node or tree_ls node
    # Still don't know which, so check root
    path = TreeLs.construct_path :list => true
    code_tree_root = CodeTree.is_code_tree_path path

    # If code tree, chop it off and run it
    if code_tree_root# && line =~ /^ *- /  # Must have bullet to be code_tree
      #path = path[code_tree_root..-1]
      CodeTree.launch options
    else  # Otherwise, run treels
      TreeLs.expand_or_open
    end

  end

  def self.init_default_launchers
    @@launchers = []

    self.add /^  +\+?-?\|/ do |line|  # | TreeLs quoted text
      TreeLs.open
    end

    self.add_paren("o") do  # - (t): Insert "Test"

      txt = Line.without_label  # Grab line
      View.to_after_bar  # Insert after bar
      insert txt

      command_execute "\C-m"

    end

    self.add_paren("rc") do  # - (rc): Run in rails console

      line = Line.without_label

      # Make it go to rext if in bar
      if View.in_bar?
        View.to_after_bar
      end
      # Go to console
      View.to_buffer "**console"
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

    self.add(/ *(.*)!!(.+)/) do |line|  # !!shell command
      View.handle_bar
      line =~ / *(.*)!!(.+)/
      dir, command = $1, $2
      Shell.run command, :dir => dir
    end

    self.add(/ *(.*)!!!(.+)/) do |line|  # !!!shell command inline
      line =~ / *(.*)!!!(.+)/
      dir, command = $1, $2
      output = Shell.run command, :dir => dir, :sync => true

      # Insert under
      indent = Line.indent
      Line.start
      started = point
      Line.next

      output.gsub!(/^/, "#{indent}  |")
      insert output
    end

    self.add(/(http|file).?:\/\/.+/) do |line|  # url
      browse_url line[/(http|file).?:\/\/.+/]
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

    self.add(/^[^|-]+\*\*/) do |line|  # Tree grep in dir
      TreeLs.expand_or_open
    end

    self.add(/^[^|]+##/) do |line|  # Tree grep in dir
      TreeLs.expand_or_open
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
      CodeTree.display_menu("CodeTree.menu")
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

  end

end
LineLauncher.init_default_launchers
