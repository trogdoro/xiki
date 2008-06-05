module AutoMenu

  CODE_SAMPLES = %q<
    # Let the Foo CodeTree class define multi-level menus with one method
    class Foo
      extend AutoMenu

      def self.auto_menu item=nil
        "- .reminders
         - .backup
           - .backup_code()
         ".gsub(/^       /, '')
      end

    - This will now return relevant substrings from Foo.auto_menu
      - Foo.menu
  >

  def method_missing(func, *args, &block)

    # Exit if no auto_menu()
    unless self.respond_to?(:auto_menu)
      raise NoMethodError.new("function #{func} undefined")
    end

    # Get whole menu, to take subset based on method
    orig_stdout = $stdout;  $stdout = StringIO.new
    self.auto_menu  # Invoke auto_menu on extending class
    tree = TextUtil.unindent($stdout.string)
    $stdout = orig_stdout

    # If they called menu(), print out top-level bullets
    if func == :menu
      return puts(tree.grep(/^-/).join(''))
    end

    # Otherwise, print out level immediately underneath method they called
    result = AutoMenu.child_bullets(tree, func)
    # If none, throw no method error
    if result == ""
      raise NoMethodError.new("function #{func} undefined")
    end
    puts result
  end

  def self.child_bullets(tree, node)
    found = nil
    result = ""
    # For each line
    tree.split("\n").each do |l|
      # Start outputting if found
      if ! found
        if l =~ /^( *)- \.#{node}\/?$/
          found = $1.size  # Remember indent
        end
      else
        current_indent = l[/^ */].size
        # If found and still indented one deeper
        if current_indent == found + 2
          result << "#{l.sub(/^ +/, '')}\n"  # Output
        else  # Otherwise, stop looking for children if indent is less
          found = nil if current_indent <= found
        end
      end

      # If found
    end
    result
  end

end
