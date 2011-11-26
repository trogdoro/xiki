module AutoMenu

  def method_missing(func, *args, &block)
    # Exit if no auto_menu()
    unless self.respond_to?(:auto_menu)
      raise NoMethodError.new("function #{func} undefined")
    end

    # Get whole menu, to take subset based on method
    orig_stdout = $stdout;  $stdout = StringIO.new
    output = self.auto_menu  # Invoke auto_menu on extending class
    tree = $stdout.string
    $stdout = orig_stdout

    tree = output if (tree||"").empty?
    tree = TextUtil.unindent(tree)

    # If they called menu(), print out top-level bullets
    if func == :menu
      return tree.grep(/^[+-]/).join('')
    end

    # Otherwise, print out level immediately underneath method they called
    result = AutoMenu.child_menus_deprecated(tree, func.to_s)
    # If none, throw no method error
    if result == ""
      raise NoMethodError.new("function #{func} undefined")
    end
    result
  end

  def self.child_menus(tree, node)

    node = "" if node.nil?

    found = nil
    result = ""

    node.gsub!(/^\//, '')
    node.gsub!(/\/$/, '')
    if ! (node).any?
      return tree.grep(/^[^ ]/).join('').gsub(/^$/, '|')
    end
    node.gsub!(/[.:]/, '')
    Tree.traverse tree do |branch|
      path = branch.map{|o| o.sub /^[+-] /, ''}.join('').gsub(/[.:]/, '')

      if ! found
        if path.start_with? node
          found = branch.length - 1  # Remember indent
        end
      else
        current_indent = branch.length - 1
        # If found and still indented one deeper
        if current_indent == found + 1
          result << "#{branch[-1]}\n"  # Output
        else  # Otherwise, stop looking for children if indent is less
          found = nil if current_indent <= found
        end
      end

    end
    result.empty? ? "" : result.gsub(/^$/, '|')
  end

  # Old version, used by code_tree
  # TODO: remove
  def self.child_menus_deprecated(tree, node)
    found = nil
    result = ""

    # For each line
    tree.split("\n").each do |l|
      # Start outputting if found
      if ! found
        if l =~ /^( *)[+-] \.#{node}\/?$/
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

  def self.strip txt
    txt[/^[ .\/+-]*(.+?)\/?$/, 1]
  end

end
