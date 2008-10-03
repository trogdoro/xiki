require 'line'

class CodeTree
  extend ElMixin

  CODE_SAMPLES = "
    # Menus for all CodeTree classes
    The menus serve as User Interfaces for the classes.

    - show menus: CodeTree.menu
  "

  # Mapped to C-.
  def self.launch options={}
    TreeLs.extra_line_if_end_of_file
    TreeLs.plus_to_minus_maybe
    orig = Location.new
    Line.to_left
    line = Line.without_indent
    path = TreeLs.construct_path :list => true
    path.each do |l|
      # if '- .xx:/", get rid of trailing slash
      l.sub!(/^([+-] .*\..+)\/$/, "\\1")
    end
    # Determine code to eval
    code = self.determine_code_from_path path
    orig.go
    self.run code, options
  end

  def self.run code, options={}
    b = View.buffer
    orig = Location.new
    orig_left = point
    returned, stdout, e = Code.eval(code)  # Eval code
    # If no stdout (and something was returned), print return value
    if ! stdout.nonempty? and returned.respond_to?(:any?) and returned.any?
      stdout =
        if returned.is_a? Array
          returned.map{|l| "#{l}\n"}.join('')
        elsif returned.is_a? Hash
          (returned.map{|k, v| v =~ /\/$/ ? "+ #{k}: #{v}" : "- #{k}: #{v}"}.join("\n")) + "\n"
        else
          "#{returned}\n"
        end
      # If list, join and bulletize
    else
      message(returned.to_s) if returned and (!returned.is_a?(String) or returned.size < 500)
    end

    if e
      returned = ''
      stdout = e.is_a?(ScriptError) ?
        "- #{e.message.sub(/.+: /, '')}!\n" :
        "#{stdout}- error evaluating '#{code}': #{e.message}\n#{e.backtrace.join("\n")}\n"
    end

    buffer_changed = b != View.buffer   # Remember whether we left the buffer
    # Insert output if there was any
    if stdout.nonempty?
      # Pull out flags
      if stdout =~ /^:code_tree_option_tree_search\n/
        options[:tree_search] = true
        stdout.sub! /.+\n/, ''   # Remove option
      elsif stdout =~ /^:code_tree_option_no_search\n/
        options[:no_search] = true
        stdout.sub! /.+\n/, ''   # Remove option
      end

      ended_up = Location.new

      # Go back to where we were before running code
      orig.go
      indent = Line.indent
      Line.to_left
      Line.next
      left = point

      # Move what they printed over to left margin initally, in case they haven't
      stdout = TextUtil.unindent(stdout)
      # Remove any double linebreaks at end
      stdout.sub!(/\n\n\z/, "\n")

      stdout.gsub!(/^/, "#{indent}  ")

      insert stdout  # Insert output
      right = point

      # Move cursor back
      orig.go
      # End up where script took us
      ended_up.go

      if options[:tree_search]  # If they want to do a tree search
        goto_char left
        TreeLs.select_next_file
        #Line.to_words
        TreeLs.search(:left => left, :right => right, :recursive => true)
      # If script didn't move us (line or buffer), do incremental search
      elsif !options[:no_search] && !buffer_changed && point == orig_left
        goto_char left
        Line.to_words
        TreeLs.search(:left => left, :right => right, :number_means_enter => true)
      end
    end
  end

  def self.menu
    l = []
    ObjectSpace.each_object(Class) do |c|
      next unless c.respond_to?(:menu) || c.respond_to?(:auto_menu)
      l << c.name
    end
    l.map! {|c| c.sub(/^#.+::/, '')}
    l.sort.each do |c|
      next if c == "CodeTree"
      puts "+ #{c}.menu/"
    end
    ""
  end

  def self.display_menu menu
    View.bar if Keys.prefix_u?

    # For buffer name, handle multi-line strings
    buffer = "*CodeTree " + menu.sub(/.+\n[ -]*/m, '').gsub(/[.,]/, '')
    View.to_buffer(buffer)
    View.clear
    $el.notes_mode

    insert "#{menu}"
    open_line 1
    CodeTree.launch
  end

  def self.open_menu
    CodeTree.display_menu("- CodeTree.menu/")
  end

  def self.layout_menu
    View.bar if Keys.prefix_u?

    buffer = "*CodeTree CodeTree menu"
    if View.buffer_open? buffer   # If open, switch to it
      View.to_buffer(buffer)
    else # Else open it
      self.display_menu("- CodeTree.menu/")
    end
  end

  def self.insert_menu menu
    insert "- #{menu}/"
    open_line 1
    CodeTree.launch
  end

  def self.insert_menus
    # Implement
    self.insert_menu "- CodeTree.menu/"
  end

  def self.insert_menu txt
    View.insert txt
    $el.open_line(1)
    CodeTree.launch
  end

  # Determine whether path is code_tree or tree_ls
  #   def self.is_code_tree_path list
  #     code_tree_root = nil
  #     index = list.size - 1
  #     list.reverse.each do |l|

  #       # If it has a char that wouldn't be in a file, must be code tree
  #       #return index if l =~ /[\[\{=]/

  #       # If last one was suspected as root, confirm we're not a dir (must be a file if parent is a dir)
  #       if code_tree_root
  #         if l =~ /\/$/  # Dir means it was a file that looked like code
  #           code_tree_root = nil
  #         else
  #           return index + 1  # Must be legit, so return our index
  #         end
  #       end
  #       # If function call, it might be the root
  #       if l =~ /^[+-]? ?[A-Z][A-Za-z0-9]*\.[a-z]/
  #         code_tree_root = index
  #       end
  #       index -= 1
  #     end

  #     code_tree_root
  #   end

  # Rules for constructing code from path
  # - examine path consisting of where C-. occurred and all its ancestors
  # - root of tree is first ancestor with Class.method
  # - invoke first ancestor method (using its params)
  # - append all ancestor data nodes as params
  #   - include self if a data node
  def self.determine_code_from_path path
    data = []
    clazz = metho = nil
    metho_maybe = clazz_maybe = nil

    i = -1

    path.reverse.each do |l|   # Climb up path
      i += 1
      metho_tmp = self.extract_method(l)
      clazz_tmp = self.extract_class(l)
      l.sub(/^(\s+)[+-] /, "\\1")   # Remove bullets
      is_root = i + 1 == path.size
      code_node = false
      if metho_tmp   # If line has method
        if clazz_tmp   # If line has class
          if is_root || self.definite_code_tree_root(l)
            metho ||= metho_tmp
            code_node = true
            clazz ||= clazz_tmp
            break if clazz
          else   # Use this if nothing else is found
            metho_maybe ||= metho_tmp
            clazz_maybe ||= clazz_tmp
          end
        else   # Otherwise, it can be a method
          metho ||= metho_tmp
          code_node = true
        end
      end
      if ! code_node   # If not code node, must be data node
        # Prepend to data list
        data << self.paramify(l)
      end
    end

    unless clazz   # If no code found
      return nil unless clazz_maybe
      metho = metho_maybe
      clazz = clazz_maybe
      data = []
    end

    # If one line, return it literally
    if i == 0
      return Line.without_label(:line=>path[-1]).sub(/\/$/, '')
    end

    method_with_params, params = metho.match(/(\w+\??)(.*)/)[1..2]
    params.sub!(/^\((.*)\)$/, "\\1")  # Remove parens if they're there

    # If any data nodes, pass as params
    if ! data.empty?
      data.reverse!
      data.map! {|a| "\"#{a}\""}
      params << ", " + data.join(", ")
    end
    # TODO Get rid of comma if there is one
    params.sub!(/^, /, '')
    "#{clazz}.#{method_with_params}(#{params})"

  end

  def self.extract_class l
    l[/^([A-Z][A-Za-z0-9]*)\.[a-z]/, 1]
  end

  def self.extract_method l
    l = l.sub(/^[+-] [\w -]+?: /, '').sub(/^[+-] /, '')  # Remove bullets
    # Either class or bol
    result = l[/^[A-Z][A-Za-z0-9]*\.([a-z].*)/, 1] ||  # Class and method
      l[/^\.([a-z].*)/, 1]  # Method at beginning of line
    result ? result.sub(/\/$/, '') : nil
  end

  def self.paramify l

    l = Line.without_label(:line=>l)

    # Always escape backslashes, single-quotes, #'s
    l.gsub!("\\", "\\\\\\\\")
    l.gsub!("\"", "\\\\\"")
    l.gsub!("#", "\\\\#")
    if l =~ /^\|/   # If |..., escape single-quotes
      #       l.gsub!("'", "\\'")
    else
      l.gsub! ', ', '", "'
    end

    l
  end

  def self.sibling_bounds
    indent_size = Line.indent.size   # Get indent

    orig = Location.new

    right1 = Line.left   # Right side of lines before
    left2 = Line.left 2   # Left side of lines after

    # Search for line indented less
    Search.backward "^ \\{0,#{indent_size-1}\\}\\($\\|[^ \n]\\)"
    Line.next
    left1 = Line.left   # Left side of lines before

    # Search for line indented less
    Search.forward "^ \\{0,#{indent_size-1}\\}\\($\\|[^ \n]\\)"
    right2 = Line.left   # Left side of lines before
    orig.go

    [left1, right1, left2, right2]
  end

  # Returns group of lines close to current line that are indented at the same level.
  # The bounds are determined by any lines indented *less* than the current line (including
  # blank lines).  In this context, lines having only spaces are not considered blank.
  # Any lines indented *more* than the current line won't affect the bounds, but will be
  # filtered out.
  def self.siblings options={}
    left1, right1, left2, right2 = self.sibling_bounds

    # Combine and process siblings
    siblings = View.txt(left1, right1) + View.txt(left2, right2)
    siblings.gsub! /^#{Line.indent} .*\n/, ''   # Remove more indented lines
    siblings.gsub! /^ +\n/, ''   # Remove blank lines
    siblings.gsub! /^ +/, ''   # Remove indents
    siblings = siblings.split("\n")

    unless options[:include_label]   # Optionally remove labels
      siblings.map!{|i| Line.without_label(:line=>i)}
    end

    Effects.blink :left=>left1, :right=>right2

    siblings
  end

  def self.child
    next_line = Line.value 2
    # If indent is one greater, it is a child
    if Line.indent.size + 2 == Line.indent(next_line).size
      return Line.without_label(:line=>next_line)
    end
    nil
  end

  def self.kill_siblings
    left1, right1, left2, right2 = self.sibling_bounds
    Effects.blink :left=>left2, :right=>right2
    View.delete left2, right2
    Effects.blink :left=>left1, :right=>right1
    View.delete left1, right1
  end

  def self.children options={}
    child = self.child
    return nil if child.nil?  # Return if no child

    indent = Line.indent(Line.value(2)).size
    children = options[:as_hash] ? {} : []
    i = 2

    # Add each child indented the same or more
    while(Line.indent(Line.value(i)).size >= indent)
      child = Line.value(i)
      if options[:as_hash]
        match = child.match(/ *([\w -]+): (.+)/)
        if match
          k, v = match[1..2]
          children[k] = v
        else
          i += 1
          next
        end

      else
        children << child
      end

      i += 1
    end

    children
  end

  def self.line_or_children
    # Return line, if there is one
    line = Line.without_label
    return line unless line == ""

    # If line is empy, return children
    CodeTree.children.join("\n")
  end

  def self.tree_search_option
    ":code_tree_option_tree_search\n"
  end

  def self.no_search_option
    ":code_tree_option_no_search\n"
  end

  def self.definite_code_tree_root line
    line =~ /^[\s+-]*[A-Z].+\..+[\/)]$/
  end

  def self.maybe_code_tree_root line
    line =~ /^[\s+-]*[A-Z].+\.[a-z]/
  end

  def self.definite_dir_tree_root line
    line =~ /^[\s+-]*\//
  end

  # Turn list into bulleted string (adding + or - based on ending in slash)
  def self.bulletize list
    (list.map{|l| l =~ /\/$/ ? "+ #{l}" : "- #{l}"}.join("\n")) + "\n"
  end

end
