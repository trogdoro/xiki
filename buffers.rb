class Buffers

  def self.menu buffer=nil
    puts "
      - .tree 20/
      - .current/
      - .search 'foo'/
      "
  end

  def self.current buffer=nil
    if buffer == nil  # If no buffer, show list
      self.list.map { |b| $el.buffer_name(b) }.to_a.each do |b|
        #name = $el.buffer_name(b)
        puts "+ #{b}"
      end
      return
    end

    # Switch to buffer
    View.to_after_bar if View.in_bar?
    View.to_buffer(buffer)

  end

  def self.viewing_array
    self.list.map { |b| $el.buffer_file_name(b) }.select{|path| path}
  end

  def self.list
    $el.buffer_list
  end

  def self.tree times=0, options={}
    times ||= History.prefix_times
    paths = viewing_array[0..(times-1)]
    if options[:dir]
      paths = paths.grep(Regexp.new(Regexp.escape(options[:dir])))
    end
    puts CodeTree.tree_search_option + FileTree.paths_to_tree(paths)
  end

  def self.search string, options={}
    orig = View.buffer

    # Get buffer from name
    list = options[:buffer] ?
      [self.from_string(options[:buffer])] :
      self.list
    found = ""
    list.to_a.each do |b|  # Each buffer open

      file = $el.buffer_file_name(b)

      next unless file

      # Skip if a verboten file
      unless options[:buffer]
        next if file =~ /(\/difflog\.notes|\.log|\/\.emacs)$/
      end

      $el.set_buffer b
      started = $el.point
      View.to_top
      found_yet = nil
      while(true)
        break unless $el.search_forward(string, nil, true)
        unless found_yet
          found << "- #{file.sub(/(.+)\//, "\\1\/\n  - ")}\n"

          found_yet = true
        end
        found << "    |#{Line.value}\n"
        Line.end
      end
      View.to started
    end

    View.to_buffer orig

    # If nothing found, just insert message
    if found.size == 0
      return puts("| Note\n- ~Nothing found~\n")
    end
    puts found
    #View.insert found
    View.to_top
    # $el.highlight_regexp string, :ls_quote_highlight
  end

  def self.from_string name
    $el.get_buffer name
  end

  def self.open_viewing
    case Keys.prefix
    when nil:  CodeTree.display_menu("- Buffers.tree 25/")
    when 0:  CodeTree.display_menu("- Buffers.tree/")
    else  CodeTree.display_menu("- Buffers.tree #{Keys.prefix}/")
    end
  end

  def self.rename
    options = {:prompt => "Rename buffer to: "}
    options[:initial_input] = $el.buffer_name if Keys.prefix_u?
    $el.rename_buffer Keys.input(options)
  end

end
