class Buffers

  def self.menu# buffer=nil
    "
    - .current/
    - .tree/
      - 20/
    - .search/

    > Lists
    - .list/
    - .current/

    - docs/
      > Todo
    "
  end

  def self.current *name
    if name.empty?  # If no buffer passed in, show list
      case Keys.prefix :clear=>true
      when nil:  return list.select{ |b| $el.buffer_file_name(b) }.map{ |b| "| #{$el.buffer_name(b)}\n" }.join('')
      when 0:  return list.select{ |b| ! $el.buffer_file_name(b) }.map{ |b| "| #{$el.buffer_name(b)}\n" }[1..-1].join('')
        #       when 0:  return list.select{ |b| ! $el.buffer_file_name(b) }.map{ |b| $el.buffer_name(b) }[1..-1]
      when 1:  return list.select{ |b| $el.buffer_file_name(b) }.map{ |b| $el.buffer_name(b) }[1..-1]
      when 3:  return list.select{ |b| ! $el.buffer_file_name(b) && $el.buffer_name(b) =~ /^#/ }.map{ |b| $el.buffer_name(b) }
      when 4:  return list.select{ |b| ! $el.buffer_file_name(b) && $el.buffer_name(b) =~ /^\*console / }.map{ |b| $el.buffer_name(b) }
      when 8, "all":  return list.map{ |b| $el.buffer_name(b) }
      when :u:  return list.select{ |b| ! $el.buffer_file_name(b) && $el.buffer_name(b) =~ /!$/ }.map{ |b| $el.buffer_name(b) }
      end
      return
    end

    # If a buffer name passed, get whole line and escape it

    name = Line.without_indent

    name.sub! /^\| /, ''

    # Switch to buffer
    View.to_after_bar if View.in_bar?
    View.to_buffer(name)
  end

  def self.names_array
    self.list.map { |b| $el.buffer_name(b) }.to_a
  end

  def self.list
    $el.buffer_list.to_a
  end

  def self.tree times=0, options={}
    times ||= History.prefix_times
    paths = View.files[0..(times-1)]
    if options[:dir]
      paths = paths.grep(Regexp.new(Regexp.escape(options[:dir])))
    end
    puts CodeTree.tree_search_option + Tree.paths_to_tree(paths)
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
      #       file = $el.buffer_file_name(b) || "*#{View.name}"
      # Show buffers too - wasn't as simple as just removing, because of filename indenting!

      next unless file
      next if file =~ /_ol.notes/

      if options[:buffer].nil?   # If we're not searching in one buffer
        next if ["todo.notes", "files.notes"].
          member? file.sub(/.+\//, '')
      end

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
          found << "- @#{file.sub(/(.+)\//, "\\1\/\n  - ")}\n"

          found_yet = true
        end
        found << "    | #{Line.value}\n"
        Line.end
      end
      View.to started
    end

    View.to_buffer orig

    # If nothing found, just insert message
    if found.size == 0
      Tree << "- nothing found!\n"
      Search.isearch string, :reverse=>1
      return
    end

    Tree << found
    # $el.highlight_regexp string, :ls_quote_highlight
  end

  def self.from_string name
    $el.get_buffer name
  end

  def self.open_viewing
    case Keys.prefix
    when nil:  Launcher.open("- Buffers.tree 25/")
    when 0:  Launcher.open("- Buffers.tree/")
    else  Launcher.open("- Buffers.tree #{Keys.prefix}/")
    end
  end

  def self.rename
    options = {:prompt => "Rename buffer to: "}
    options[:initial_input] = $el.buffer_name if Keys.prefix_u?
    $el.rename_buffer Keys.input(options)
  end

  def self.name buffer
    $el.buffer_name(buffer)
  end

  def self.kill name
    self.delete name
  end

  def self.delete name
    $el.kill_buffer name
  end

  def self.to name
    View.to_buffer name
  end

  def self.txt name
    $el.with(:save_window_excursion) do
      $el.switch_to_buffer name
      View.txt
    end
  end

end
