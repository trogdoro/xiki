require 'tree_cursor'

class Tree
  def self.search options={}
    return $xiki_no_search=false if $xiki_no_search
    #     return if $xiki_no_search

    recursive = options[:recursive]
    recursive_quotes = options[:recursive_quotes]
    left, right = options[:left], options[:right]
    if ! left
      ignore, left, right = View.block_positions "^>"
    end

    # No search if there aren't more than 3 lines
    return if((Line.number(right) - Line.number(left)) <= 1 && View.txt(left, right) !~ /\/$/) && options[:always_search].nil?

    # Make cursor blue
    Cursor.remember :before_file_tree
    Cursor.blue
    error = ""

    pattern = ""
    lines = $el.buffer_substring(left, right).split "\n"

    Message << "filter... "

    ch, ch_raw = Keys.char
    if ch.nil?
      return Cursor.restore(:before_file_tree)
    end

    # While narrowing down list
    while (ch =~ /[ -"&-),.\/:<?A-~]/) ||   # Be careful editing, due to ranges (_-_)
        (recursive && ch_raw == 2 || ch_raw == 6) ||
        ch == :up || ch == :down
      if ch == ' '
        pattern = ''
      elsif ch_raw == 2   # C-b
        while(Line.previous == 0)
          next if FileTree.dir?  # Keep going if line is a dir
          Line.to_words
          break  # Otherwise, stop
        end

      elsif ch_raw == 6   # C-f
        while(Line.next == 0)
          next if FileTree.dir?  # Keep going if line is a dir
          Line.to_words
          break  # Otherwise, stop
        end
      elsif ch == :up
        Line.previous
        Line.to_words
      elsif ch == :down
        Line.next
        Line.to_words

      else
        if ch == "\\"  # If escape, get real char
          ch = $el.char_to_string($el.read_char)
        end
        pattern << Regexp.quote(ch)

        if pattern =~ /[A-Z]$/   # If upper, remove any lower
          pattern.sub!(/^[a-z]+/, '')
        elsif pattern =~ /[a-z]$/   # If lower, remove any upper
          pattern.sub!(/^[A-Z]+/, '')
        end

        regexp = pattern

        $el.delete_region left, right

        regexp = "\\/$|#{regexp}" if recursive
        # Always keep if "- file" or "- /dir"
        regexp = "^ *:\\d|^ *[+-] [a-zA-Z0-9:.\/]|#{regexp}" if recursive_quotes

        regexp = /#{regexp}/i
        lines_new = nil
        if pattern =~ /[A-Z]$/   # If upper, search in directory
          lines_new = search_dir_names(lines, /#{pattern}/i)
        else
          lines_new = lines.grep(regexp)
        end
        # If search not found, don't delete all
        if lines_new.size == 0
          error = "   ---------- no matches! ---------- "
          View.beep
        else
          lines = lines_new
        end

        # Remove dirs with nothing under them
        self.clear_empty_dirs! lines if recursive
        self.clear_empty_dirs!(lines, :quotes=>true) if recursive_quotes

        # Put back into buffer
        View.insert(lines.join("\n") + "\n")
        right = $el.point

        # Go to first file
        $el.goto_char left

        # Move to first file
        if recursive
          FileTree.select_next_file
        elsif recursive_quotes
          Search.forward "|\\|#"
          Line.to_beginning
        else
          Line.to_beginning
        end

      end

      message = "filter... #{pattern}#{error}"
      message << "    (space for 'and')" if pattern.present?
      Message << message
      ch, ch_raw = Keys.char
      if ch.nil?
        return Cursor.restore(:before_file_tree)
      end

    end

    # Exiting, so restore cursor
    Cursor.restore :before_file_tree

    # Options during search
    case ch   # Do option based on last char, or run as command
    when "0"
      file = self.construct_path   # Expand out ~
      $el.shell_command("open #{file}")
    when "\C-a"
      Line.to_left
    when "\C-j"
      ch = Keys.input :chars=>1
      if ch == 't'   # just_time
        self.to_parent
        self.kill_under
        FileTree.dir :date_sort=>true
      elsif ch == 's'   # just_size
        self.to_parent
        self.kill_under
        FileTree.dir :size_sort=>true
      elsif ch == 'n'   # just_name
        self.to_parent
        self.kill_under
        FileTree.dir
      end
    when :control_return, :return, "\C-m", :control_period, :right   # If C-., go in but don't collapse siblings
      Keys.clear_prefix
      Launcher.launch
    when "\t"   # If tab, hide siblings and go in
      $el.delete_region(Line.left(2), right)
      Keys.clear_prefix
      Launcher.launch
    when :backspace, :left   # Collapse tree
      self.to_parent
      self.kill_under
      self.search(:left => Line.left, :right => Line.left(2))

    when :control_slash   # Collapse tree and exit
      self.to_parent
      self.kill_under

    when "9"   # Show methods, or outline
      $el.delete_region(Line.left(2), right)   # Delete other files
      return FileTree.drill_quotes_or_enter_lines self.construct_path.sub(/\|.*/, ''), Line.=~(/^ *\|/)

    when "#"   # Show ##.../ search
      self.stop_and_insert left, right, pattern
      View.insert self.indent("- ##/", 0)
      View.to(Line.right - 1)

    when "*"   # Show **.../ search
      self.stop_and_insert left, right, pattern
      View.insert self.indent("- **/", 0)
      View.to(Line.right - 1)

    when "$"   # Insert '$ ' for command
      self.stop_and_insert left, right, pattern
      View.insert self.indent("$ ", 0)

    when "%"   # Insert '!' for command
      self.stop_and_insert left, right, pattern
      View.insert self.indent("% ", 0)

    when "-"   # Insert '-' for bullet
      self.stop_and_insert left, right, pattern
      View.insert self.indent("- ", 0)

    when "@"   # Insert '@' for menus
      self.stop_and_insert left, right, pattern
      View.insert self.indent("@", 0)

    when "+"   # Create dir
      self.stop_and_insert left, right, pattern, :dont_disable_control_lock=>true
      Line.previous
      parent = self.construct_path
      Line.next
      View.insert self.indent("", 0)
      name = Keys.input(:prompt=>'Name of dir to create: ')
      Dir.mkdir("#{parent}#{name}")
      View.insert "- #{name}/\n"
      View.insert self.indent("", 0)
      #Line.to_right

    when ">"   # Split view, then launch
      $el.delete_region(Line.left(2), right)
      Keys.clear_prefix
      View.create
      Launcher.launch

    when "8"
      # If a quote, insert lines indented lower
      if Line.matches(/\|/)
        CodeTree.kill_siblings
        self.enter_under
      elsif FileTree.dir?  # A Dir, so do recursive search
        $el.delete_region(Line.left(2), right)
        FileTree.dir_recursive
      else   # A file, so enter lines
        $el.delete_region(Line.left(2), right)
        FileTree.enter_lines(//)  # Insert all lines
      end

    when "1".."7"
      if ch == "7" and ! View.bar?   # Open in bar
        $el.delete_region(Line.left(2), right)  # Delete other files
        View.bar
        Keys.clear_prefix
        # Expand or open
        Launcher.launch
        return
      end
      Keys.clear_prefix
      n = ch.to_i

      # Pull whole string out
      lines = $el.buffer_substring(left, right).split "\n"
      $el.delete_region left, right
      if recursive
        filtered = []
        file_count = 0
        # Replace out lines that don't match (and aren't dirs)
        lines.each_with_index do |l, i|
          is_dir = (l =~ /\/$/)
          file_count += 1 unless is_dir
          # If dir or nth, keep
          filtered << l if (is_dir or (file_count == n))
          #p l
        end

        # Remove dirs with nothing under them
        self.clear_empty_dirs! filtered

        # Put back into buffer
        View.insert(filtered.join("\n") + "\n")
        right = $el.point

        # Go to first file and go back into search
        $el.goto_char left
        FileTree.select_next_file

        # Todo: merge this and the following .search
        self.search(:recursive => true, :left => Line.left, :right => Line.left(2))
      else
        nth = lines[ch.to_i - 1]
        View.insert "#{nth}\n"
        $el.previous_line
        if options[:number_means_enter]  # If explicitly supposed to enter
          Launcher.launch
        elsif FileTree.dir?  # If a dir, go into it
          Launcher.launch
        else
          Launcher.launch
          return

          Line.to_beginning
          # Get back into search, waiting for input
          self.search(:left => Line.left, :right => Line.left(2))
        end
      end

    when "\C-s"
      $el.isearch_forward

    when "\C-r"
      $el.isearch_backward

    when ";"   # If semicolon, append selected dir to parent dir

      # If CodeTree search
      if CodeTree.handles?
        # Kill others
        View.delete(Line.left(2), right)

        if Line.without_label =~ /^\./   # If just a method
          # Back up to first . on last line
          Search.forward "\\."
          right = View.cursor
          Line.previous
          Search.forward "\\."
        else   # Else, just delete previous line
          right = View.cursor
          Line.previous
          Line.to_beginning
        end
        View.delete(View.cursor, right)
        return Launcher.launch
      end

      $el.delete_region(Line.left(2), right)  # Delete other files
      $el.delete_horizontal_space
      $el.delete_backward_char 1

      # delete -|+ if there
      if View.txt(View.cursor, Line.right) =~ /^[+-] /
        $el.delete_char 2
      end

      Launcher.launch

    when "="   # Drill into the file
      dir = self.construct_path  # Expand out ~
      View.open(dir)

    when "0"   # Drill into the file
      $el.delete_region(Line.left(2), right)   # Delete other files
      self.drill

    else
      $el.command_execute ch
    end
  end

  # Drill into a file, showing lines one indent level deeper
  # Returns lowest item in path, or if pipe-quoted
  # grabs siblings and turns into multi-line string.
  def self.leaf path, options={}

    if path =~ /(?:^\||\/\|) ?(.*)/   # If has ^| or /|, grab siblings
      orig = $1
      # First, make sure the current line is quoted (otherwise, .siblings will be pulling from somewhere else)
      return orig if options[:dont_look] || Line.value !~ /^ *\|/

      siblings = Tree.siblings(:quotes=>1)
      siblings = siblings.map{|i| i.gsub(/^\| ?/, '')}.join("\n")  # :
      siblings << "\n" if siblings =~ /\n/
      return siblings
    end

    path.split("/")[-1]

  end

  def self.rootless path
    path.sub /^\/?[^\/]+\/?/, ''
  end

  def self.root path
    path.sub /\/.*/, ''
  end

  def self.drill
    Line.to_left
    # Get indent between | and tabs
    match = Line.value.match(/^( *)\|( *)(.+)/)
    # Get content
    path = Bookmarks.expand(construct_path)  # Get path
    matches = ""
    if match  # It's a quoted line
      indent, indent_after_bar, rest = match[1,3]
      indent_after_bar.gsub!("\t", "        ")
      #"#{indent_after_bar}#{rest}"
      # Go through and find line
      found = nil
      IO.foreach(path[/(.+?)\|/, 1]) do |line|
        line.sub!(/[\r\n]+$/, '')
        # If not found yet, check for line
        if !found && line == "#{indent_after_bar}#{rest}"
          found = true
          next
        end
        next unless found

        # Exit if indented at same level (unless blank or comment)
        if((Line.indent(line).length <= indent_after_bar.length) &&
           (! (line =~ /^\s*$/)) &&
           (! (line =~ /^\s*#/))
           )
          break
        end

        # Skip unless indented 2 later
        next unless Line.indent(line).length == 2 + indent_after_bar.length

        matches << "#{indent}| #{line}\n"
      end
      self.insert_quoted_and_search matches
    else  # It's a file
      indent = Line.value[/^ */]
      this_was_used = last_was_used = false
      IO.foreach(path) do |line|  # Print lines with no indent
        last_was_used = this_was_used
        this_was_used = false
        line.sub!(/[\r\n]+$/, '')
        next if line =~ /^ +/  # Skip non top-level lines
        next if line =~ /^$/ and ! last_was_used  # Skip blank lines, unless following top-level
        matches << "#{indent}  | #{line}\n"
        this_was_used = true
      end
      self.insert_quoted_and_search matches
      # TODO Search in result

      # TODO Do some checking for duplicates
    end
  end

  def self.clear_empty_dirs! lines, options={}
    regex = options[:quotes] ?
      /^ +[+-] [^#|]+$|^ *:\d+$/ :
      /^[^|]+\/$/

    lines = lines.split("\n") if lines.is_a?(String)

    file_indent = 0
    i = lines.length
    while( i > 0)
      i -= 1
      l = lines[i]

      l =~ /^( +)/
      spaces = $1 ? $1.length : 0
      if l =~ regex   # If thing to always keep (dir, or dir and file)
        if spaces < file_indent   # If lower than indent, decrement indent
          file_indent -= 2
        else   # Else, delete
          lines.delete_at i
        end
      else   # If file
        file_indent = spaces   # Set indent
      end
    end
    lines
  end

  def self.stop_and_insert left, right, pattern, options={}
    $el.goto_char left
    # TODO: delete left if recursive - emulate what "delete" does to delete, first
    pattern == "" ?
      $el.delete_region($el.point, right) :
      Line.next
    $el.open_line 1
    ControlLock.disable unless options[:dont_disable_control_lock]
  end

  def self.kill_under options={}
    indent = Line.indent.size
    pattern = "^ \\{0,#{indent}\\}\\([^ \n]\\|$\\)"

    self.minus_to_plus_maybe unless options[:no_plus]

    # Get indent
    orig = Line.left
    left = Line.left(Keys.prefix_u? ? 1 : 2)
    Line.next
    Search.forward pattern
    Line.to_left
    View.delete(left, View.cursor)
    View.to orig
  end

  def self.under txt, options={}

    return if txt.nil?

    txt = TextUtil.unindent(txt) if txt =~ /\A[ \n]/

    escape = options[:escape] || ''
    txt = txt.gsub!(/^/, escape)
    txt.gsub!(/^\| $/, '|')

    # Add linebreak at end if none
    txt = "#{txt}\n" unless txt =~ /\n/

    # Insert linebreak if at end of file

    txt.gsub! /^  /, '' if options[:before] || options[:after]   # Move back to left

    self.output_and_search txt, options
    nil
  end

  def self.to_parent
    prefix = Keys.prefix :clear=>true

    # U means go to previous line at margin
    if prefix == :u
      Search.backward "^[^ \t\n]"
      return
    end

    times = prefix || 1
    times.times do
      indent = Line.value[/^  ( *)/, 1]

      # If odd indent, subtract 1
      indent.slice!(/ /) if indent && indent.length % 2 == 1

      $el.search_backward_regexp "^#{indent}[^\t \n]"
      Line.to_beginning :quote=>1
    end
  end

  def self.plus_to_minus
    self.toggle_plus_and_minus if Line.matches(/^\s*\+ /)
  end

  def self.plus_to_minus_maybe
    self.plus_to_minus if Line.matches(/(^\s*[+-] [a-z]|\/$)/)
  end

  def self.minus_to_plus
    self.toggle_plus_and_minus if Line.matches(/^\s*- /)
  end

  def self.minus_to_plus_maybe
    self.minus_to_plus if Line.matches(/(^\s*[+-] [a-z]|\/$)/)
  end

  # Mapped to Enter when on a FileTree buffer.  Opens file cursor is on in the tree.
  # It assumes the path to a dir is on the current line.
  def self.construct_path options={}
    begin
      path = []
      orig = $el.point

      # Do until we're at a root
      line = Line.value
      clean = self.clean_path line
      while(line =~ /^ / && (options[:all] || clean !~ /^@/))
        line =~ /^  ( *)(.+)/
        spaces, item = $1, $2
        item = clean unless options[:raw]
        path.unshift item  # Add item to list
        $el.search_backward_regexp "^#{spaces}[^\t \n]"

        # If ignoring Ol lines, keep searching until not on one
        if options[:ignore_ol]
          while Line =~ /^[# ]*Ol\b/
            $el.search_backward_regexp "^#{spaces}[^\t \n]"
          end
        end

        line = Line.value
        clean = self.clean_path line
      end
      # Add root of tree
      root = Line.value.sub(/^ +/, '')
      root = self.clean_path(root) unless options[:raw]
      root.slice! /^@ ?/
      path.unshift root

      path.map!{|o| o =~ /\/$/ ? o : "#{o}/"} if options[:slashes]

      $el.goto_char orig
      if options[:indented]
        indentify_path path
      elsif options[:list]
        path
      else
        path.join
      end
    rescue Exception=>e
      raise ".construct_path couldn't construct the path - is this a well-formed tree\?: #{e}"
    end
  end

  def self.to_root
    Move.to_end   # In case we're already on root at left margin
    $el.search_backward_regexp "^[^\t \n]"
  end

  def self.is_root? path
    # It's the root if it's not at the left margin
    result = path !~ /^ /
    result ? true : false
  end

  def self.clean_path path
    path = Line.without_label :line=>path#, :leave_indent=>true
    path.sub!(/^([^|\n-]*)##.+/, "\\1")  # Ignore "##"
    path.sub!(/^([^|\n-]*)\*\*.+/, "\\1")  # Ignore "**"
    path
  end

  def self.toggle_plus_and_minus
    orig = Location.new
    l = Line.value 1, :delete => true
    case l[/^\s*([+-])/, 1]
    when '+'
      View.insert l.sub(/^(\s*)([+-]) /, "\\1- ")
      orig.go
      '+'
    when '-'
      View.insert l.sub(/^(\s*)([+-]) /, "\\1+ ")
      orig.go
      '-'
    else
      View.insert l
      orig.go
      nil
    end
  end

  def self.acronym_regexp search
    search.gsub(/([a-zA-Z])/, "[a-z]*[_.]\\1").sub(/.+?\].+?\]/,'^ +')
  end

  def self.search_dir_names(lines, regexp)
    result = []
    stack = [0]
    indent = indent_size(lines[0])
    lines.each do |l|
      last_indent = indent

      indent, name = l.match(/^( *)(.+)/)[1..2]
      indent = indent_size(indent)
      if indent > last_indent
        stack << 0
      elsif indent < last_indent
        (last_indent - indent).times { stack.pop }
      end
      stack[stack.size-1] = name
      # If file, remove this line if path doesn't match
      if stack.last !~ /\/$/
        next unless stack[0..-2].join =~ regexp
      end
      result << l
    end
    result
  end

  def self.indent_size(line)
    spaces = line[/^ +/]
    return 0 unless spaces
    spaces.size / 2
  end

  def self.indent txt, line=1
    indent = Line.indent(Line.value(line))
    txt.gsub!(/^/, "#{indent}  ")
  end

  def self.unquote! txt
    txt.replace self.unquote(txt)
  end

  def self.unquote txt
    txt.gsub(/^\| ?/, '')
  end

  def self.quote txt
    TextUtil.unindent(txt).gsub(/^([^|@>+-])/, "| \\1").gsub(/^\| $/, '|')
  end

  def self.insert_quoted_and_search matches, options={}
    # Insert matches
    Line.next
    left = $el.point
    View.insert matches, options
    right = $el.point

    $el.goto_char left
    if options[:line_found] && options[:line_found] > 0
      Line.next(options[:line_found]-1)
      Color.colorize :l
    end

    Line.to_words
    # Do a search

    return if options[:no_search]

    Tree.search(:left=>left, :right=>right)
  end

  def self.<< txt, options={}
    self.under txt, options
  end

  def self.after txt
    View.under txt, :after=>1
  end

  # Returns group of lines close to current line that are indented at the same level.
  # The bounds are determined by any lines indented *less* than the current line (including
  # blank lines).  In this context, lines having only spaces are not considered blank.
  # Any lines indented *more* than the current line won't affect the bounds, but will be
  # filtered out.
  def self.siblings options={}
    left1, right1, left2, right2 = self.sibling_bounds

    return self.siblings(:all=>true).join("\n").gsub(/^ *\| ?/, '')+"\n" if options[:string]

    # Combine and process siblings
    if options[:all] || options[:everything]
      siblings = View.txt(options.merge(:left=>left1, :right=>right2))

    elsif options[:quotes]   # Only grab contiguous quoted lines
      above = View.txt(options.merge(:left=>left1, :right=>right1))
      found = true
      above = above.split("\n").reverse.select{|o| found && o =~ /^ *\|/ or found = false}.reverse.join("\n")
      above << "\n" if above.any?

      middle = View.txt(options.merge(:left=>right1, :right=>left2))

      below = View.txt(options.merge(:left=>left2, :right=>right2))
      found = true
      below = below.split("\n").select{|o| found && o =~ /^ *\|/ or found = false}.join("\n")
      below << "\n" if below.any?

      siblings = "#{above}#{middle}#{below}"  #.strip

    else
      siblings = View.txt(options.merge(:left=>left1, :right=>right1)) + View.txt(options.merge(:left=>left2, :right=>right2))
    end

    if options[:everything]
      indent = siblings[/\A */]
      return siblings.gsub(/^#{indent}/, '')
    end

    siblings.gsub! /^#{Line.indent} .*\n/, ''   # Remove more indented lines
    siblings.gsub! /^ +\n/, ''   # Remove blank lines
    siblings.gsub! /^ +/, ''   # Remove indents
    siblings = siblings.split("\n")

    unless options[:include_label]   # Optionally remove labels
      siblings.map!{|i| Line.without_label(:line=>i)}
    end

    siblings
  end

  def self.sibling_bounds
    indent_size = Line.indent.size   # Get indent
    indent_less = indent_size - 1

    orig = Location.new

    right1 = Line.left   # Right side of lines before

    # Search for line indented less - parent (to get siblings after)
    indent_less < 0 ?
      Search.backward("^$") :
      Search.backward("^ \\{0,#{indent_less}\\}\\($\\|[^ \n]\\)")

    Line.next
    left1 = Line.left   # Left side of lines before

    orig.go

    # Search for line indented same or less (to get siblings after)
    Line.next
    Search.forward "^ \\{0,#{indent_size}\\}\\($\\|[^ \n]\\)"
    Line.to_left
    left2 = View.cursor
    # Search for line indented less
    indent_less < 0 ?
      Search.forward("^$") :
      Search.forward("^ \\{0,#{indent_less}\\}\\($\\|[^ \n]\\)")
    right2 = Line.left   # Left side of lines before
    orig.go

    [left1, right1, left2, right2]
  end

  def self.search_appropriately left, right, output, options={}

    View.cursor = left unless options[:line_found]
    Line.to_words

    # Determine how to search based on output!

    params = {:left=>left, :right=>right}

    root_indent = output[/\A */]
    if output =~ /^#{root_indent}  /   # If any indenting
      if output =~ /^  +\|/
        Search.forward "^ +\\(|\\|- ##\\)", :beginning=>true
        Line.to_beginning
        params[:recursive_quotes] = true
      else
        FileTree.select_next_file
        params[:recursive] = true
      end
      Tree.search params
    else
      Tree.search params.merge(:number_means_enter=>true)
    end

  end

  def self.traverse tree, options={}, &block
    branch, indent = [], 0
    tree.split("\n").each do |line|
      # Ol << "line: #{line.inspect}"
      line_indent = line[/^ */].length / 2
      line.strip!

      branch[line_indent] = line
      if line_indent < indent
        branch = branch[0..line_indent]
      end


      branch_dup = branch.dup

      if options[:no_bullets]
        branch_dup.map!{|o| o.sub /^[+-] /, ''}
      end

      if options[:flattened]
        flattened = branch_dup.dup
        flattened.map!{|o| o.sub /^[+-] /, ''} if ! options[:no_bullets]   # Might have side-effects if done twice
        flattened = flattened.join('')#.gsub(/[.:]/, '')   # Why were :'s removed??
        block.call branch_dup, flattened
      else
        block.call branch_dup
      end

      indent = line_indent
    end
  end

  # Insert section from a file under it in tree
  def self.enter_under
    Line.beginning
    path = Tree.construct_path  # Get path
    path.sub!(/\|.+/, '')  # Remove file
    path = Bookmarks.expand(path)

    # Cut off indent and pipe (including following space)
    Line.value =~ /(^ +)\| (.+)/
    quote_indent, line = $1, $2

    if line =~ /^> /   # If heading in a notes file
      # Go through lines in file until end of section
      matches = ""
      found_yet = false
      IO.foreach(path) do |l|
        l.sub!(/[\r\n]+$/, '')
        l.gsub!("\c@", '.')   # Replace out characters that el4r can't handle
        # Swallow up until match
        if !found_yet
          found_yet = l == line
          next
        end
        # Grab rest until another pipe
        break if l =~ /^\> /

        l = " #{l}" unless l.empty?
        matches << "#{quote_indent}  |#{l}\n"
      end

      # Insert and start search
      Tree.insert_quoted_and_search matches

    else  # Otherwise, grab by indent

      # Go through lines in file until we've found it
      indent = line[/^\s*/].gsub("\t", '        ').length
      matches = ""
      found_yet = false
      IO.foreach(path) do |l|
        l.sub!(/[\r\n]+$/, '')
        l.gsub!("\c@", '.')   # Replace out characters that el4r can't handle
        # Swallow up until match
        if !found_yet
          found_yet = l == line
          next
        end
        # Grab rest until not indented less

        current_indent = l[/^\s*/].gsub("\t", '        ').length

        break matches.<<("#{quote_indent}  |\n") if line.blank?

        break if current_indent <= indent

        l = " #{l}" unless l.blank?
        matches << "#{quote_indent}  |#{l}\n"
      end

      # Insert and start search
      Tree.insert_quoted_and_search matches
    end
  end



  def self.dotify! tree, target
    target_flat = target.join "/"

    self.traverse(tree, :flattened=>1, :no_bullets=>1) do |branch, path|

      match = self.target_match path, target_flat

      if match == :same || match == :longer
        indent = branch.length - 1

        # If last item in path has period and target doesn't
        if branch[indent] =~ /^\./ && target[indent] !~ /^\./
          # Add period to nth item in target
          target[indent].sub! /^/, '.'
        end
      end
    end

    # Optimization
      # If last path wasn't match and indent is lower than last path, we won't match

  end


  def self.target_match path, target

    pi, ti = 0, 0

    while true
      pathi, targeti = path[pi], target[ti]

      if pathi.nil? || targeti.nil?
        return :same if pathi.nil? && targeti.nil?   # Handles a, a and a/, a/
        return :same if pathi.nil? && target[ti+1].nil? && targeti.chr == "/"   # Handles a, a/
        return :same if targeti.nil? && path[pi+1].nil? && pathi.chr == "/"   # Handles a/, a

        return :shorter if pathi && (pathi.chr == "/" || path[pi-1].chr == "/" || pi == 0)
        return :longer if targeti && (targeti.chr == "/" || target[ti-1].chr == "/" || ti == 0)
        return nil   # At end of one, but no match
      end

      # If chars equal, increment
      if pathi == targeti
        pi += 1
        next ti += 1
      elsif pathi.chr == "." && (path[pi-1].chr == "/" || pi == 0) && (target[ti-1].chr == "/" || ti == 0)
        next pi += 1
      end

      break   # Not found
    end

    nil
  end


  # Use instead of .leaf when you know all but the root is part of the leaf
  # (in case there are slashes).
  def self.rest path
    path = self.rootless path
    path = "|#{path}" unless path =~ /^(\||$)/

    self.leaf(path)
  end


  # Copy children from treeb to treea, but only for branches in treea where children were removed.
  def self.restore treea, treeb

    treea, treeb = TreeCursor.new(treea), TreeCursor.new(treeb)

    # For each leaf in A
    treea.each do
      next unless treea.at_leaf?   # We only care about leafs

      treeb.select treea.line   # Find branch in B
      next if treeb.at_leaf?   # Skip if no children children

      treea << treeb.under   # Grab them and move into A
    end

    treea.txt
  end

  def self.collapse
    # If at root or end of line, go to next
    Line.next if Line !~ /^ / || View.cursor == Line.right
    Move.to_end -1
    left = View.cursor
    $el.skip_chars_forward(" \n+-")
    View.delete left, View.cursor

    Move.to_end
    left, right = View.paragraph :bounds=>true, :start_here=>true

    $el.indent_rigidly View.cursor, right, -2
  end

  def self.branch
    txt = Tree.siblings(:all=>1).join("\n")+"\n"
    txt.gsub! /^\| ?/, ''
    txt
  end

  def self.unset_env_vars
    ENV['noslash'] = nil
  end

  def self.output_and_search block_or_string, options={}
    line=options[:line]

    buffer_orig = View.buffer
    orig = Location.new
    orig_left = View.cursor
    error_happened = nil

    self.unset_env_vars

    output =
      if block_or_string.is_a? String
        block_or_string
      else   # Must be a proc
        begin
          result = block_or_string.call line
          result
        rescue Exception=>e
          message = e.message

          error_happened = true
          CodeTree.draw_exception e, Code.to_ruby(block_or_string)
        end
      end
    return if output.blank?

    # TODO: move some of this crap into the else block above (block_or_string is proc)

    buffer_changed = buffer_orig != View.buffer   # Remember whether we left the buffer

    ended_up = Location.new
    orig.go   # Go back to where we were before running code

    # Move what they printed over to left margin initally, in case they haven't
    output = TextUtil.unindent(output) if output =~ /\A[ \n]/
    # Remove any double linebreaks at end
    output = CodeTree.returned_to_s output

    return View.flash $1 if output =~ /^\.flash (.+)/

    output.sub!(/\n\n\z/, "\n")
    output = "#{output}\n" if output !~ /\n\z/

    if $menu_resize
      height = output.count("\n") + 5
      height = 60 if height > 60
      View.height = height
      $menu_resize = false
    end

    if options[:just_return]
      return output
    end

    # Add slash to end of line if not suppressed, and line isn't a quote
    if !options[:no_slash] && ! ENV['noslash'] && Line !~ /(^ *\||\/$)/
      Line << "/"
    end
    indent = Line.indent
    Line.to_left
    Line.next
    left = View.cursor
    output.gsub!(/^/, "#{indent}  ")

    View.<< output, :utf8=>1
    right = View.cursor

    orig.go   # Move cursor back  <-- why doing this?
    ended_up.go   # End up where script took us

    moved = View.cursor != orig_left

    # Move to :line_found if any
    if options[:line_found] && options[:line_found] > 0
      Line.next(options[:line_found])
      Color.colorize :l
    end

    if !error_happened && !$xiki_no_search && !buffer_changed && !moved
      Tree.search_appropriately left, right, output, options
    elsif ! options[:line_found]
      Line.to_beginning :down=>1
    end
    output
    #     nil
  end

  def self.closest_dir
    dir = Xiki.trunk.reverse.find{|o| FileTree.matches_root_pattern? o}
    return nil if dir.nil?

    File.expand_path dir
  end

  # Tree.slashless("hey/you/").should == "hey/you"
  def self.slashless txt
    txt.sub /\/$/, ''
  end


  def self.children tree=nil, target=nil

    return self.children_at_cursor(tree) if tree.nil? || tree.is_a?(Hash)  # tree is actually options

    target = target.join("/") if target.is_a? Array
    target = "" if target == nil || target == "/"   # Must be at root if nil
    tree = TextUtil.unindent tree

    target.sub!(/^\//, '')
    target.sub!(/\/$/, '')

    found = nil
    result = ""

    found = -1 if target.empty?

    self.traverse tree, :flattened=>1 do |branch, path|

      if ! found
        target_match = Tree.target_match path, target

        if target_match == :shorter || target_match == :same
          found = branch.length - 1  # Remember indent
        end
      else
        current_indent = branch.length - 1
        # If found and still indented one deeper
        if current_indent == found + 1
          item = branch[-1]
          item.sub!(/^- /, '+ ') if item =~ /\/$/
          item.sub!(/^([+-] )?\./, "\\1")
          result << "#{item}\n"  # Output
        else  # Otherwise, stop looking for children if indent is less
          found = nil if current_indent <= found
        end
      end

    end
    result.empty? ? nil : result.gsub(/^$/, '|')
  end

  def self.children_at_cursor options={}
    options ||= {}
    child = self.child
    return nil if child.nil?   # Return if no child

    indent = Line.indent(Line.value(2)).size

    # :as_hash isn't used anywhere
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

    if options[:string]
      return children.join("\n")+"\n"
    end

    children
  end

  def self.children?
    # Whether next line is more indented
    Line.indent(Line.value(2)).size >
      Line.indent.size
  end

  def self.child
    next_line = Line.value 2
    # If indent is one greater, it is a child
    if Line.indent.size + 2 == Line.indent(next_line).size
      return Line.without_label(:line=>next_line)
    end
    nil
  end

end
