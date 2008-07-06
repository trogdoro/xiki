require 'styles'
require 'line'
require 'view'
require 'net/http'
require 'uri'
require 'cursor'

# Draws a tree from a dir structure and lets you incrementally search in the tree.
# Usage (user):
#   - Type C-x C-t
#     - A tree will be drawn in a new buffer
#       - Representing the directory structure in the current dir
#   - Type Enter to open a file
#     - Navigate within the buffer
#     - Then press Enter when you're on the file you wish to open
# Usage (api):
#  - Call this to draw a tree from the current directory
#    - TreeLs.ls
class TreeLs
  extend ElMixin
  include ElMixin

  @@search_going_or_interrupted = false
  @@one_view_in_bar_by_default = false

  CODE_SAMPLES = %q<
    # || Lets you navigate your filesystem as a tree.
    # Do a C-. on this line:
    /

    # || Overview of keys
    # - Type letters to incrementally search in filenames
    # - Enter: stops searching
    # - Tab, period, or slash: drills into tree or opens file
    #   - tab collapses tree as it goes
    #   - period doesn't collapse
    #   - slash adds dir to same line

    # Tree of 3 most recent open files
    puts TreeLs.paths_to_tree elvar.recentf_list.to_a[0..2]

  >

  # Call this method from your init.rb to use the default key shortcuts.
  def self.keys
    Keys.XT { TreeLs.ls }
  end

  def initialize
    @res = ""
    @list = []
    @file_regex = nil
  end
  attr :res
  attr :list
  attr_accessor :file_regex

  # Change dirs into spaces, etc
  def clean path, indent=0
#     path.gsub(/.+?\//, "  ")
    path = path.gsub(/.+?\//, "  ")
    indent == 0 ?
      path :
      path.sub(/^#{indent}/, '')
  end


  # Recursively draws out tree
  def traverse path

    entries = Dir["#{path}/*"].entries.sort

    # Process dirs
    entries.each{ |f|
      next unless FileTest.directory?(f)
      @res += "#{clean f}/\n"
      traverse f
    }

    # Process files
    entries.each{ |f|
      next unless FileTest.file?(f)
      @res += "#{clean f}\n"
    }

  end

  def self.grep dir, regex, options={}
    raw = options[:raw]

    # Expand out bookmark (if there is one)
    dir = Bookmarks.expand(dir)
    dir = Bookmarks.dir_only dir  # Cut off file (if there is one)
    dir.sub!(/\/$/, '')  # Remove slash from end

    # Turn regex into a regex, if a string
    regex = Regexp.new(regex, Regexp::IGNORECASE) if regex.is_a? String

    unless raw
      View.bar if options[:bar]
      View.to_buffer "*tree grep";  View.dir = dir
      View.clear;  notes_mode
    end

    @@indent_count = dir.count('/') - 1
    @@indent = "  " * @@indent_count
    t = self.new

    files = options[:files]
    if files
      files = Regexp.new(files, Regexp::IGNORECASE) if files.is_a? String
      t.file_regex = files
    end
    t.list << "#{dir}/"
    t.grep_inner dir, regex

    list = t.list
    self.clear_empty_dirs! list
    if raw
      return list
    end
    if list.size == 0
      insert "| Note\n- ~No Results Found~\n"
    else
      insert list.join("\n") + "\n"
    end

    View.to_top
    highlight_regexp(regex, :ls_quote_highlight) if regex
    re_search_forward "|"

  end

  def self.grep_one_file(f, regex, indent)
    result = []
    IO.foreach(f) do |line|
      line.sub!(/[\r\n]+$/, '')
      if regex
        next unless line =~ regex
      end
      result << "#{indent}|#{line}"
    end
    result
  end

  def grep_inner path, regex

    path.sub!(/\/$/, '')
    entries = Dir["#{path}/*"].entries.sort

    # Process dirs
    entries.each{ |f|
      next unless FileTest.directory?(f)
      @list << "#{clean(f, @@indent)}/"
      grep_inner f, regex
    }

    indent = nil

    # Process files
    entries.each do |f|
      next unless FileTest.file?(f)
      # If matching filename, skip if no match
      if file_regex
        stem = f[/[^\/]+$/]
        next unless stem =~ file_regex
      end

      indent = "  " * (f.count('/') - @@indent_count) unless indent

      if regex
        result = TreeLs.grep_one_file(f, regex, indent)   # Search in file contents

        if result.size > 0   # Add if any files were found
          @list << clean(f, @@indent)
          @list += result
        end
      else
        @list << clean(f, @@indent)
      end
    end

  end

  # Does ls in current buffer, without making any modifications to the environment
  def self.ls_here dir
    Dir.chdir dir
    t = self.new
    t.traverse "."
    insert "#{dir}\n"
    insert t.res
    #select_next_file
  end


  def self.define_styles

    Styles.define :diff_line_number,
      :fg => "aaa",
      :bold => true,
      :size => "-2"

    Styles.define :diff_red,
      :bg => "ffdddd", :fg => "cc4444",
      :size => "-1"
    Styles.define :diff_green,
      :bg => "ddffcc", :fg => "337744",
      :size => "-1"

    if Styles.inverse
      Styles.define :diff_red,
        :bg => "440000", :fg => "ff6666"
      Styles.define :diff_green,
        :bg => "113300", :fg => "44dd33"
    end

    # dir/
    Styles.define :ls_dir,
      :fg => "99e",
      :face => "verdana",
      :size => "-2",
      :bold => true

    #   | Quoted text
    Styles.define :ls_quote,
      :size => "-1",
      :fg => "88c"

    #   001| Quoted text lines
    Styles.define :ls_quote_line_number,
      :size => "-4",
      :fg => "eee"

    # Highlight in search
    Styles.define :ls_quote_highlight,
      :size => "-1",
      :bg => "ffff44",
      :fg => "666666"
  end

  def self.apply_styles
    el4r_lisp_eval "(setq font-lock-defaults '(nil t))"

    # With numbers
    Styles.apply("^ +\\(:[0-9]+\\)\\(|.*\n\\)", nil, :ls_quote_line_number, :ls_quote)
    #   +|... diffs
    Styles.apply("^ +\\(\+|.*\n\\)", nil, :diff_green)
    Styles.apply("^ +\\(-|.*\n\\)", nil, :diff_red)
    Styles.apply("^ +\\(:[0-9]+\\)$", nil, :diff_line_number)

    # Dir line
    #Styles.apply('^\\([A-Za-z]\\)$', nil, :ls_dir)  # Most dirs
    #Styles.apply("\\([A-Za-z][^:\n]+/\\)$", nil, :ls_dir)  # Most dirs
    Styles.apply('^[ +-]*\\(.+/\\)$', nil, :ls_dir)  # Most dirs
    #Styles.apply('^[ -]*\\([ a-zA-Z0-9\/_\.$-]*\\w/\\)$', nil, :ls_dir)  # Most dirs
    Styles.apply('^ *\\(//?\\)$', nil, :ls_dir)  # /
    Styles.apply('^ *\\(\./\\)$', nil, :ls_dir)  # ./

    #   |... lines
    Styles.apply("^ +\\(|.*\n\\)", nil, :ls_quote)
  end

  # Define key
  def self.define_keys

#     make_local_variable :tab_width
#     elvar.tab_width = 3

#     elvar.treels_mode_keymap = make_sparse_keymap
#     use_local_map elvar.treels_mode_keymap

#     # Previous file
#     Keys.P(:treels_mode_keymap) do
#       beginning_of_line
#       TreeLs.select_previous_file
#     end
#     # Next file
#     Keys.N(:treels_mode_keymap) do
#       end_of_line
#       TreeLs.select_next_file
#     end

#     # Enter should open the file
#     Keys.M(:treels_mode_keymap) do
#       TreeLs.open
#     end

  end

  # Mapped to Enter when on a treels buffer.  Opens file curor is on in the tree.
  # It assumes the path to a dir is on the current line.
  def self.construct_path options={}
    path = []
    orig = point

    # Do until we're at a root dir
    line = Line.value
    while(! self.is_root?(line) )
      break unless line =~ /^ /  # If at left margin, assume it's the parent
      Line.value =~ /^  ( *)(.+)/
      spaces, item = $1, $2
      item = self.clean_path item unless options[:raw]
      path.unshift item  # Add item to list
      search_backward_regexp "^#{spaces}[^\t \n]"
      line = Line.value
    end
    # Add root of tree
    root = Line.value.sub(/^ +/, '')
    root = self.clean_path(root) unless options[:raw]
    path.unshift root
    #path.unshift Line.value.sub(/^ +/, '')

    goto_char orig
    if options[:indented]
      indentify_path path
    elsif options[:list]
      path
    else
      path.join
    end

  end

  def self.clean_path path
    path.sub!(/^ *- .+: /, "- ")  # Treat "- foo:" text as comments (ignore)
    path.sub!(/^([^|\n-]*)##.+/, "\\1")  # Ignore "##"
    path.sub!(/^([^|\n-]*)\*\*.+/, "\\1")  # Ignore "\*\*"
    path
  end

  # Open the line in the tree that the cursor is on.  This is probably
  # be mapped to C-. .
  # TODO: remove ignore_prefix, and just use Keys.clear_prefix
  def self.open ignore_prefix=nil
    path = construct_path

    # Split off |... if it's there (search string)
    path =~ /(.*?)-?\+?\|(.*)/
    path, search_string = $1, $2 if $2
    # Pull number off end of path if there

    path =~ /(.+):(\d+)$/
    path, line_number = $1, $2 if $2

    path = Bookmarks.expand(path)

    remote = self.is_remote?(path)
    unless remote
      path = File.expand_path(path)
    end

    # If 0, enter by level
    case Keys.prefix
    when 8
      Keys.clear_prefix
      if search_string  # If quote, enter lines under
        self.enter_under
      else  # If file, enter all lines
        self.enter_lines(//)
      end
      return
    when 1
      # Implement: grab code after delete runs
    when 0
      return self.drill
    end

    # If numeric prefix, jump to nth window
    if (! ignore_prefix) and Keys.prefix
      View.to_nth(Keys.prefix - 1)
    # Otherwise, move to after bar if the bar is open
    else
      #View.to_after_bar
      # Only go to bar if after bar (for example, after OE)
      View.to_after_bar if View.in_bar?
    end

    # Open or go to file
    if remote
      View.to_buffer path
      set_visited_file_name path
      elvar.buffer_auto_save_file_name = nil
      # Get text from server and insert
      insert self.remote_file_contents(path)
    else
      Location.go path
    end

    if line_number  # If line number, go to it
      goto_line line_number.to_i
    elsif search_string  # Else, search for |... string if it passed
      Move.top
      found = search_forward_regexp("^#{regexp_quote(search_string)}$", nil, true)

      # If not found, search for just the string itself
      unless found
        Move.top
        search_forward_regexp("#{regexp_quote(search_string)}", nil, true)
      end

      # If not found, search for just the string itself
      unless found
        Move.top
        search_forward_regexp("#{regexp_quote(search_string.strip)}")
      end

      beginning_of_line
      recenter 0
    end
  end

  # Incremental search
  def self.search options={}
    Cursor.remember :before_tree_ls
    Cursor.blue
    @@search_going_or_interrupted = true

    # Make cursor blue
    recursive = options[:recursive]
    left = options[:left] || point_min
    right = options[:right] || point_max

    pattern = ""
    lines = buffer_substring(left, right).split "\n"
    message "Show files matching: "
    ch_raw = nil
    begin
      ch_raw = read_char
    rescue Exception => e  # Assume it was a mouse event
      Cursor.restore :before_tree_ls
      return
    end
    ch = char_to_string(ch_raw)

    # While narrowing down list (and special check for C-.)
    while (ch =~ /[\\() ~">a-zA-Z!_,~-]/ && ch_raw != 67108910) ||
        (recursive && ch_raw == 2 || ch_raw == 6)
      # Slash means enter in a dir
      break if recursive && ch == '/'
      if ch == ','
        pattern = ''
      elsif ch_raw == 2  # C-b
        while(Line.previous == 0)
          next if Line.matches(/\/$/)  # Keep going if line is a dir
          Line.to_words
          break  # Otherwise, stop
        end
      elsif ch_raw == 6  # C-f
        while(Line.next == 0)
          next if Line.matches(/\/$/)  # Keep going if line is a dir
          Line.to_words
          break  # Otherwise, stop
        end
      else
        if ch == "\\"  # If escape, get real char
          ch = char_to_string(read_char)
        end
        pattern << Regexp.quote(ch)

        if pattern =~ /[A-Z]$/   # If upper, remove any lower
          pattern.sub!(/^[a-z]+/, '')
        elsif pattern =~ /[a-z]$/   # If lower, remove any upper
          pattern.sub!(/^[A-Z]+/, '')
        end

        acronym = acronym_regexp(pattern)
        delete_region left, right

        # Replace out lines that don't match (and aren't dirs)
        regexp = Keys.prefix == 5 ?
          "#{acronym}" :
          "#{acronym}|#{pattern}"
        regexp = "\\/|#{regexp}" if recursive
        regexp = /#{regexp}/i

        lines_new = nil
        if pattern =~ /[A-Z]$/   # If upper, search in directory
          lines_new = search_dir_names(lines, /#{pattern}/i)
        else
          lines_new = lines.grep(regexp)
        end
        # If search not found, don't delete all
        lines = lines_new unless lines_new.size == 0

        # Remove dirs with nothing under them
        self.clear_empty_dirs! lines if recursive

        # Put back into buffer
        insert lines.join("\n") + "\n"
        right = point

        # Go to first file
        goto_char left

        # Move to first file
        recursive ?
          self.select_next_file :
          Move.to_line_text_beginning

      end
      message "Show files matching: %s", pattern
      begin
        ch_raw = read_char
      rescue Exception => e  # Assume it was a mouse event
        Cursor.restore :before_tree_ls
        return
      end

      ch = char_to_string(ch_raw)
    end

    # Exiting, so restore cursor
    Cursor.restore :before_tree_ls

    # Special check for C-.
    ch = "enter" if ch == "\C-m"
    ch = "period" if ch == "." || ch_raw == 67108910
    ch = "delete" if ch_raw == 127

    # Options during search
    case ch  # Do option based on last char, or run as command
    when "enter"  # Enter key
      # Do nothing (end search)
    when "\C-a"
      Line.to_left
    when "\C-e"
      Line.to_right
#       # Grab text
#       text = buffer_substring(left, right)
#       # Delete text
#       # Delete tree above
#       while(! self.is_root?(Line.value) )
#         Line.previous
#       end
#       delete_region Line.left, right

#       # Erase |'s and put back
#       insert text.gsub(/^ +\|/, '')

    when "period"  # If C-., go in but don't collapse siblings
      Keys.clear_prefix
      LineLauncher.launch

    when "\t"  # If tab, hide siblings and go in
      delete_region(Line.left(2), right)
      Keys.clear_prefix
      LineLauncher.launch
      #self.expand_or_open
      #Line.to_words
    when "delete"  # If backspace, cancel last action and exit
      goto_char left
      Line.next if options[:recursive]
      delete_region(point, right)
      Line.previous
      Line.to_words
      self.search(:left => Line.left, :right => Line.left(2))
    when ch == "/"  # If /, run this line ???
      Keys.prefix = 2
      delete_region(Line.left(2), right)
      self.open

    when ";"  # Show methods, or outline
      delete_region(Line.left(2), right)  # Delete other files
      if Line.matches(/\.rb$/)
        self.enter_lines(/^ *(def|class|module|it|describe) /)
      else
        self.enter_lines(/^\| /)
      end

    when "#"  # Show ##.../ search
      self.stop_and_insert left, right, pattern
      View.insert TreeLs.indent("##/", 0)
      View.to(Line.right - 1)

    when "*"  # Show **.../ search
      self.stop_and_insert left, right, pattern
      View.insert TreeLs.indent("**/", 0)
      View.to(Line.right - 1)

    when "8"
      # If a quote, insert lines indented lower
      if Line.matches(/\|/)
        self.enter_under
      elsif Line.matches(/\/$/)  # A Dir, so do recursive search
        delete_region(Line.left(2), right)
        self.dir_recursive
        return @@search_going_or_interrupted = false
      else  # A file, so enter lines
        delete_region(Line.left(2), right)
        self.enter_lines(//)  # Insert all lines
      end
    when "9"  # Open in bar
      delete_region(Line.left(2), right)  # Delete other files
      View.bar
      Keys.clear_prefix
      # Expand or open
      self.expand_or_open

    when "1".."7"
      Keys.clear_prefix
      n = ch.to_i

      # Pull whole string out
      lines = buffer_substring(left, right).split "\n"
      delete_region left, right

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
        insert filtered.join("\n") + "\n"
        right = point

        # Go to first file and go back into search
        goto_char left
        self.select_next_file

        # Todo: merge this and the following .search
        self.search(:recursive => true, :left => Line.left, :right => Line.left(2))
      else

        nth = lines[ch.to_i - 1]
        insert "#{nth}\n"
        previous_line
        if options[:number_means_enter]  # If explicitly supposed to enter
          LineLauncher.launch
        elsif Line.matches(/\/$/)  # If a dir, go into it
          self.dir
        else
          Move.to_line_text_beginning
          # Get back into search, waiting for input
          self.search(:left => Line.left, :right => Line.left(2))
        end

      end
      #self.open

    when "\C-s"
      isearch_forward

    when "/"  # If slash, append selected dir to parent dir

      # If line is a dir, do tree-like tab
      if Line.value =~ /\/$/

        delete_region(Line.left(2), right)  # Delete other files
        delete_horizontal_space
        delete_backward_char 1

        # Open if a dir
        Line.matches(/\/$/) ?
          self.expand_or_open :
          Line.end

      elsif Line.value =~ /"/  # If it contains quotes, "tab" to field
        line = Line.delete true
        insert line.sub(/".+"/, '""')
        Search.forward('"')
        # TODO make codetree not move cursor back to starting point
      end

    when "="  # Drill into the file
      dir = self.construct_path  # Expand out ~
      View.open(dir)

    when "0"  # Drill into the file
      delete_region(Line.left(2), right)  # Delete other files
      self.drill

    else
      command_execute ch
    end
    @@search_going_or_interrupted = false


  end

  # Goes through files in reverse order and deletes empty dirs
  def self.clear_empty_dirs_string tree
    lines = tree.split("\n")
    self.clear_empty_dirs! lines
    lines.join("\n")
  end

  # Goes through files in reverse order and deletes empty dirs
  def self.clear_empty_dirs! lines
    file_indent = 0
    i = lines.length
    while( i > 0)
      i -= 1
      l = lines[i]
      l =~ /^( +)/
      spaces = $1 ? $1.length : 0
      # If dir
      if l =~ /\/$/ && l !~ /\|/
        # If lower than indent, decrement indent
        if spaces < file_indent
          file_indent -= 2
        # Else, delete
        else
          lines.delete_at i
        end
      # If file
      else
        # Set indent
        file_indent = spaces
      end
    end
  end

  def self.select_next_file
    search_forward_regexp /[^\/]$/
    beginning_of_line
    skip_chars_forward " "
  end

  def self.select_previous_file
    search_backward_regexp /[^\/]$/
    beginning_of_line
    skip_chars_forward "\t"
  end

  # Draw tree, use dir of bookmark from user input
  def self.ls options={}

    dir = options[:dir]
    # If no dir, do tree in current dir
    dir ||= elvar.default_directory

    # If to be in bar, go to the tree file in the bar
    if options[:open_in_bar]
      # Open tree
      self.open_in_bar :ignore_prefix
      # If not on a blank line, go to the top
      View.to_top unless Line.matches(/^$/)
      # If on line, add another blank
      unless Line.matches(/^$/)
        insert "\n\n";  backward_char 2
      end
    end

    dir = File.expand_path(dir)  # Expand out ~
    # If file, back up to dir
    dir = Bookmarks.dir_only dir unless File.directory?(dir)

    # If dir or recursive
    if File.directory?(dir) || options[:recursive]
      dir.sub!(/([^\/])$/, "\\1/")  # Add slash on end if not there
    end

    name = dir[/.+\/(.+)\//, 1]

    # Don't open new dir if not appropriate
    if options[:open_in_bar] || options[:here]
      # Don't upen buffer
    else
      View.to_buffer "*tree #{name}"
      View.clear
      View.dir = Bookmarks.dir_only dir
      self.apply_styles
    end

    # If recursive
    if options[:recursive]
      left = point
      self.ls_here dir  # Draw actual tree
      right = point
      goto_char left
      self.select_next_file
      # Start incremental search
      self.search(:recursive => true, :left => left, :right => right)
      #self.draw(dir)
    else
      # Insert with linebreak if file
      if File.file?(dir)
        #insert dir
        insert self.filename_to_next_line(dir)  # Add linebreak before filename
        #insert dir.sub(/(.+)\//, "\\1/\n  ")  # Add linebreak before filename
        open_line 1
        #self.search
        Line.to_words
        self.search(:left => Line.left, :right => Line.left(2))
        return
      end

      insert "#{dir}\n"
      previous_line
      self.dir  # Draw actual tree
    end
  end


  def self.open_in_bar ignore_prefix=nil

    # If numeric prefix, open nth thing in tree
    if Keys.prefix and !(Keys.prefix_u) and !(ignore_prefix)
      # Remember original view
      start = selected_window
      # Open tree (ignoring prefix)
      self.open_in_bar :ignore_prefix
      # Find nth file in tree
      beginning_of_buffer
      Keys.prefix.times do
        re_search_forward "^  +[a-zA-Z0-9_.-]+$"
      end
      # Go to next line if comment
      next_line if Line.next_matches(/^ *\|/)
      Move.to_line_text_beginning
      self.open :ignore_prefix
      #self.open
  #    View.bar
      return
    end

    # If already open, just go there
    if View.bar?
      select_window(View.first)
      #return
    else
      View.bar
    end
    find_file Bookmarks.expand("$t")

    two_views_in_bar = Keys.prefix_u
    two_views_in_bar = ! two_views_in_bar if @@one_view_in_bar_by_default

    unless two_views_in_bar  # Unless u prefix, open $tl as well (under bar)
      # If 2nd view is $tl, just go to it
      if buffer_file_name( window_buffer( View.list[1] ) ) == Bookmarks["$f"]
        View.to_nth 1
      end

      # If 2nd view isn't at left margin, open 2nd view
      if View.left_edge(View.list[1]) != 0
        View.create
      end

      # Open $tl in 2nd view
      View.to_nth 1
      find_file Bookmarks["$f"]
      View.to_nth 0
    end
  end

  # Creates tree snippet of text in file
  def self.snippet str=nil
    str ||= View.selection
    # Remove linebreak from end
    str = str.sub(/\n\z/, "")
    if buffer_file_name
      "#{elvar.default_directory}\n  #{file_name_nondirectory(buffer_file_name)}\n" + str.gsub(/^/, "    |")
    else
      "- From #{buffer_name}:\n" + str.gsub(/^/, "    #")
    end
  end

  # Recursively display dir in tree  # Insert dir contents at point (usually in existing tree)
  def self.dir
    beginning_of_line
    Dir.chdir elvar.default_directory
    if Keys.prefix == 8
      self.dir_recursive
    else
      self.dir_one_level
   end
  end

  # Insert all files in dirs within a dir, and allow user to
  # incremental search therein.
  def self.dir_recursive
    beginning_of_line
    line = Line.value
    left = point
    indent = line[/^ */] + "  "  # Get indent
    dir = self.construct_path

    Line.next

    # Get tree
    t = self.new
    dir.sub!(/\/$/, '')
    t.traverse Bookmarks.expand(dir)

    # Adjust indent
    result_indent = t.res[/^ */]
    insert t.res.gsub(/^#{result_indent}/, indent)

    right = point
#    insert "-"

    goto_char left
#    isearch_forward
    self.select_next_file
    self.search(:recursive => true, :left => left, :right => right)
  end

  def self.dir_one_level
    Line.start
    # Get dir, then insert each line
    line = Line.value
    indent = line[/^ */] + "  "  # Get indent
    dir = Bookmarks.expand(self.construct_path)

    remote = self.is_remote?(dir)
    unless remote
      dir = File.expand_path(dir)
      # If C-2, just open in dired
      if Keys.prefix == 2
        # TODO: Open in 1st window
        View.to_after_bar
        find_file dir
        return
      end
    end

#     insert dir
    # Get dirs and files in it
    dirs, files = self.files_in_dir(dir)

    # Change path to proper indent
    dirs.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}\\1/")}

    # Change path to proper indent
    files.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}\\1")}

    next_line
    beginning_of_line
    left = point

    insert (dirs + files).join("\n") + "\n"
    right = point
    goto_char left

    Move.to_line_text_beginning

    # Kick off hidesearch (deleting)
    self.search(:left => left, :right => right)

  end

#   # Does search on the top-level dirs/files of one dir.
#   # Called by dir_one_level, among others.
#   def self.flat_search options={}

#   end

  # Drill into a file, showing lines one indent level deeper
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
#         next if line =~ /^\s*$/  # Skip blank lines

        # Exit if indented at same level (unless blank or comment)
        if((Line.indent(line).length <= indent_after_bar.length) &&
           (! (line =~ /^\s*$/)) &&
           (! (line =~ /^\s*#/))
           )
          break
        end

        # Skip unless indented 2 later
        next unless Line.indent(line).length == 2 + indent_after_bar.length

        matches << "#{indent}|#{line}\n"
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
        matches << "#{indent}  |#{line}\n"
        this_was_used = true
      end
      self.insert_quoted_and_search matches
      # TODO Search in result

      # TODO Do some checking for duplicates
    end

  end

  def self.enter_snippet
    start = selected_window
    snippet = self.snippet
    self.open_in_bar

    loc = Location.new
    Line.to_left
    #orig = point

    # If at indented line
    while(Line.matches(/^ /))
      Line.previous
    end

    # Check to see if it's the same file
    if snippet[/.+\n.+\n/] == buffer_substring(point, Line.left(3))
      Line.next 2
      insert "#{snippet.sub(/.+\n.+\n/, '')}\n"
    # Check to see if it's just in same dir
    elsif snippet[/.+\n/] == buffer_substring(point, Line.left(2))
      Line.next
      insert "#{snippet.sub(/.+\n/, '')}\n"
    else
      insert "#{snippet}\n"
    end

    loc.go
    #goto_char orig

    select_window(start)
  end

  # Enter tree of selection at the spot (saved by AS).
  def self.enter_at_spot
    snippet = self.snippet
    Location.jump("0")
    insert "#{snippet}\n"
    Location.save("0")  # So any more will be entered after
  end

  # Enter what's in clipboard with | to on the left margin, with appropriate indent
  def self.enter_quoted

    Line.to_left
    clip = Clipboard.get(0, :add_linebreak => true)

    # If current line is path
    if Line.matches(/\/$/)
      dir = self.construct_path
      t = Clipboard.get("=")
      ml ''
      ml dir
      ml t
      t = t.gsub(/^#{dir}/, '')
      t.gsub!(/\A\n/, '')
      ml t
      Line.next
      View.insert "#{t}\n".gsub(/^/, '  ')
      return
    end


    # If C-u or whole thing is quoted already
    if Keys.prefix_u || clip =~ /\A  +[-+]?\|/
      # Unquote
      clip = clip.grep(/\|/).join()
      return insert(clip.gsub(/^ *[-+]?\|/, ""))
    end

    # If empty line, just enter tree
    if Line.blank?
      start = point
      t = Clipboard.get("=")
      # Indent prefix spaces, or 2
      indent = Keys.prefix || 0
      t = t.gsub(/^/, " " * indent)
      insert t
      set_mark(Line.left(2))
      goto_char start
      return
    end

    # Get current indent
    indent = Line.indent
    on_comment_line = Line.matches /^ +\|/
    next_line

    # Indent one level further unless on comment already
    unless on_comment_line
      indent = "#{indent}  "
    end
    insert clip.gsub(/^/, "#{indent}\|")
  end

  # Remove the following lines indented more than the current one
  def self.kill_under
    kill_this_also = Keys.prefix_u
    orig = point
    line = Line.value.sub('|', '')

    # Get indent of current line
    indent = line[/^ */]

    # Go to next line
    Line.start
    Line.next
    left = Line.left
    while(Line.next == 0)
      # Keep going unless line not indented more
      break unless Line.value.sub('|', '') =~ /^#{indent} /
    end
    delete_region left, Line.left
    goto_char orig
    Line.delete if kill_this_also
  end

  def self.kill_siblings
    # Get indent of current line
    Line.to_left
    indent = Line.indent
    # Remove lines before
    while(true)
      previous_line
      break unless Line.indent == indent
      kill_line
    end

    # Remove lines after
    next_line 2
    while(true)
      break unless Line.indent == indent
      kill_line
    end
    previous_line

  end

  # Expand if dir, or open if file
  def self.expand_or_open
    line = Line.value
    indent = Line.indent
    list = nil
    case line
    when /\*\*|##/  # *foo or ## means do grep
      dir = self.construct_path
      raw = self.construct_path(:raw => true, :list => true)
      files = contents = nil
      case raw.join('')
      when /\*\*(.+)##(.+)/  # *foo means search in files
        files, contents = $1, $2.sub!(/\/$/, '')
      when /\*\*(.+)/  # *foo means search in files
        files = $1
      when /##(.+)/  # ##foo means search in filenames
        contents = $1.sub!(/\/$/, '')
        if raw[-2] =~ /\*\*(.+)/   # If prev is **, use it
          files = $1
        elsif raw[-2] =~ /(.+[^\/])$/   # If prev line is file, just show matches
          contents = Regexp.new(contents, Regexp::IGNORECASE)
          list = TreeLs.grep_one_file(Bookmarks.expand(dir), contents, "  ")
          #files = "^#{$1}$"
        end
        #files = $1 if Line.value(0) =~ /\*\*(.+)/
      end

      files.sub!(/\/$/, '') if files
      #contents.sub!(/\/$/, '') if contents
      options = {:raw => true}
      options.merge!({:files => files}) if files

      unless list   # If not already gotten
        list = self.grep dir, contents, options
        list.shift  # Pull off first dir, so they'll be relative
      end
      Line.to_next
      left = point
      tree = list.join("\n") + "\n"
      insert tree.gsub(/^/, indent)
      right = point
      goto_char left
      #Move.to_line_text_beginning
      if Line.matches(/^\s*$/)  # Do nothing
      elsif Line.matches(/^\s+\|/)
        self.search :left => left, :right => right
      else
        Move.to_junior
      end

    when /\/$/  # foo/ is a dir
      self.dir
    else
      self.open
    end
  end

  # Grabs matching lines in file and starts hide search
  def self.enter_lines pattern=nil
    unless pattern
      if Line.blank?  # If blank line, get bookmark and enter into current file
        #dir = Bookmarks.input(:prompt => "Enter bookmark from which to enter outline: ")
        History.open_current :enter_here => true, :prompt_for_bookmark => true
        return
      end
      if Line.matches(/\.rb$/)
        self.enter_lines(/^ *(def|class|module|it|describe) /)
      elsif Line.matches(/\.js$/)
        self.enter_lines(/^ *(function) /)
      elsif Line.matches(/\.notes$/)
        self.enter_lines(/^\| /)
      else
        # Delegate to 0
      end
      return
    end

    Line.to_left
    path = construct_path  # Get path
    path = Bookmarks.expand(path)
    indent = Line.indent  # get indent

    # Get matches from file
    matches = ""
    IO.foreach(path) do |line|
      line.sub!(/[\r\n]+$/, '')
      next unless line =~ pattern
      matches << "#{indent}  |#{line}\n"
    end
    self.insert_quoted_and_search matches
  end

  def self.insert_quoted_and_search matches
    # Insert matches
    Line.next
    left = point
    insert matches
    right = point
    goto_char left
    Line.to_words
    # Do a search
    self.search(:left => left, :right => right)
  end

  # Insert section from a file under it in tree
  def self.enter_under
    Line.beginning
    path = construct_path  # Get path
    path.sub!(/\|.+/, '')  # Remove file
    path = Bookmarks.expand(path)

    # Cut off indent and pipe
    Line.value =~ /(^ +)\|(.+)/
    quote_indent, line = $1, $2

    # If starts with pipe
    if line =~ /^\|/
      # Go through lines in file until end of section
      matches = ""
      found_yet = false
      IO.foreach(path) do |l|
        l.sub!(/[\r\n]+$/, '')
        # Swallow up until match
        if !found_yet
          found_yet = l == line
          next
        end
        # Grab rest until another pipe
        break if l =~ /^\| /
        matches << "#{quote_indent}|#{l}\n"
      end

      # Insert and start search
      self.insert_quoted_and_search matches

    else  # Otherwise, grab by indent
      # Go through lines in file until we've found it
      indent = line[/^\s*/].gsub("\t", '        ').length
      matches = ""
      found_yet = false
      IO.foreach(path) do |l|
        l.sub!(/[\r\n]+$/, '')
        # Swallow up until match
        if !found_yet
          found_yet = l == line
          next
        end
        #insert l[/^\s*/].gsub("\t", '').length.to_s
        # Grab rest until not indented less
        #Try this: Line.indent(l)
        break if l[/^\s*/].gsub("\t", '        ').length == indent
        matches << "#{quote_indent}|#{l}\n"
      end

      # Insert and start search
      self.insert_quoted_and_search matches
    end
  end

  # Mapped to shortcuts that displays the trees
  def self.launch options={}

    # Get optional input (. means current dir)
    input = Keys.input(:timed => true, :prompt => "Tree_ls in which dir? (enter bookmark): ")

    if input and !(input == ".")  # Do tree in dir from bookmark
      dir = Bookmarks.expand("$#{input}")
      dir = Bookmarks.dir_only dir if options[:recursive]
    else  # If no input, do tree in current dir
      dir = elvar.default_directory
    end

    options.merge!(:dir => dir)

    # If U prefix, open in bar
    if Keys.prefix_u
      self.ls options.merge(:open_in_bar => true)
    # Otherwise, open in new buffer
    else
      self.ls options
    end
  end

  def self.to_parent
    (Keys.prefix || 1).times do
      indent = Line.value[/^  ( *)/, 1]
      search_backward_regexp "^#{indent}[^\t \n]"
      Move.to_line_text_beginning
    end
  end

  # Returns a regexp to match the acronym
  def self.acronym_regexp search
    search.gsub(/([a-zA-Z])/, "[a-z]*[_.]\\1").sub(/.+?\].+?\]/,'^ +')
  end

  def self.is_remote? path
    return path =~ /^\/\//
  end

  # Returns the files in the dir
  def self.files_in_dir dir
    if self.is_remote?(dir)
      self.remote_files_in_dir(dir)
    else
      all = Dir["#{dir}/*"].entries
      dirs = all.select{|i| FileTest.directory?(i)}.sort
      files = all.select{|i| FileTest.file?(i)}.sort
      [dirs, files]
    end
  end

  def self.remote_file_contents dir
    url = self.url_from_path "ls", dir

    Net::HTTP.get URI.parse(url)
  end

  def self.remote_files_in_dir dir
    url = self.url_from_path "ls", dir
    all = Net::HTTP.get URI.parse(url)
    dirs, files = all.split(/-----\n/)
    [(dirs || "").split("\n"), (files || "").split("\n")]
  end

  def self.url_from_path verb, path
    server, path = path.match(/([\w.-]+)(\/.*)/)[1..2]
    "http://#{server}:7433/#{verb}#{path}"
  end

  def self.save_remote
    contents = View.txt
    path = buffer_file_name
    url = self.url_from_path "ls", path

    # Post contents to url
    save_url = URI.parse(url)
    req = Net::HTTP::Post.new(save_url.path)

    Net::HTTP.start(save_url.host, save_url.port) do |http|
      http.post(save_url.path, contents) do |str|
        message str
      end
    end
  end

  def self.indentify_path path
    result = ""
    indent = ""
    path.each do |i|
      result << "#{indent}#{i}\n"
      indent += "  "
    end
    result
  end

  # Takes in paths, constructs tree of them
  def self.paths_to_tree paths
    result = ""
    stack = []
    # For each path
    paths.sort.each do |path|
      # Create list from path
      path = path.sub(/^\//, '')
      split = path.split('/')

      # Pop from stack until path begins with stack
      while(stack.size > 0 && stack != split[0..(stack.size - 1)])
        stack.pop
      end
      # Get remainder of path after stack
      indent = stack.length
      remainder = split[indent..-1]
      remainder.each do |dir|
        result << "/" if indent == 0
        result << ("  " * indent) + dir
        result << "/" unless indent == (split.length - 1)
        result << "\n"
        indent += 1
      end
      stack = split
    end
    result
  end

  def self.tree_to_paths tree
  end

#   def self.list_to_path path
#     path.size == 0 ?
#       '/' :
#       "/#{path.join('/')}/"
#   end

  def self.filename_to_next_line path
    path.sub(/(.+)\//, "\\1/\n  ")  # Add linebreak before filename
  end

  def self.is_root? path
    # It's the root if it matches a pattern, or is at left margin
    path =~ /^ *(\/|\.\/|\$)/ || path !~ /^ /
  end

  def self.create_dir
    make_directory(elvar.default_directory)

    # TODO: if in tree, create dir in tree
    # Construct path
    # Prompt for name
    #Keys.input
    # Create dir (using path and name)
  end

  def self.search_going_or_interrupted
    @@search_going_or_interrupted
  end

  def self.search_going_or_interrupted= to
    @@search_going_or_interrupted = to
  end

  # Indent txt to be one level lower than current line
  def self.indent txt, line=1
    indent = Line.indent(Line.value(line))
    txt.gsub!(/^/, "#{indent}  ")
  end

  def self.one_view_in_bar_by_default= to
    @@one_view_in_bar_by_default = to
  end

  def self.copy_path
    dir = TreeLs.construct_path
    dir = View.file unless dir =~ /^\//
    Clipboard["0"] = dir
  end

private
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

  def self.stop_and_insert left, right, pattern
    goto_char left
    #Line.next if options[:recursive]
    # TODO: delete left if recursive - emulate what "delete" does to delete, first
    pattern == "" ?
      delete_region(point, right) :
      Line.next
    $el.open_line 1
    ControlLock.disable
  end

end
TreeLs.define_styles
