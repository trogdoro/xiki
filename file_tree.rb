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
#    - FileTree.ls
class FileTree
  extend ElMixin
  include ElMixin

  @@one_view_in_bar_by_default = false

  CODE_SAMPLES = %q<
    # || Lets you navigate your filesystem as a tree.
    # Do a C-. on this line:

    # || Overview of keys
    # - Type letters to incrementally search in filenames
    # - Enter: stops searching
    # - Tab, period, or slash: drills into tree or opens file
    #   - tab collapses tree as it goes
    #   - period doesn't collapse
    #   - slash adds dir to same line

    # Tree of 3 most recent open files
    puts FileTree.paths_to_tree elvar.recentf_list.to_a[0..2]

  >

  # TODO
  # - Make search handle trees with multiple roots

  # Call this method from your init.rb to use the default key shortcuts.
  def self.keys
    Keys.XT { FileTree.ls }
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
    entries = Dir.glob("#{View.expand_path(path)}/*", File::FNM_DOTMATCH).
      select {|i| i !~ /\/\.(\.*|svn|git)$/}.
      sort

    # Process dirs
    entries.each{ |f|
      next unless FileTest.directory?(f)
      cleaned = clean f
      @res += "#{cleaned.sub(/(^ *)/, "\\1- ")}/\n"
      traverse f
    }

    # Process files
    entries.each{ |f|
      next unless FileTest.file?(f)
      cleaned = clean f
      @res += "#{cleaned.sub(/(^ *)/, "\\1+ ")}\n"
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
    t.list << "- #{dir}/"
    t.grep_inner dir, regex, :first_time

    list = t.list
    Tree.clear_empty_dirs! list
    if raw
      return list
    end
    if list.size == 0
      View.insert "| Note\n- No Results Found!\n"
    else
      View.insert(list.join("\n") + "\n")
    end

    View.to_top
    highlight_regexp(regex, :ls_quote_highlight) if regex
    re_search_forward "|"
  end

  def self.grep_with_hashes path, regex, prepend='##'

    Search.append_log path, "- #{prepend}#{regex}/"

    View.to_buffer "*tree grep";  View.dir = Files.dir_of(path)
    View.clear; notes_mode

    if File.directory?(path)
      View << "- #{File.expand_path path}/\n  - #{prepend}#{regex}/\n"
    else
      dir, name = path.match(/(.+\/)(.+)/)[1..2]
      View << "- #{File.expand_path dir}/\n  - #{name}\n    - #{prepend}#{regex}/\n"
    end

    View.to_bottom; Line.previous
    self.launch
  end

  def self.grep_one_file(f, regex, indent)
    result = []
    IO.foreach(f) do |line|
      line.gsub!(/[\r\n\c@]+/, '')
      if regex
        next unless line =~ regex
      end
      result << "#{indent}| #{line}"
    end
    result
  end

  def self.outline_search(f, regex, indent)
    result = []
    current_line = Search.outline_goto_once   # If search_outline, we want to put cursor on that line when done
    line_found, matches_count, i = nil, 0, 0
    IO.foreach(f) do |line|
      i+=1
      line.gsub!(/[\r\n\c@]+/, '')

      if current_line && line_found.nil?
        line_found = matches_count if i == current_line
      end

      if regex
        next unless line =~ regex
      end
      result << "#{indent}| #{line}"
      matches_count+=1
    end
    Search.outline_goto_once = line_found
    result
  end

  def self.skip
    @skip || {}
  end

  def grep_inner path, regex, first_time=nil

    path.sub!(/\/$/, '')
    entries = Dir["#{path}/*"].entries.sort

    entries = entries.select{|o| o !~ /\/(vendor|log)$/}   # Don't search in unwanted dirs

    # Make these be in config!  pass along as instance var
    # TMP:::: Hard-code skipping image/ dirs (for now) !!!!!!!!!!!!!
    entries = entries.select{|o| o !~ /\/(images|twilio_sound)$/}

    if first_time and skip = FileTree.skip[path]
      entries = entries.select{|o| ! skip.member? o[/.+\/(.+)/, 1]}
    end

    # Process dirs
    entries.each{ |f|
      next unless FileTest.directory?(f)
      cleaned = clean(f, @@indent)
      @list << "#{cleaned.sub(/(^ *)/, "\\1- ")}/"
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
        result = FileTree.grep_one_file(f, regex, indent)   # Search in file contents

        if result.size > 0   # Add if any files were found
          @list << clean(f, @@indent).sub(/(^ *)/, "\\1- ")
          @list += result
        end
      else
        @list << clean(f, @@indent).sub(/(^ *)/, "\\1+ ")
      end
    end

  end

  # Does ls in current buffer, without making any modifications to the environment
  def self.ls_here dir
    t = self.new
    t.traverse View.dir
    View.insert "- #{dir}\n"

    result_indent = t.res[/^ */]   # Get indent of first line
    View.insert t.res.gsub(/^#{result_indent}/, "  ")
  end


  def self.define_styles

    if Styles.inverse   # Bullets
      Styles.define :ls_bullet,
        :face => 'courier', :size => "+2",  # Mac
        :fg => "dd7700", :bold => true
    else
      Styles.define :ls_bullet,
        :face => 'courier', :size => "+2",  # Mac
        :fg => "ff7700", :bold => true
    end

    Styles.define :diff_line_number, :bold => true, :size => "-2", :fg => "ccc"
    Styles.define :diff_red, :bg => "ffdddd", :fg => "cc4444"
    Styles.define :diff_green, :bg => "ddffcc", :fg => "337744"
    Styles.define :diff_small, :fg => "ddd", :size => "-11"

    Styles.define :quote_heading, :fg=>"fff", :size=>"0",
      :face=>"arial",
      :bold=>false
    Styles.define :quote_heading_pipe, :fg=>"4c4c4c", :size=>"0",
      :face => "verdana",
      :bold=>true
    Styles.define :quote_heading_bracket, :fg=>"4c4c4c", :size=>"-1",
      :bold=>false
    Styles.define :quote_heading_small, :fg=>"fff", :size=>"-2",
      :face => "arial black",
      :bold=>true

    if Styles.inverse
      Styles.define :diff_line_number, :bold => true, :size => "-2", :fg => "444444"
      Styles.define :diff_red, :bg => "400", :fg => "ee3333"
      Styles.define :diff_green, :bg => "130", :fg => "44dd33"
      Styles.define :diff_small, :fg => "222", :size => "-11"
    end

    # dir/
    Styles.define :ls_dir,
      :fg => "888888",
      :face => "verdana",
      :size => "-2",
      :bold => true

    # ##search/
    Styles.define :ls_search,
      :fg => "ff7700",
      :face => "verdana",
      :size => "-2",
      :bold => true

    if Styles.inverse   #   | Quoted text
      Styles.define :ls_quote,
        :size => "-1",
        :fg => "aaa"
    else
      Styles.define :ls_quote,
        :size => "-1",
        :fg => "77b"
    end

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

    # Must go before quotes - if it goes after, it supercedes them
    Styles.apply("\\(~\\)\\(.+?\\)\\(~\\)", :notes_label)
    Styles.apply("https?://[a-zA-Z0-9\/.~_:-]+", :notes_link)   # blue-ify url's

    # - bullets
    Styles.apply("^[ \t]*\\([+-]\\)\\( \\)", nil, :ls_bullet, :variable)

    # With numbers
    Styles.apply("^ +\\(:[0-9]+\\)\\(|.*\n\\)", nil, :ls_quote_line_number, :ls_quote)

    # Path-like lines and parts of lines (make gray)

    # Remove later?
    Styles.apply("^[ +-]*\\([^|\n]+/\\)$", nil, :ls_dir)  # slash at end

    Styles.apply("^[ +-]*\\([@a-zA-Z0-9_,? ().:-]*[^ \n]\/\\)", nil, :ls_dir)  # slash after almost anything
    Styles.apply("^[ +-]*\\([@a-zA-Z0-9_,? ().:-]+\/[a-zA-Z0-9_,? ().:\/-]+\/\\)", nil, :ls_dir)  # one word, path, slash
    #     Styles.apply("^[ +-]*\\([.@a-zA-Z0-9 ]+\/[.@a-zA-Z0-9 ]+\/\\)", nil, :ls_dir)  # one word, path, slash

    Styles.apply("^[ \t]*[+-] [a-zA-Z0-9_,? ().:-]+?[:)] \\(\[.@a-zA-Z0-9 ]+\/\\)", nil, :ls_dir)   # label, one word, slash
    Styles.apply("^[ \t]*[+-] [a-zA-Z0-9_,? ().:-]+?[:)] \\([.@a-zA-Z0-9 ]+\/[.@a-zA-Z0-9 \/]+\/\\)", nil, :ls_dir)   # label, one word, path, slash
    #     Styles.apply("^[ \t]*[+-] [a-zA-Z0-9_,? ().:-]+?: \\(\\w+\/.+\/\\)", nil, :ls_dir)   # Dirs with labels


    # Bullets
    Styles.apply("^[ \t]*[+-] [^(\n]+?) \\(.+/\\)$", nil, :ls_dir)   # Dirs with labels
    Styles.apply("^[ \t]*[+-] [a-zA-Z0-9_,? ().:-]+?: \\(.+/\\)$", nil, :ls_dir)   # Dirs with labels
    Styles.apply("^[ +-]*\\([^|\n]+/\\)$", nil, :ls_dir)   # Dirs with bullets

    Styles.apply('https?://[a-zA-Z0-9\/.~_:?&=|#+-]+', :notes_link)   # Url

    #   |... lines (quotes)
    Styles.apply("^ +\\(|\\)\\( *\\)", nil, :quote_heading_pipe, :ls_quote)
    Styles.apply("^ +\\(|\\)\\(.*\n\\)", nil, :quote_heading_pipe, :ls_quote)
    Styles.apply("^ +\\(|\\)\\(.+?\\)([+-].*[-+])", nil, :quote_heading_pipe, :ls_quote)   # quoted lines: beginnings of lines
    Styles.apply("^ +|.*([-+].*[+-])\\(.+\\)$", nil, :ls_quote)   # quoted lines: ends of lines
    Styles.apply("[+-])\\(.*?\\)([+-]", nil, :ls_quote)   # quoted lines: between diffs

    Styles.apply("^ +\\(|\\)\\( \\)\\(>\\)\\(\n\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading)
    Styles.apply("^ +\\(|\\)\\( \\)\\(>>\\)\\(\n\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading_small)
    # | >... Headings

    # |+... diffs
    Styles.apply("^ +\\(:[0-9]+\\)$", nil, :ls_quote)
    Styles.apply("^ +\\(|\\+.*\\)", nil, :diff_green)   # whole lines
    Styles.apply("^ +\\(|\-.*\\)", nil, :diff_red)
    Styles.apply("^ +\\(|@@ .*\n\\)", nil, :diff_line_number)

    #Styles.apply('^[ -]*\\([ a-zA-Z0-9\/_\.$-]*\\w/\\)$', nil, :ls_dir)  # Most dirs
    Styles.apply('^ *\\(//?\\)$', nil, :ls_dir)  # /
    Styles.apply('^ *\\(\./\\)$', nil, :ls_dir)  # ./


    Styles.apply('^ *[+-] \\(##.*/\\)$', nil, :ls_search)  # ##_/
    Styles.apply('^ *[+-] \\(\*\*.+/\\)$', nil, :ls_search)  # **_/
  end

  def self.handles? list=nil
    list ||= Tree.construct_path(:list => true)   # Use current line by default
    FileTree.matches_root_pattern?(list.first)
  end

  # Open the line in the tree that the cursor is on.  This is probably
  # be mapped to C-. .
  # TODO: remove ignore_prefix, and just use Keys.clear_prefix
  def self.open options={}
    original_file = View.file_name

    path = options[:path] || Tree.construct_path(:list=>true)
    path_orig = path

    if path.is_a?(Array)
      # Pull off search string if exists
      search_string = path.pop[/\|(.*)/, 1] if path.last =~ /^\|/
      search_string.sub! /^[ +-]/, '' if search_string   # Should start with space or -|+
      # Discard rest of |... lines
      path = path.grep(/^[^|]/).join('')
    else
      # Split off |... if it's there (search string)
      path =~ /(.*?)-?\+?\|(.*)/
      path, search_string = $1, $2 if $2
    end

    # Pull number off end of path if there
    path =~ /(.+):(\d+)$/
    path, line_number = $1, $2 if $2

    path = Bookmarks.expand(path)

    return Files.open_in_os(path) if Keys.prefix == 0

    remote = self.is_remote?(path)
    unless remote
      path = File.expand_path(path)
    end
    # Prefix keys with specific behavior
    prefix_was_u = false
    case Keys.prefix
    when :u   # Just open file
      prefix_was_u = true
      Keys.clear_prefix
    when 4   # Save ($ave) file
      return self.save_quoted path
    when 8
      Keys.clear_prefix
      search_string ?   # If quote, enter lines under
        Tree.enter_under :
        self.enter_lines(//)   # If file, enter all lines
      return
    when 9
      Keys.clear_prefix
      return self.drill_quotes_or_enter_lines path, search_string
    end

    # If numeric prefix, jump to nth window
    if (! options[:ignore_prefix]) and Keys.prefix and Keys.prefix != 7

      # If number larger than number of windows, open new one first
      if Keys.prefix > View.list.size
        View.to_nth(View.list.size - 1)
        View.create
      end
      View.to_nth(Keys.prefix - 1)

    end

    column = View.column
    if search_string
      column -= (Line.value[/^.*?\|./] || '').length
      column = 0 if column < 0
    end

    # Open or go to file

    if remote
      self.remote_file_contents(path)   # Get text from server and insert

    else   # Normal file opening
      options[:same_view] ? View.open(path, :same_view=>true) : Location.go(path)

      Effects.blink(:what=>:line) unless line_number or search_string
    end

    return unless line_number || search_string

    if line_number   # If line number, go to it
      goto_line line_number.to_i
      Effects.blink(:what=>:line)
    elsif search_string   # Else, search for |... string if it passed
      Move.top
      # Search for exact line match
      found = Search.forward "^#{regexp_quote(search_string)}$"

      unless found   # If not found, search for substring of line, but with a break at the end
        Move.top
        # :beginning
        found = Search.forward "#{regexp_quote(search_string)}\\([^_a-zA-Z0-9\n]\\|$\\)", :beginning=>true
        #         found = search_forward_regexp("#{regexp_quote(search_string)}\\([^_a-zA-Z0-9\n]\\|$\\)", nil, true)
      end
      unless found   # If not found, search for substring of line
        Move.top
        found = search_forward_regexp("#{regexp_quote(search_string)}", nil, true)
      end
      unless found   # If not found, search for it stripped
        Move.top
        found = search_forward_regexp("#{regexp_quote(search_string.strip)}")
      end

      beginning_of_line
      recenter(0) unless prefix_was_u
      Effects.blink(:what=>:line)

      dir, name = path.match(/(.+\/)(.+)/)[1..2]

      return if original_file == "search_log.notes"

      # Add to log
      Search.append_log dir, "- #{name}\n    | #{search_string}"
    end
    View.column = column

  end

  def self.drill_quotes_or_enter_lines path, quote
    return self.enter_lines if ! quote   # If not quote must be whole file
    result = self.drill_quote path   # Try showing only children with children
    return Tree.<<(result) if result.any?
    return Tree.enter_under   # Only leafs, so show them
  end

  def self.save_quoted path
    txt = Tree.siblings :all=>true

    return if txt[0] !~ /^\|/

    txt = txt.map{|o| "#{o.sub /^\| ?/, ''}\n"}.join('')
    DiffLog.save_diffs :patha=>path, :textb=>txt
    File.open(path, "w") { |f| f << txt }

    View.success "- Saved!"
  end

  def self.drill_quote path
    parent = Line.value
    parent.sub! /^ *\| /, ''

    found, indent, candidate, result = false, 0, nil, ""
    IO.foreach(path) do |line|
      line = line[/.*/]

      if ! found
        if line == parent
          found = true
          indent = (line[/^ */].length / 2) + 1
        end
        next
      end

      # Found

      # Skip if blank
      next if line.blank?

      current_indent = line[/^ */].length / 2

      # If indented exactly 2 under, add it
      if current_indent == indent
        candidate = "| #{line}\n"

      # If child of candidate, append candidate if one needs appending
      elsif current_indent == indent + 1
        next if candidate.nil?
        result << candidate
        candidate = nil
      # If indented less, stop
      elsif current_indent < indent
        break
      end
    end

    result
  end

  # Goes through files in reverse order and deletes empty dirs
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
      self.open_in_bar :ignore_prefix   # Open tree
      # If not on a blank line, go to the top
      View.to_top unless Line.matches(/^$/)
      # If on line, add another blank
      unless Line.matches(/^$/)
        View.insert "\n\n";  backward_char 2
      end
    end
    dir = View.expand_path(dir)   # Expand out ~
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
      use_local_map elvar.notes_mode_map
    end

    # If recursive
    if options[:recursive]
      left = point
      self.ls_here dir   # Draw actual tree
      right = point
      goto_char left
      self.select_next_file
      # Start incremental search
      Tree.search(:recursive => true, :left => left, :right => right)
      #self.draw(dir)
    else
      # Insert with linebreak if file
      if File.file?(dir)
        #insert dir
        View.insert self.filename_to_next_line(dir)  # Add linebreak before filename
        #insert dir.sub(/(.+)\//, "\\1/\n  ")  # Add linebreak before filename
        open_line 1
        #Tree.search
        Line.to_words
        Tree.search(:left => Line.left, :right => Line.left(2))
        return
      end

      View.insert "+ #{dir}\n"
      previous_line
      self.dir  # Draw actual tree
    end
  end


  def self.open_in_bar options={}

    # If numeric prefix, open nth thing in tree
    if Keys.prefix and !(Keys.prefix_u?) and !(options[:ignore_prefix])
      # Remember original view
      start = selected_window
      # Open tree (ignoring prefix)
      self.open_in_bar :ignore_prefix=>true
      # Find nth file in tree
      View.to_highest
      #beginning_of_buffer
      Keys.prefix.times do
        re_search_forward "^  +[a-zA-Z0-9_.-]+$"
      end
      # Go to next line if comment
      Line.next if Line.next_matches(/^ *\|/)
      Move.to_line_text_beginning
      self.open :ignore_prefix=>true
      return
    end

    unless View.bar?   # If already open, just go there
      View.bar
    end
    View.to_nth 0
    find_file Bookmarks.expand("$t")

    only_one_view_in_bar = Keys.prefix_u?
    only_one_view_in_bar = ! only_one_view_in_bar if @@one_view_in_bar_by_default
    unless only_one_view_in_bar  # Unless u prefix, open $tl as well (under bar)

      # If 2nd view isn't at left margin, open 2nd view
      if View.left_edge(View.list[1]) != 0
        View.create
      end

      View.to_nth 1

      # If 2nd view isn't $f, open it
      if buffer_file_name( window_buffer( View.list[1] ) ) != Bookmarks["$f"]
        find_file Bookmarks["$f"]
      end

      View.to_nth 0

    end
  end

  # Creates tree snippet of text in file
  def self.snippet str=nil
    str ||= View.selection
    # Remove linebreak from end
    str = str.sub(/\n\z/, "")
    if buffer_file_name
      "#{elvar.default_directory}\n  #{file_name_nondirectory(buffer_file_name)}\n" +
        str.gsub(/^/, "    | ").
        gsub(/^    \| $/, "    |")   # Remove trailing spaces on blank lines
    else
      "- From #{buffer_name}:\n" + str.gsub(/^/, "    #")
    end
  end

  # Recursively display dir in tree   # Insert dir contents at point (usually in existing tree)
  def self.dir options={}
    return Files.open_in_os(Tree.construct_path) if Keys.prefix == 0

    Tree.plus_to_minus_maybe
    Line.to_left
    if Keys.prefix == 8
      self.dir_recursive
    else
      self.dir_one_level options
   end
  end

  # Insert all files in dirs within a dir, and allow user to
  # incremental search therein.
  def self.dir_recursive
    beginning_of_line
    line = Line.value
    left = point
    indent = line[/^ */] + "  "  # Get indent
    dir = Tree.construct_path

    Line.next

    # Get tree
    t = self.new
    dir.sub!(/\/$/, '')
    t.traverse Bookmarks.expand(dir)

    # Adjust indent
    result_indent = t.res[/^ */]   # Get indent of first line
    View.insert t.res.gsub(/^#{result_indent}/, indent)

    right = point

    goto_char left
    #    isearch_forward
    self.select_next_file
    Tree.search(:recursive => true, :left => left, :right => right)
  end

  def self.dir_one_level options={}
    Line.to_left
    line = Line.value
    indent = line[/^ */] + "  "  # Get indent

    dir = Bookmarks.expand(Tree.construct_path)

    remote = self.is_remote?(dir)
    unless remote
      # If C-2, just open in dired
      if Keys.prefix == 2
        # TODO: Open in 1st window
        View.to_after_bar
        find_file dir
        return
      end
    end

    dirs, files = self.files_in_dir(dir, options)   # Get dirs and files in it

    # Change path to proper indent
    dirs.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1/")}

    # Change path to proper indent
    files.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1")}

    Line.next
    left = point

    # Move .notes files to top
    files = files.select{|i| i =~ /\.notes$/} + files.select{|i| i !~ /\.notes$/}

    both = options[:date_sort] || options[:size_sort] ?
      files + dirs : dirs + files
    View.insert(both.join("\n") + "\n")
    right = point
    goto_char left

    Move.to_line_text_beginning

    Tree.search(:left=>left, :right=>right, :always_search=>true)   # Kick off hidesearch (deleting)

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
      View.insert "#{snippet.sub(/.+\n.+\n/, '')}\n"
    # Check to see if it's just in same dir
    elsif snippet[/.+\n/] == buffer_substring(point, Line.left(2))
      Line.next
      View.insert "#{snippet.sub(/.+\n/, '')}\n"
    else
      View.insert "#{snippet}\n"
    end

    loc.go
    #goto_char orig

    select_window(start)
  end

  # Enter tree of selection at the spot (saved by AS).
  def self.enter_at_spot
    snippet = self.snippet
    Location.jump("0")
    View.insert "#{snippet}\n"
    Location.save("0")  # So any more will be entered after
  end

  # Enter what's in clipboard with | to on the left margin, with appropriate indent
  def self.enter_quote
    # Skip forward if on heading
    Line.to_left
    if Line[/^\|/]
      Move.to_end
      $el.newline
    end
    clip = Clipboard.get(0, :add_linebreak => true)
    dir = Tree.construct_path rescue nil
    if self.dir? && FileTree.handles? # If current line is path
      Tree.plus_to_minus_maybe
      indent = Line.indent
      dir = Tree.construct_path
      t = Clipboard.get("=")
      t = t.gsub(/^#{dir}/, '')
      if t.sub!(/\A\n/, '')   # If no dir left, indent one over
        t.gsub!(/^  /, '')
      end
      self.add_pluses_and_minuses t, '-', '-'
      Line.next
      View.insert "#{t}\n".gsub(/^/, "#{indent}  ")
      return
    end

    if Keys.prefix_u? || clip =~ /\A  +[-+]?\|[-+ ]/   # If C-u or whole thing is quoted already
      # Unquote
      clip = clip.grep(/\|/).join()
      return insert(clip.gsub(/^ *[-+]?\|([-+ ]|$)/, ""))   # Remove | ..., |+...., |<blank>, etc
    end

    if Line.blank?   # If empty line, just enter tree
      start = point
      t = Clipboard.get("=")
      indent = Keys.prefix || 0   # Indent prefix spaces, or 2
      t = t.gsub(/^/, " " * indent)
      self.add_pluses_and_minuses t, '-', '-'
      View.insert t
      set_mark(Line.left(2))
      goto_char start
      return
    end


    indent = Line.indent   # Get current indent
    on_comment_line = Line.matches /^ +\|/
    Line.next

    # Indent one level further unless on comment already
    unless on_comment_line
      indent = "#{indent}  "
    end

    indent += " " * Keys.prefix_or_0   # If numeric prefix, add to indent
    clip = clip.sub /\n+\z/, ''   # Remove last \n
    clip = clip.gsub /^/, "#{indent}\| "
    clip = clip.gsub /^( *\|) $/, "\\1"   # Remove blank lines
    View.insert "#{clip}\n"
  end

  def self.enter_as_search
    indent = Line.indent
    Move.to_end
    View.insert "\n#{indent}  - ###{Clipboard["0"]}/"
    Launcher.launch
  end

  # Remove the following lines indented more than the current one

  # Expand if dir, or open if file
  def self.launch options={}

    #Tree.plus_to_minus_maybe
    line = Line.value
    indent = Line.indent
    list = nil
    if line =~ /\.rb\//   # foo.rb/
      Launcher.wrapper
    elsif line =~ /^[^|\n]* (\*\*|##)/   # *foo or ## means do grep
      self.grep_syntax indent
    elsif self.dir?   # foo/ is a dir (if no | before)
      self.dir
    else
      self.open
    end
  end

  # Grabs matching lines in file and starts hide search
  def self.outline_pattern extension=nil, options={}
    if extension.nil?
      extension = View.file_name[/\.(\w+)$/, 1]
    end

    elisp = options[:elisp]

    if extension == "rb"
      "^\s*(def|class|module|it|describe|create_table|context) "
      #       elisp ? "^\\s-*\\(def\\|class\\|module\\|it\\|describe\\) " : "^\s*(def|class|module|it|describe) "
    elsif extension == "rake"
      "^\\s*(task|def|class) "
      #       elisp ? "^\\s-*\\(task\\|def\\|class\\) " : "^\\s*(task|def|class) "
    elsif extension == "js"
      "(^ *(function)| = function\\()"
      #       elisp ? "\\(^ *function\\| = function(\\)" : "(^ *(function)| = function\\()"
    elsif extension == "notes"
      "^[\\|>]( |$)"
      #       elisp ? "^| " : "^\\| "
    else
      "^[^ \\t\\n]"
      #       elisp ? "^[^ \\t\\n]" : "^[^ \\t\\n]"
    end
  end

  def self.enter_lines pattern=nil, options={}
    $xiki_no_search = false

    # If dir, delegate to C-. (they meant to just open it)
    return Launcher.launch if self.dir?

    Tree.plus_to_minus

    if Line.blank?   # If blank line, get bookmark and enter into current file

      bm = Keys.input(:timed => true, :prompt => "Enter bookmark to show outline for: ")
      path = Bookmarks.expand(bm, :just_bookmark => true)
      path = File.expand_path(path)
        #Files.directory? Bookmarks.expand("h", :just_bookmark=>true)

      # If it's a dir, delegate to Open Tree
      if path =~ /\/$/
        FileTree.ls :here => true, :dir => path
        return
      end

      View.insert "- " + FileTree.filename_to_next_line(path)
      $el.open_line 1
    end
    line = options[:path] || Line.value
    extension = line[/\.(\w+)$/, 1]
    if pattern.nil?
      return self.enter_lines(/#{outline_pattern extension}/, options)
    end
    Line.to_left
    path ||= options[:path] || Tree.construct_path  # Get path
    path = Bookmarks.expand(path)
    indent = Line.indent  # get indent

    # Get matches from file
    matches = ""
    indent_more = options[:path] ? '' : '  '
    if path =~ /^\/\w+@/
      contents = Remote.file_contents path
      Tree.under contents, :escape=>'| '
      return
    end

    # Adjust so it finds if we're on the line
    current_line = options[:current_line] + 1 if options[:current_line]

    line_found, matches_count, i = nil, 0, 0

    IO.foreach(path) do |line|
      i+=1
      line.sub!(/[\r\n]+$/, '')

      if current_line && line_found.nil?
        line_found = matches_count if i == current_line
      end

      next unless line =~ pattern
      line = line == "" ? "" : " #{line}"
      line.sub! /^ > $/, ' >'
      matches << "#{indent}#{indent_more}|#{line}\n"

      matches_count+=1
    end

    if Keys.prefix_u
      matches.gsub!(/    \| /, '    ')
    end

    Tree.insert_quoted_and_search matches, :line_found=>line_found
  end

  # Mapped to shortcuts that displays the trees
  def self.tree options={}
    $xiki_no_search = false

    dir = Keys.bookmark_as_path(:prompt=>"Enter bookmark to show tree of: ")
    dir = "/" if dir == :slash
    if dir.nil?
      beep
      return View.message("Bookmark doesn't exist.")
    end
    dir = Bookmarks.dir_only(dir) if options[:recursive]
    options.merge!(:dir => dir)

    # If U prefix, open in bar
    if Keys.prefix_u?
      self.ls options.merge(:open_in_bar => true)
    # Otherwise, open in new buffer
    else
      self.ls options
    end
  end

  def self.parent
    return nil unless Line[/^ /]

    orig = View.cursor   # Store original location

    Tree.to_parent
    parent = Line.without_label

    View.cursor = orig
    parent
  end

  # Returns a regexp to match the acronym
  def self.is_remote? path
    return path =~ /^\/\w+@/
  end

  # Returns the files in the dir
  def self.files_in_dir dir, options={}
    if self.is_remote?(dir)
      self.remote_files_in_dir(dir)
    else
      all = Dir.glob("#{dir}*", File::FNM_DOTMATCH).
        select {|i| i !~ /\/\.(\.*|svn|git)$/}.sort

      if options[:date_sort]
        all = all.sort{|a,b| File.mtime(b) <=> File.mtime(a)}
      elsif options[:size_sort]
        all = all.sort{|a,b| File.size(b) <=> File.size(a)}
      end

      dirs = all.select{|i| FileTest.directory?(i)}#.sort
      files = all.select{|i| FileTest.file?(i)}#.sort
      [dirs, files]
    end
  end

  def self.remote_file_contents file
    path, file = file.match(/(.+\/)(.+)/)[1..2]
    Remote.dir path, file   # Delegate to Remote.dir
  end

  def self.remote_files_in_dir dir
    res = Remote.dir(dir)
    res.map!{|i| "#{dir}#{i}"}
    [res.select{|i| i =~ /\/$/}.map{|i| i.sub(/\/$/, '')}, res.select{|i| i !~ /\/$/}]
  end

  def self.url_from_path verb, path
    server, path = path.match(/([\w.-]+)(\/.*)/)[1..2]
    "http://#{server}:7433/#{verb}#{path}"
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
    paths.sort.each do |path|   # For each path
      path = path.sub(/^\//, '')   # Create list from path
      split = path.split('/')

      # Pop from stack until path begins with stack
      while(stack.size > 0 && stack != split[0..(stack.size - 1)])
        stack.pop
      end
      indent = stack.length   # Get remainder of path after stack
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
    self.add_pluses_and_minuses result
    result
  end

  # Prepend bullets (pluses and minuses) to lines
  def self.add_pluses_and_minuses tree, dirs='-', files='+'
    tree.gsub! /^( *)([^ \n|].*\/)$/, "\\1#{dirs} \\2"
    tree.gsub! /^( *)([^ \n|].*[^\/\n])$/, "\\1#{files} \\2"
  end

  def self.tree_to_paths tree
    # TODO: implement
  end

  def self.filename_to_next_line path
    path.sub(/(.+)\//, "\\1/\n  - ")  # Add linebreak before filename
  end

  # If cursor on a tree, returns it, otherwise return path of current file
  def self.tree_path_or_this_file dir_only=false

    # If in tree, use that dir
    path = FileTree.handles? ?
      Tree.construct_path :
      View.file

    #     path = Files.just_dir(path) if dir_only
    if dir_only
      path = path =~ /\/$/ ? path : File.dirname(path)+"/"
    end
    path
  end

  def self.do_create_dir

    path = self.tree_path_or_this_file :dir_only

    $el.make_directory path

    # Construct path
    # Prompt for name
    #Keys.input
    # Create dir (using path and name)
  end

  # Indent txt to be one level lower than current line
  def self.one_view_in_bar_by_default= to
    @@one_view_in_bar_by_default = to
  end

  def self.copy_path options={}
    Effects.blink(:what=>:line)
    # If no space at left, grab dir of file
    return Clipboard["0"] = View.file unless Line.matches(/^ /)

    # If space, assume it's a tree
    dir = Tree.construct_path
    dir = View.file unless dir =~ /^\//
    Clipboard["0"] = dir
  end

  # Adds extra line if we're at the end of the file.
  # If there's no linebreak, it causes weird errors.
  def self.extra_line_if_end_of_file
    if Line.right == View.bottom
      Line.to_right
      $el.open_line(1)
    end
  end

  def self.add_slash_maybe dir
    dir =~ /\/$/ ? dir : "#{dir}/"
  end

  def self.grep_syntax indent
    Tree.plus_to_minus_maybe
    dir = Tree.construct_path
    raw = Tree.construct_path(:raw => true, :list => true)

    files = contents = nil
    if raw.join('') =~ /\*\*(.+)##(.+)/  # *foo means search in files
      files, contents = $1, $2.sub!(/\/$/, '')
    elsif raw.last =~ /\*\*(.+)/  # *foo means search in files
      files = $1
    elsif raw.last =~ /##(.+)/  # ##foo means search in filenames
      contents = $1.sub!(/\/$/, '')
      if raw[-2] =~ /\*\*(.+)/   # If prev is **, use it
        files = $1
      elsif raw[-2] =~ /(.+[^\/])$/   # If prev line is file, just show matches
        contents = Regexp.new(contents, Regexp::IGNORECASE)
        list = self.outline_search(Bookmarks.expand(dir), contents, "  ")
      end
    end
    files.sub!(/\/$/, '') if files
    options = {:raw => true}
    options.merge!({:files => files}) if files

    unless list   # If not already gotten
      Search.append_log dir, "- ###{contents}/"
      list = self.grep dir, contents, options
      list.shift   # Pull off first dir, so they'll be relative
    end
    Line.to_next
    left = point
    tree = list.join("\n") + "\n"
    View.insert tree.gsub(/^/, indent)
    right = point
    goto_char left
    if Line.matches(/^\s*$/)  # Do nothing
    elsif Line.matches(/^\s+\|/)
      if Search.outline_goto_once   # If we want to go back to a line
        Line.next Search.outline_goto_once
        Search.outline_goto_once = nil
      end

      Tree.search :left=>left, :right=>right
    elsif contents   # If we searched for something (and it wasn't just the dir)
      Move.to_quote
      Tree.search :left=>left, :right=>right, :recursive_quotes=>true
    else
      Move.to_junior
      Tree.search :left=>left, :right=>right, :recursive=>true
    end
  end

  def self.move_dir_to_junior
    Keys.prefix_times.times do
      orig = Location.new
      indent = Line.indent
      txt = View.paragraph(:start_here => true, :delete => true)
      View.insert self.move_dir_to_junior_internal(txt, Keys.prefix_u?)

      orig.go

    end
  end

  def self.move_dir_to_junior_internal txt, prefix_u=nil

    first, rest = txt.match(/(.+?\n)(.+)/m)[1..2]
    indent = first[/ */]

    # Split off any text not under branch (of first line)
    # Text indented the same as the first line (and after)
    rest_and_siblings = rest.match(/(.*?)^(#{indent}[^ \n].*)/m)
    rest, siblings = rest_and_siblings ? rest_and_siblings[1..2] : [rest, ""]

    # What does this do??
    if prefix_u
      rest.sub! /^\s*[+-] /, ''
      rest.gsub! /^  /, ''
      "#{first}#{rest}#{siblings}"
    else

      first.sub! /(.+\/)(.+\/)$/, "\\1\n#{indent}  - \\2"
      rest.gsub! /^/, "  "
      "#{first}#{rest}#{siblings}"
    end
  end

  # Returns whether line is a dir (ends with "/")
  def self.dir? txt=nil
    txt ||= Line.value
    txt =~ /^[^,|\n]*\/$/
  end

  def self.delete_file
    if ! Line[/^ +[+-] /]   # Error if not indented and ^/- /
      View.beep
      return View.message "You need to be in a file tree to delete."
    end

    dest_path_raw = Tree.construct_path
    dest_path = File.expand_path dest_path_raw

    executable = dest_path =~ /\/$/ ? "rmdir" : "rm"

    command = "#{executable} \"#{dest_path}\""

    answer = Keys.input :one_char=>true, :prompt=>"Are you sure you want to delete #{dest_path_raw} ?"   #"

    return unless answer =~ /y/i

    result = Console.run command, :sync=>true
    if (result||"").any?
      View.beep
      View.message "#{result}"
      return
    end

    Line.delete
    Move.to_line_text_beginning

  end

  def self.copy_to_latest_screenshot dest_path, dest_dir
    desktop = Bookmarks['$dt']

    latest_screenshot = `ls -t #{desktop} "Screen shot*"`[/Screen shot .*/]

    return View.message("No screenshot found.") if latest_screenshot.empty?

    command = "mv \"#{desktop}#{latest_screenshot}\" \"#{dest_path}\""

    result = Console.run command, :sync=>true
    View.message result
  end

  def self.copy_to
    prefix = Keys.prefix

    if ! Line[/^ *[+-] /]   # Error if not indented and ^/- /
      View.beep
      return View.message "You need to be in a file tree to copy."
    end

    dest_path = Tree.construct_path
    dest_dir = dest_path.sub(/(.+\/).*/, "\\1")

    if prefix == 2
      return self.copy_to_latest_screenshot dest_path, dest_dir
    end

    orig = Location.new   # Save where we are
    Location.to_spot
    source_path = Tree.construct_path

    if prefix == :u
      Line.delete

      # Adjust orig if in same file and above
      if orig.file_or_buffer == View.file_or_buffer && orig.line > View.line_number
        orig.line = orig.line - 1
      end
    end

    orig.go   # Go back to where we were

    stem = source_path.sub(/.+\/(.+)/, "\\1")   # Cut of path of source
    indent = Line.indent

    dest_is_dir = dest_path =~ /\/$/

    if dest_is_dir   # If dest is a dir, insert junior
      Line.next
      indent << "  "
      dest_stem = stem

    else   # If file, use as dest
      dest_stem = Line.without_label
    end
    executable = prefix == :u ? "mv" : "cp -r"

    # Add ".1" if prefix is 1
    if prefix == 1
      dest_stem << '.1'
    end

    command_dest_stem = dest_stem

    if source_path =~ /\/$/   # If source is dir
      source_path.sub! /\/$/, ''   # Remove slash, so it copies dir, not contents
      command_dest_stem = ""   #     put into, not replace
    end

    command = "#{executable} \"#{source_path}\" \"#{dest_dir}#{command_dest_stem}\""

    result = Console.run command, :sync=>true

    if (result||"").any?   # If output isn't as expected, beep and show error
      View.beep
      View.message "#{result}"
      return
    end

    Line.to_left
    if dest_is_dir || prefix == 1   # If it's a file, we didn't delete it
      Line.next if prefix == 1
      View.insert "#{indent}+ #{dest_stem}\n", :dont_move=>true
      Line.previous if prefix == 1
    end
    Move.to_line_text_beginning
  end

  def self.rename_file
    # If dired mode, use wdired
    return $el.wdired_change_to_wdired_mode if $el.elvar.major_mode.to_s == "dired-mode"

    column = View.column

    if ! Line[/^ *[+-] /]   # Error if not indented and ^/- /
      View.beep
      return View.message "TODO: implement renaming current file?"
    end
    source_path = Tree.construct_path
    is_dir = source_path =~ /\/$/
    source_path.sub! /\/$/, ''

    new_name = Keys.input(
      :prompt=>"Rename #{source_path} to what?: ",
      :initial_input=>(Keys.prefix_u? ? File.basename(source_path) : "")
      )

    dest_path = "#{source_path.sub(/(.+\/).+/, "\\1#{new_name}")}"

    command = "mv \"#{source_path}\" \"#{dest_path}\""

    Console.run command, :sync=>true

    indent = Line.indent
    Line.delete
    View.insert((is_dir ? "#{indent}- #{new_name}/\n" : "#{indent}+ #{new_name}\n"), :dont_move=>true)

    View.column = column
  end

  def self.open_as_upper where=false
    orig_u = Keys.prefix_u
    view = View.current
    path = Tree.construct_path(:list=>true)

    if where == :lowest
      Move.to_window 9
    else
      View.to_upper
    end
    was_at_top = View.start == View.point
    $el.split_window_vertically

    View.next if where   # :lowest or :second

    if was_at_top
      where ? View.previous : View.next
      $el.recenter 0
      where ? View.next : View.previous
    end
    FileTree.open :path=>path, :same_view=>true
    View.to_window(view) if orig_u
  end

  def self.to_outline

    prefix = Keys.prefix :clear=>true
    args = [View.txt, View.line] if prefix == :u

    current_line = Line.number

    paths = View.file
    View.to_buffer("*tree outline")
    View.clear;  notes_mode
    if paths
      View.insert FileTree.paths_to_tree(paths)
      View.to_top
      FileTree.select_next_file
    else
      View.insert "- buffer/\n"
      View.to_top
    end

    if prefix == :u
      txt, line = Search.deep_outline *args
      Tree.<< txt, :line_found=>line, :escape=>'| '
      return
    end

    FileTree.enter_lines nil, :current_line=>current_line
  end

  def self.skip_dirs dir, skip_these
    dir = Bookmarks[dir].sub /\/$/, ''
    (@skip ||= {})[dir] = skip_these
  end

  def self.matches_root_pattern? path
    without_label = Line.without_label :line=>path   #, :leave_indent=>true
    (without_label =~ /^\/[^\n,]*$/ ||
    without_label =~ /^~\// ||
    without_label =~ /^\.\.?\// ||
    without_label =~ /^\$[\w-]*\/?$/)
  end

end
FileTree.define_styles
