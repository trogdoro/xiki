require 'xiki/core/tree_cursor'
require 'json'

module Xiki
  # Class for manipulating tree-like strings.  Nesting is done by 2-space
  # indenting. Primary methods:
  #   .children
  #   .traverse
  class Tree

    # Eventually make this class have no editor dependencies
    # - That is, View or $el

    def self.menu
      "
      - api/
        > Get dir (or file) menu is nested under
        =p Tree.file
        =p Tree.file :require=>1   # Raise message if not nested under dir or file
        =p Tree.file :require=>'file'   # Raise message if not nested under file
        =p Tree.file :require=>'dir'   # Raise message if not nested under dir

        > Show siblings
        =p Tree.siblings

        | Include all siblings (current line is usually ommited), just
        | siblings before, or just siblings after:
        =p Tree.siblings :all=>1
        =p Tree.siblings :after=>1
        =p Tree.siblings :before=>1
        =p Tree.siblings :string=>1   # Consecutive lines, quotes removed

        > Moving around
        =Tree.to_parent   # Go to parent, regardless of blanks
        =Tree.after_children   # Go after children of this element, crossing blank lines
        =Tree.before_siblings   # Jumps to first sibling, crossing blank lines, but not double-blank lines
        =Tree.after_siblings   # Go after last sibling, crossing blank lines, but not double-blank lines

        > All methods
        =Tree.meths
      "
    end


    def self.filter options={}   # Tree.filter / tree filter / tree search

      filter_count = $el.elvar.xiki_filter_count rescue 0

      $el.make_variable_buffer_local :xiki_filter_options
      $el.make_variable_buffer_local :xiki_filter_hotkey
      $el.make_variable_buffer_local :xiki_bar_special_text

      left, right = options[:left], options[:right]

      # No search if there aren't more than 3 lines
      return if((Line.number(right) - Line.number(left)) <= 1 && View.txt(left, right) !~ /\/$/) && options[:always_search].nil? && ! options[:hotkey]

      # If searching for :hotkey, use old behavior - it's ok to lock up the UI for now...

      # Otherwise, just enable the keymap so A-Z filters...

      options["filter"] = ""
      # This forces the before filter to delegate to .filter_each
      $el.elvar.xiki_filter_options = options.to_json

      if options[:hotkey]
        # Do initial extract to just highlight
        self.filter_hotkey_extract_letters options.merge(:just_highlight=>1)
        $el.elvar.xiki_filter_hotkey = true   # Enables local map that forces C-a to C-z to be single keys
        $el.elvar.xiki_bar_special_text = "   A-Z Select     ESC Back     (or arrow keys)   "
      else

        # If in xsh view, and no filters yet, show "ESC" instead of quit
        quit_key = filter_count == 0 && View.name == "xsh" && ! View.file ?
          "ESC Quit" :
          "^Q Quit"

        $el.elvar.xiki_bar_special_text = options[:recent_history_external] ?
          "   A-Z Filter   RETURN Execute in shell   ESC Quit   (or arrow keys)   " :
          "   A-Z Filter     #{quit_key}     (or arrow keys)   "
      end
    end

    # Finish the search and run the command according to the char that was typed
    def self.search_finished
      $el.elvar.xiki_bar_special_text = nil
    end

    # Called by Tree.filter :hotkey=>1
    # Highlights 1st (or close to it) for each line.
    # Then prompts for key.
    def self.filter_hotkey letter, options={}

      $el.elvar.xiki_filter_options, $el.elvar.xiki_bar_special_text = nil, nil
      $el.xiki_filter_completed
      $el.elvar.xiki_filter_hotkey = nil

      letters = self.filter_hotkey_extract_letters options

      # Get a character as input...

      ch = letter
      ch_raw = $el.string_to_char ch

      self.delete_hotkey_underlines

      letterized = $el.char_to_string(Keys.remove_control ch_raw).to_s if ch_raw

      # They typed a letter, so process it...

      Message << ""   # Clear it so it doesn't stay around after

      if ! ch
        return self.search_finished
      end

      if ch == :control_backslash   # Ctrl+\, so just stop
        ControlTab.clear_once   # Make this Ctrl+\ be separate from the ControlTab ones
        return self.search_finished
      end

      if ch_raw == 7   # C-g
        return self.search_finished
      end

      if letters[letterized]
        self.search_finished
        Line.next letters[letterized][0] - 1
        CodeTree.kill_siblings :cross_blank_lines=>1 #, :must_match=>"~ |--"

        Launcher.launch :was_letter=>1
        return nil
      end

      # If was a valid letter but no match
      if ch =~ /^[,a-z0-9]$/i

        # Optionally pass letter as menu item when not found
        if options[:letter_when_not_found] && ch != ","
          indent = Line.indent
          bounds = Tree.sibling_bounds :cross_blank_lines=>1
          View.delete bounds[0], bounds[3]
          View << "#{indent}#{ch}"
          Launcher.launch :was_letter=>1, :letter_when_not_found=>1
          return nil
        end

        return [ch, ch_raw]   # We didn't do anything, so continue on in Tree.filter
      end

      self.search_finished
      $el.command_execute ch

      nil   # We handled it
    end

    def self.filter_hotkey_escape
      View.kill if View.name =~ /\/$/
    end

    def self.delete_hotkey_underlines
      Overlay.delete_all
    end

    def self.filter_hotkey_extract_letters options

      left, right = options[:left], options[:right]
      lines = $el.buffer_substring(left, right).split "\n"

      letters = {}
      lines.each_with_index do |l, i|
        next if l =~ /^ *[|>]/   # Don't underline if |... or >... lines
        next if options[:omit_slashes] && l =~ /\/$/   # Don't underline foo/, if :=>1

        l.length.times do |j|
          letter = l[j].chr.downcase

          next if letter !~ /[a-z0-9]/i
          next if letters[letter]

          letters[letter] = [i+1, j+1]   # Set letter to index, if not there yet
          break
        end
      end

      if options[:just_highlight]
        View.cursor = left
        return self.highlight_hotkey_letters lines, letters, Line.number
      end

      return letters
    end


    def self.highlight_hotkey_letters lines, letters, line

      cursor = Line.left
      before_moved = 0

      letters.each do |k, v|

        # Move cursor up past each line not incremented yet
        while(before_moved < v[0]-1)
          cursor += lines[before_moved].length+1
          before_moved += 1
        end

        current = lines[v[0]-1]

        slash = current =~ /\/$/
        indent = current[/^[ ~+-]+/].length

        left = cursor+v[1]-1

        face = slash ? :tree_letters2 : :tree_letters
        Overlay.face face, :left=>left, :right=>left+1

      end
    end


    # Gets block of quoted text at cursor.
    def self.quoted
      self.leaf "|"
    end

    # Return text of consecutive |... lines.
    def self.txt
      self.leaf "|"
    end

    # Gets block of quoted text at root of tree.
    def self.quoted_at_root
      orig = Location.new
      Tree.to_root
      Line.next
      txt = Tree.quoted
      orig.go
      txt
    end

    # If foo/bar line, returns bar.
    #
    # Tree.leaf("hey/you").should == "me"
    def self.leaf path, options={}
      if path =~ /(?:^\||\/\|) ?(.*)/   # If has ^| or /|, grab siblings
        # We should probably verify that the current line is the same as the path too? (in case cursor moved to other quoted tree?)
        orig = $1
        # First, make sure the current line is quoted (otherwise, .siblings will be pulling from somewhere else)
        return orig if options[:dont_look] || Line.value !~ /^ *\|/

        siblings = Tree.siblings :quotes=>1
        siblings = siblings.map{|i| i.gsub(/^\| ?/, '')}.join("\n")  # :
        siblings << "\n"
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


    # Pretty much returns regex for dirs, or dirs and files
    def self.regex_for_items_to_keep options={}
      regex = options[:quotes] || options[:recursive_quotes] ?
        /^ +[+-] [^#|:]+$|^ *:\d+$|^ *=/ :
        /^[^|:]+\/$/

    end


    def self.clear_empty_dirs! lines, options={}

      regex = self.regex_for_items_to_keep options

      lines = lines.split("\n") if lines.is_a?(String)

      file_indent = 0
      i = lines.length
      while(i > 0)
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

    # Go to the right side of the tree item (move to after last item that is indented lower)
    def self.after_children options={}
      indent = Line.indent.size

      pattern = "\\(\n\n\n\\|^ \\{0,#{indent}\\}[^ \n]\\)"   # Find line with same or less indent

      # Why would I do this at all?
      #     self.minus_to_plus_maybe unless options[:no_plus]

      Line.next
      Search.forward pattern, :go_anyway=>1
      Line.to_left

      Search.backward "^."
      Line.next
    end

    # Move cursor to after last sibling, crossing blank lines, but not double-blank lines.
    # Tree.after_siblings
    def self.after_siblings # options={}
      indent = Line.indent.size

      indent -= 2

      pattern = "\\(\n\n\n\\|^ \\{0,#{indent}\\}[^ \n]\\)"   # Find line with less indent
      pattern = "\n\n\n" if indent == -2   # If at axis

      Search.forward pattern, :go_anyway=>1
      Line.to_left

      Search.backward "^."
      Line.next
      nil
    end

    def self.collapse options={}

      # Get indent
      orig = View.cursor
      left = Line.left(Keys.prefix_u? ? 1 : 2)

      self.after_children options

      View.delete(left, View.cursor)
      View.cursor = orig
    end

    #
    # Inserts text indented under, and searches.
    # Called by Tree.<<
    #
    def self.under txt, options={}

      return if txt.nil?

      txt = TextUtil.unindent(txt) if txt =~ /\A[ \n]/ && ! options[:not_under]

      escape = options[:escape] || ''
      txt = txt.gsub(/^/, escape)
      txt.gsub!(/^([|:]) $/, "\\1")   # Remove single trailing spaces

      # Add linebreak at end if none
      txt = "#{txt}\n" unless txt =~ /\n/

      self.output_and_search txt, options

      nil
    end

    # Jumps to first sibling, crossing blank lines, but not double-blank lines
    def self.before_siblings

      indent = Line.value[/^  ( *)/, 1]

      # For now, don't handle if at root

      regex = "\\(\n\n\n\\|^#{indent}[^\t \n]\\)"
      regex = "\n\n\n" if ! indent   # If at axis
      Search.backward regex, :go_anyway=>true
      hit_two_blanks = View.cursor == Line.right

      return Move.to_next_paragraph(:no_skip=>1) if hit_two_blanks || Line.value(2).blank?
      Line.next

      # Can't get siblings of item at left margin - undecided how to implement it" if !indent
    end

    # Jumps to parent, regardless of blanks
    def self.to_parent prefix=nil #, options={}

      prefix ||= Keys.prefix :clear=>true

      Move.to_axis

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

        found = $el.search_backward_regexp "^#{indent}[^\t \n]", nil, true

        # If none indented exacty 2 less, search for one indented less
        while ! found && indent.any?
          indent.sub! "  ", ""
          found = $el.search_backward_regexp "^#{indent}[^\t \n]", nil, true
        end

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

    # Climbs ancestors to determine the path.  Assumes cursor is on a tree,
    # indented by 2 space increments.
    #
    # Examples:
    # a/
    #   =b/
    #     c/
    #       p Tree.construct_path
    #         => "b/c/p Tree.construct_path"
    #       p Tree.construct_path :list=>1
    #         => ["b/", "c/", "p Tree.construct_path :list=>1"]
    #       p Tree.construct_path :all=>1, :slashes=>1
    #         => "a/=b/c/p Tree.construct_path :all=>1"


    def self.construct_path options={}
      # 2013-03-20 TODO: rewrite this to separate out
      # - 1) Raw retrieval of each line into list
      #   - probably always get whole path (never stop upon @... lines)
      # - 2) Processing list into other form
      #   - this method will be reusable from vim, etc.

      path = []
      orig = View.cursor

      # Expand :raw option if there, which is shortcut for other options
      options.merge!(:list=>1, :all=>1, :keep_hashes=>1) if options.delete(:raw)

      # Until we're at a root...

      line_orig = line = Line.value

      clean = self.clean_path line, options
      while(line =~ /^ / && (options[:all] || clean !~ /^=/))
        line =~ /^  ( *)(.*)/
        spaces, item = $1, $2
        item = clean unless options[:labels]   # Removes labels and maybe ## and **
        if item != ""   # If item wasn't completely cleaned away

          # Make it grab siblings if |... (and quote?)...

          if item =~ /^\|/
            siblings = Tree.siblings :quotes=>"|", :string=>1
            item = Path.escape siblings
          end

          path.unshift item  # Add item to list
        end

        # Back up to next parent...

        $el.search_backward_regexp "^#{spaces}[^\t \n]"

        # If ignoring Ol lines, keep searching until not on one
        if options[:ignore_ol]
          while Line =~ /^[# ]*Ol\b/
            $el.search_backward_regexp "^#{spaces}[^\t \n]"
          end
        end

        line = Line.value
        clean = self.clean_path line, options
      end

      # Add root of tree
      root = line.sub(/^ +/, '')
      root_has_equals = root =~ /^=/
      root = self.clean_path(root, options) unless options[:labels]
      root.slice! /^= ?/
      path.unshift root


      # Prepend command heading
      # - if :all, put =... back on root if it was there

      # Only add heading if :all or no "=" at root
      if options[:all] || ! root_has_equals

        cursor_on_heading = line_orig =~ /^>/

        if command_heading = Notes.command_heading(:check_current_line=>(cursor_on_heading))
          root.sub! /^/, "=" if root_has_equals   # Add "=" back on root if was there

          # If blank path, replace, otherwise prepend to list
          if path == [""] || cursor_on_heading
            path[0].replace command_heading
          else
            path.unshift command_heading
          end
        end
      end

      # At this point, items are broken up by line...

      0.upto(path.length-2) { |i|
        path[i].sub!(/$/, '/') if path[i] !~ /\/$/
      } if options[:slashes]

      View.cursor = orig
      path =
        if options[:indented]
          indentify_path path
        elsif options[:list]
          path
        else
          path.join
        end

      path

    rescue Exception=>e
      View.cursor = orig
      raise ".construct_path couldn't construct the path - is this a well-formed tree\?: #{e}"

    end

    #
    # Moves cursor to root of tree.
    #
    # Tree.to_root   # To last @.. line
    # Tree.to_root :highest=>1   # All the way to highest root (left margin)
    #
    def self.to_root options={}

      Move.to_end   # In case already at left of line and root

      # Always go up at least once
      Tree.to_parent

      # Until we're at the root, keep jumping to parent
      line = Line.value

      if options[:highest]
        while(line =~ /^\s/) do
          Tree.to_parent
          line = Line.value
        end
        return
      end

      while(line =~ /^\s/ && line !~ /^ *([+-] )?=/) do
        Tree.to_parent
        line = Line.value
      end

      nil
    end

    def self.is_root? path
      # It's the root if it's not at the left margin
      result = path !~ /^ /
      result ? true : false
    end

    def self.clean_path path, options={}
      path = Line.without_label :line=>path#, :leave_indent=>true
      # Ignore "##"
      path.sub!(/^([^|\n-]*)##.+/, "\\1") unless options[:keep_hashes]
      # Ignore "**"
      path.sub!(/^([^|\n*-]*)\*\*.+/, "\\1") unless options[:keep_hashes]
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

    # Removes | chars at beginning
    #
    # Tree.unquote "| a\n|\n| b"
    def self.unquote txt
      txt.gsub(/^ *[|:] ?/, '')
    end

    # Add "| " to the beginnig of each line.
    #
    # Tree.quote "a\nb\n"
    # Tree.quote "a\nb\n", :unquote_menus=>1
    def self.pipe txt, options={}
      self.quote txt, options.merge(:char=>"|")
    end

    def self.quote! txt, options={}
      txt.replace self.quote(txt, options)
    end

    def self.quote txt, options={}

      txt = txt.inspect if ! txt.is_a?(String)

      # Change "| foo/" to "=foo/" > add equals char for certain patterns
      return self.prepend_equals_for_some_patterns(txt, options) if options[:unquote_menus]

      char = options[:char] || ":"
      if options[:leave_headings]
        return TextUtil.unindent(txt).gsub(/^([^>])/, "#{char} \\1").gsub(/^#{Regexp.quote char} $/, char)
      end

      txt = TextUtil.unindent(txt) if txt =~ /\A[ \n]/

      txt = txt.gsub(/^/, "#{char} ").gsub(/^#{Regexp.quote char} $/, char)

      txt = txt.gsub(/^/, options[:indent]) if options[:indent]
      txt
    end

    # Adds :... quotes before each line, except for menu-ish
    # lines and lines indented under them. For menu-ish lines,
    # it leaves them exposed (unquoted) and prepends equals to
    # the root.
    def self.prepend_equals_for_some_patterns txt, options={}
      txt = txt.split "\n", -1

      menuish_flag = false

      char = options[:char] || ":"

      txt.each do |line|

        # If no indent, reset flag
        menuish_flag = false if line !~ /^ /

        # Treat double-blank lines as a barrier? > defeats the purpose
        # Update so it sets 'menuish_flag = false' > if 2nd consecutive blank line!

        # If this line looks menu-ish, enable flag

        if line =~ /^\// ||
            line =~ /^\w[\w_ -]*\// ||
            line =~ /^https?:\/\// ||
            line =~ /^(~|[%$] |\$\w+\/)/ ||
            line =~ /^\^/
          line.sub! /^/, '='
          menuish_flag = true
        end

        next if menuish_flag

        line.sub!(/^/, line == "" ? "#{char}" : "#{char} ")

      end

      txt.join "\n"
    end

    def self.colons txt, options={}
      TextUtil.unindent(txt).gsub(/^/, ": ").gsub(/^: $/, ':')
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
      end

      Line.to_words

      return if options[:no_search]

      Tree.filter(:left=>left, :right=>right)
    end

    def self.<< txt, options={}
      self.under txt.to_s, options
    end


    def self.siblings options={}

      return self.siblings.join("\n").gsub(/^ *[|:] ?/, '')+"\n" if options[:string] && ! options[:cross_blank_lines] && ! options[:before] && ! options[:after] && ! options[:quotes]

      left1, right1, left2, right2 = self.sibling_bounds options

      quote = options[:quotes].is_a?(String) ? options[:quotes] : nil
      quote ||= Line[/^ *([|:])/, 1] || '|'

      quote = Regexp.quote quote

      # Combine and process siblings
      if options[:exclude_current]

        siblings = View.txt(options.merge(:left=>left1, :right=>right1)) + View.txt(options.merge(:left=>left2, :right=>right2))

      elsif options[:quotes]   # Only grab contiguous quoted lines

        above = View.txt(options.merge(:left=>left1, :right=>right1))
        found = true
        above = above.split("\n").reverse.select{|o| found && o =~ /^ *#{quote}/ or found = false}.reverse.join("\n")
        above << "\n" if above.any?

        middle = View.txt(options.merge(:left=>right1, :right=>left2))

        below = View.txt(options.merge(:left=>left2, :right=>right2))
        found = true
        below = below.split("\n").select{|o| found && o =~ /^ *#{quote}/ or found = false}.join("\n")
        below << "\n" if below.any?

        siblings = "#{above}#{middle}#{below}"  #.strip

      elsif options[:before]
        siblings = View.txt options.merge(:left=>left1, :right=>right1)
      elsif options[:after]
        siblings = View.txt options.merge(:left=>left2, :right=>right2)
      else
        # By default return current line in addition to its siblings
        siblings = View.txt options.merge(:left=>left1, :right=>right2)
      end

      if options[:children]
        indent = siblings[/\A */]
        return siblings.gsub(/^#{indent}/, '')
      end

      siblings.gsub! /^#{Line.indent} .*\n/, ''   # Remove more indented lines (children)
      siblings.gsub! /^ +\n/, ''   # Remove blank lines
      siblings.gsub! /^ +/, ''   # Remove indents

      if options[:string]   # Must have :before or :after option also if it got here
        return siblings.gsub /^#{quote} ?/, ''
      end

      siblings = siblings.split("\n")

      unless options[:include_label]   # Optionally remove labels
        siblings.map!{|i| Line.without_label(:line=>i)}
      end

      # Change blanks to nil
      siblings.map!{|o| o.blank? ? nil : o}

      siblings
    end

    # Gets position before and after sibling bounds.
    #
    # Tree.sibling_bounds   # Returns: start, current line start, current line end, end
    # Tree.sibling_bounds :cross_blank_lines=>1   # Returns: top, bottom
    # Tree.sibling_bounds :must_match=>/^ *\*/   # Returns: top, bottom (only includes siblings that match regex, and their children)
    def self.sibling_bounds options={}
      if options[:cross_blank_lines]   # If :cross_blank_lines, just jump to parent, then use .after_children
        indent = Line.indent

        coords = [0, Line.left, Line.right+1, 0]   # left, right1, left2, right]
        cursor = View.cursor
        self.before_siblings
        coords[0] = Line.left

        View.cursor = cursor
        self.after_siblings
        coords[3] = View.cursor

        txt = View.txt coords[2], coords[3]
        x = Xik.new txt, :leave_indent=>1

        while x.indent != indent && ! x.at_last_line?
          x.next
        end
        coords[2] += x.cursor-1
        self.apply_must_match options, coords

        View.cursor = cursor

        return coords
      end

      indent_size = Line.indent.size   # Get indent
      indent_less = indent_size - 1

      orig = Location.new

      right1 = Line.left   # Right side of lines before

      # Search for line indented less - parent (to get siblings after)
      found = indent_less < 0 ?
        Search.backward("^$", :go_anyway=>1) :
        Search.backward("^ \\{0,#{indent_less}\\}\\($\\|[^ \n]\\)")

      Line.next if found
      left1 = Line.left   # Left side of lines before

      orig.go

      # Search for line indented same or less (to get siblings after)
      Line.next
      Search.forward "^ \\{0,#{indent_size}\\}\\($\\|[^ \n]\\)"   # Move after original node's children, if any
      Line.to_left
      left2 = View.cursor
      # Search for line indented less
      indent_less < 0 ?
        Search.forward("^$") :
        Search.forward("^ \\{0,#{indent_less}\\}\\($\\|[^ \n]\\)")
      right2 = Line.left   # Left side of lines before
      orig.go

      # If :quotes, cut off lines above and below that aren't quoted
      if options[:quotes]# || options[:consecutive]

        quote = options[:quotes].is_a?(String) ? options[:quotes] : ':'
        quote = Regexp.quote quote

        # Limit til until 1st unquoted line below
        below = View.txt left2, right2
        quote_end = below =~ /^ *[^ #{quote}]/

        right2 = left2 + quote_end if quote_end

        # Limit til until 1st unquoted line above
        # Get block above and see how many chars to subtract by moving upward
        # Does same as above, but trickier to do backwards
        above = View.txt left1, right1
        above = above.split("\n")
        match_size = 0
        above.reverse.each do |o|
          break if o !~ /^ *#{quote}/
          match_size += o.length+1
        end
        left1 = right1 - match_size
      end

      coords = [left1, right1, left2, right2]
      self.apply_must_match options, coords

      coords
    end

    # :must_match=>..., so subtract non-matching off from beginning and end...!
    def self.apply_must_match options, coords

      must_match = options[:must_match]   # If :cross_blank_lines, just jump to parent, then use .after_children
      return if ! must_match

      # Move left of window up to the 1st appropriate sibling...

      txt = View.txt coords[0], coords[1]
      first_matching_before = self.matching_siblings_start_index(txt, must_match)
      coords[0] += (first_matching_before || txt.length)   # If none found, move past all

      # Move right of window back to the 1st inappropriate sibling...

      txt = View.txt coords[2], coords[3]
      first_nonmatching_after = self.matching_siblings_end_index(txt, must_match)
      coords[3] = coords[2] + first_nonmatching_after if first_nonmatching_after

    end


    # Call Tree.filter with arguments appropriate to whether "output" string
    # has nested children and/or quated children.
    def self.filter_appropriately left, right, txt, options={}
      txt

      View.cursor = left unless options[:line_found]

      # Determine how to search based on txt!

      options.merge! :left=>left, :right=>right

      root_indent = txt[/\A */]

      if txt =~ /^#{root_indent}  /   # If any indenting
        if txt =~ /^  +[|:]/
          Search.forward "^ +\\([|:]\\|- ##\\)", :beginning=>true
          Line.to_beginning
          options[:recursive_quotes] = 1
        else
          FileTree.select_next_file if txt =~ /[^\n\/]$/
          options[:recursive] = 1
        end
        Line.to_words
        Tree.filter options
      else
        Line.to_words
        Tree.filter options
      end
    end

    # Iterate through each line in the tree
    # Tree.traverse("a\n  b") {|o| p o}
    def self.traverse tree, options={}, &block

      branch, line_indent, indent = [], 0, 0

      if tree.is_a? String
        tree = tree.split("\n", -1)
      end

      tree.each_with_index do |line, i|

        # If empty line use last non-blank line's indent
        if line.empty?
          line = nil
          last_indent = line_indent
          line_indent = tree[i+1]   # Use indent of following line
          line_indent = line_indent ? (line_indent[/^ */].length / 2) : 0
          raise "Blank lines in trees between parents and children, or 2 consecutive blank lines, aren't allowed." if line_indent > 0 && line_indent > last_indent
        else
          line_indent = line[/^ */].length / 2
          line = line.sub /^ +/, ''
        end

        branch[line_indent] = line

        if line_indent < indent
          branch = branch[0..line_indent]
        end

        branch_dup = branch.dup

        if options[:no_bullets]
          branch_dup.map!{|o| o ? o.sub(/^[<+-][<=]* /, '') : nil }
        end

        flattened = branch_dup.dup

        flattened.map!{|o| o ? o.sub(/^[+-] /, '') : nil } if ! options[:no_bullets]   # Might have side-effects if done twice

        flattened = Tree.join_path flattened

        block.call [branch_dup, flattened, i+1]

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

        IO.foreach(path, *Files.encoding_binary) do |line|
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

        IO.foreach(path, *Files.encoding_binary) do |line|
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



    # Adds dots to a path according to a tree.  Dots are added when
    # the corresponding items in the tree have dots.  When the path
    # is eval'ed, the dots will amount to method calls.  Thus dotifying
    # amounts to routing.
    # Old distructive way, before Unified refactor.
    def self.dotify! tree, target
      target_flat = target.join "/"

      self.traverse(tree, :no_bullets=>1) do |branch, path|

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


    # New way - just creates boolean array (not touching path).
    def self.dotify tree, target, boolean_array=[]

      tree = tree.gsub "_", ' '

      target_flat = target.join "/"
      target_flat.gsub! "_", ' '

      self.traverse(tree, :no_bullets=>1) do |branch, path|
        match = self.target_match path, target_flat

        if match == :same || match == :longer
          indent = branch.length - 1

          # If last item in path has period
          if branch[indent] =~ /^\./ # && target[indent] !~ /^\./
            # Add period to nth item in target
            boolean_array[indent] = true
          end
        end
      end

      # Optimization
        # If last path wasn't match and indent is lower than last path, we won't match

      boolean_array
    end

    # Called by Tree.children and Tree.dotify
    #
    # Tree.target_match "a/b", "a/b"
    #  :same
    # Tree.target_match "a/b", "a"
    #  :shorter
    # Tree.target_match "a", "a/b"
    #  :longer
    def self.target_match path, target
      path, target = path.downcase, target.downcase
      pi, ti = 0, 0

      # Step through a character at a time
      while true
        pathi, targeti = path[pi], target[ti]

        # If we reached the end of one of the strings, return something...

        if pathi.nil? || targeti.nil?
          return :same if pathi.nil? && targeti.nil?   # Handles a, a and a/, a/
          return :same if pathi.nil? && target[ti+1].nil? && targeti.chr == "/"   # Handles a, a/
          return :same if targeti.nil? && path[pi+1].nil? && pathi.chr == "/"   # Handles a/, a

          return :shorter if pathi && (pathi.chr == "/" || path[pi-1].chr == "/" || pi == 0)
          return :longer if targeti && (targeti.chr == "/" || target[ti-1].chr == "/" || ti == 0)

          # At end of one, but no match
          return Path.split(path[0..pi], :trailing=>1).length-1   # Count up items in one of the paths
        end

        # If we reached the end of one of the strings, return something...

        # If chars equal, increment
        if pathi == targeti
          pi += 1
          next ti += 1

        # If path has /. or . at beginning, increment path
        elsif pathi.chr == "." && (path[pi-1].chr == "/" || pi == 0) && (target[ti-1].chr == "/" || ti == 0)
          next pi += 1

        # If path has /*/ or /* at end, increment path and increment target to next / or end
        elsif pathi.chr == "*" && path[pi-1].chr == "/" && (path[pi+1].nil? || path[pi+1].chr == "/")
          pi += 1
          ti = target.index("/", ti+1) || target.length
          next
        end
        # Ol["!"]
        break   # Not found
      end
      matched = path[0..pi]
      matched.sub! /\/$/, ''

      return Path.split(matched, :trailing=>1).length-1   # Count up items in one of the paths
    end


    # Cuts off 1st item in the path.
    #
    # Use instead of .leaf when you know all but the root is part of the leaf
    # (in case there are slashes).
    #
    # Tree.rest "hey/you/there"
    #  "you/there"
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


    # Puts this line on same line as its parent line.
    # Not sophisticated enough to deal with parent being on same line with
    # other item.
    #
    # a/b/
    #   Tree.collapse
    #       =>
    # a/b/Tree.collapse
    #
    # a/b/
    #   Tree.collapse :replace_parent=>1
    #       =>
    # Tree.collapse
    def self.collapse_upward options={}

      # If at root or end of line, go to next
      Line.next if Line !~ /^ / || Line.at_right?
      CodeTree.kill_siblings

      Move.to_end -1

      Line.sub! /([ +-]*).+/, "\\1" if options[:replace_parent]

      left = View.cursor
      $el.skip_chars_forward(" \n+-")
      View.delete left, View.cursor

      Move.to_end
      left, right = View.paragraph :bounds=>true, :start_here=>true

      $el.indent_rigidly View.cursor, right, -2
    end

    # Gets path from root to here, indenting each line by 2 spaces deeper.
    # Tree.ancestors_indented
    def self.ancestors
      path = Tree.path
      return if path.length < 2

      path[0..-2]
    end

    def self.ancestors_indented options={}

      all = options[:just_sub_tree] ? nil : 1
      path = Tree.construct_path(:list=>1, :ignore_ol=>1, :all=>all)
      result = ""
      path.each_with_index { |o, i|
        result << "#{'  ' * i}#{o}\n"
      }

      result
    end

    def self.output_and_search txt, options={}

      # TODO: passing blocks is deprecated, so can remove this
      if $el
        buffer_orig = View.buffer
        orig = Location.new
        orig_left = View.cursor
      end
      error_happened = nil

      # Might be an array or hash?
      return if !txt.respond_to?(:blank?) || txt.blank?

      # TODO: move some of this stuff into the else block above? (block_or_string is proc)

      if $el
        buffer_changed = buffer_orig != View.buffer   # Remember whether we left the buffer

        ended_up = Location.new
        orig.go   # Go back to where we were before running code
      end

      txt = txt.dup   # So nothing below alters the string

      not_under = options[:not_under] || Line.blank?

      # Move what they printed over to left margin initally, in case they haven't
      txt = TextUtil.unindent(txt) if txt =~ /\A[ \n]/ && !not_under
      txt = CodeTree.returned_to_s txt   # Turn bullets or strings into arrays

      txt = "#{txt.sub /\n+\z/, ''}\n"   # Make only one trailing linebreak

      return txt if options[:just_return]

      # Add slash to end of line if not suppressed, and line isn't a quote
      if !options[:no_slash] && !not_under && Line !~ /(^ *\||\/$)/
        Line << "/"
      end

      # Presumes cursor is at the parent node, so git indent and move to after...
      # $el.message "=3"
      Line.to_left
      if not_under
        indent = ""
      else
        indent = "#{Line.indent}  "
        Line.next
      end

      left = View.cursor
      txt.gsub! /^./, "#{indent}\\0"   # Add indent, except for blank lines

      View.<< txt, :utf8=>1
      right = View.cursor

      orig.go   # Move cursor back  <-- why doing this?
      ended_up.go   # End up where script took us
      moved = View.cursor != orig_left

      # Move to :line_found if any
      if options[:line_found] && options[:line_found] > 0
        Line.previous if not_under   # Back up one if not inserting under

        Line.next(options[:line_found])
        Line.to_words
      end

      if !error_happened && !$xiki_no_search && !options[:no_search] && !options[:launch] && !buffer_changed && !moved
        self.filter_appropriately left, right, txt, options
      elsif ! options[:line_found]
        not_under ?
          Line.to_beginning :
          Line.to_beginning(:down=>1)
      end

      Launcher.launch if options[:launch]

      txt
    end

    # Port to use Tree.path
    def self.closest_dir path=nil
      path ||= self.path

      dir = path.reverse.find{|o| FileTree.matches_root_pattern? o}

      dir = Bookmarks[dir]
      return nil if dir.nil?

      trailing_slash = dir =~ /\/\z/
      dir = File.expand_path dir
      FileTree.extract_filters! dir   # Remove ##.../ parts of path

      # Remove $... parts of dir, and after

      dir = Path.split dir
      if found = dir.index{|o| o =~ /^[$%&]/}
        dir = dir[0..found-1]
      end
      dir = Path.join dir

      # Add slash back if gone

      dir << "/" if trailing_slash

      dir
    end

    # Tree.slashless("hey/you/").should == "hey/you"
    def self.slashless txt
      txt.sub /\/$/, ''
    end


    # Extracts children from tree arg and target (path) arg.
    #
    # Or, if no tree passed in, delegates to Tree.children_at_cursor
    #
    # Tree.children "a\n  b\n  c", "a"
    def self.children tree=nil, target=nil, options={}
      raise "Target should be a string or array, not a #{target.class}" if target && ! target.is_a?(String) && ! target.is_a?(Array)

      exclamations_normal = options[:exclamations_normal]

      # Read in file, if tree looks like a file path
      tree = File.read(File.expand_path tree) if tree !~ /\n/ && tree =~ /^~/

      include_subitems = options[:include_subitems]   # Include sub-items for all children

      if tree.nil? || tree.is_a?(Hash)   # tree is actually options
        return self.children_at_cursor(tree)
      end

      target = target.join("/") if target.is_a? Array
      target = "" if target == nil || target == "/"   # Must be at root if nil

      if tree.is_a? String
        tree = TextUtil.unindent tree
        tree = tree.sub! /\n\z/, ''   # The below treats it as though there's a linebreak, so remove last one??
      end

      target.sub!(/^\//, '')
      target.sub!(/\/$/, '')

      found = nil
      result = ""

      found = -1 if target.empty?

      # All items under @... or item with no slash will be expanded shown ("preexpanded")
      @@under_preexpand = false

      self.traverse tree do |branch, path, i|
        blank = branch[-1].nil?

        if ! found
          target_match = Tree.target_match path, target

          # If target path is deeper than path with !... children, grab them as output...

          if ! exclamations_normal && branch[-1] =~ /^! / &&
            target_match == branch.length - 1   # Only match overreaching path with !... when it matched so far

            item = branch[-1]
            result << "#{item}\n"  # Output

            options[:exclamations_args] = Path.split(target)[branch.length-1..-1]

            options[:children_line] = i-1
            found = branch.length - 2   # Pretend like parent matched
            next
          end

          next if target_match != :shorter && target_match != :same   # If we found where patch matched

          # Found, so start collecting subsequent lines as output...

          options[:children_line] = i
          found = branch.length - 1   # Found, remember indent

        else

        # Match was found and we're collecting more lines as output...

          current_indent = branch.length - 1
          # If found and still indented one deeper
          one_deeper = current_indent == found + 1

          if one_deeper || ((include_subitems || @@under_preexpand) && current_indent > found)

            next result << "\n" if blank

            item = branch[-1]

            # Let menus do it themselves
            item.sub!(/^- /, '+ ') if item =~ /\/$/

            item.sub!(/^([<+-][<=]* )?\./, "\\1")
            next if item =~ /^[+-] \*\/$/   # Skip asterixes

            # If @@under_preexpand, add on indent
            if include_subitems || @@under_preexpand
              item = "#{'  ' * (branch.length - found - 2)}#{item}"
            end

            @@under_preexpand = false if one_deeper

            # Remember to gather nested items in output in some cases...
            # Nested under
            #   =foo/
            #   - no slash and no colon at beginning

            # Pre-expand if =... or doesn't end in slash
            @@under_preexpand = true if one_deeper && (item =~ /^([+-] )?=/ || (item !~ /\/$/ && item !~ /\A[+~] / && item !~ /^ *[|:]/))

            result << "#{item}\n"  # Output

          else  # Otherwise, stop looking for children if indent is less

            #           # If blank line is at same level
            #           if branch.empty? && found == current_indent
            #             next result << "\n"
            #           end

            next if current_indent > found
            # Probably doesn't need to continue on, but whatever
            # No longer beneath found item
            found = nil
            @@under_preexpand = false
          end
        end

      end

      result.empty? ? nil : result
    end

    # Returns tre at cursor.
    def self.at_cursor options={}
      orig = View.cursor

      Tree.to_root :highest=>1 if Line.indent.any?
      left = Line.left
      Tree.after_children
      right = View.cursor

      View.cursor = orig
      txt = View.txt left, right

      return [left, right] if options[:range]

      txt
    end

    #
    # Returns children indented underneath.
    #
    # Tree.children_at_cursor(:string=>1).inspect
    # Tree.children_at_cursor(:cross_blank_lines=>1).inspect
      # a
        # ab
      # b

      # c
    #
    def self.children_at_cursor options={}

      options ||= {}
      child = self.child
      return nil if child.nil?   # Return if no child

      # If :cross_blank_lines, use Tree.after_children to find end
      if options[:cross_blank_lines]
        orig = Location.new

        left = Line.left 2
        Tree.after_children
        right = Line.left
        orig.go
        return View.txt left, right
      end

      indent = Line.indent(Line.value(2)).size

      children = []
      i = 2

      # Go down and grab each line until indented less...

      while(Line.indent(Line.value(i)).size >= indent)
        child = Line.value(i)
        children << child
        i += 1
      end

      if options[:string]
        return children.join("\n")+"\n"
      end

      children
    end

    # Whether next line is more indented
    def self.children?
      Line.indent(Line.value(2)).size >
        Line.indent.size
    end

    #
    # Returns the child indented underneath (but not its children).
    #
    # Tree.child
      # foo
        # bar
    #
    def self.child
      following_line = Line.value 2
      # If indent is one greater, it is a child
      if Line.indent.size + 2 == Line.indent(following_line).size
        return Line.without_label(:line=>following_line)
      end
      nil
    end

    #
    # Returns subtree rooted at cursor.
    #
    # Improvement: should this just delegate to .siblings instead?
    #
    def self.subtree

      # Just return line if no children
      return Line.value if ! Tree.has_child?

      orig = View.cursor
      left = Line.left
      Line.next

      bounds = Tree.sibling_bounds :cross_blank_lines=>1
      bounds[0] = left
      View.cursor = orig

      txt = View.txt bounds[0], bounds[3]
      txt
    end

    #
    # Goes to spot (from as+spot) and grabs path?
    #
    def self.file_at_spot options={}
      orig = Location.new   # Save where we are
      Location.to_spot

      path = Tree.construct_path
      # TODO: Make it use this instead:
      #     path = Tree.construct_path :string=>1 instead

      if options[:delete]
        Effects.glow :fade_out=>1
        Tree.collapse
        Line.delete

        # Adjust orig if in same file and above
        if orig.file_or_buffer == View.file_or_buffer && orig.line > View.line_number
          orig.line = orig.line - 1
        end
      end

      orig.go   # Go back to where we were

      Bookmarks[path]
    end

    # Returns the dir that a =menu is nested under, or says
    # must be nested under a dir.
    #
    # Tree.dir
    def self.dir options={}   # Dir from ancestor in tree
      self.file({:require=>'dir'}.merge(options))
    end

    # Returns the dir or file that a =menu is nested under.
    # If cursor is on a file path, return just it.
    #
    # Tree.file
    # Tree.file :require=>1   # Shows message if not nested under something
    # Tree.file :require=>'dir'   # Shows message if not nested under a dir
    # Tree.file :require=>'file'   # Shows message if not nested under a file
    # Tree.file :at_cursor=>1   # Returns path only if cursor is on a file
    def self.file options={}   # ...that menu is nested under

      path = self.path

      # If tree we're in is a file tree, they probably just wanted that

      return Bookmarks[path[-1]] if FileTree.handles? path[-1]
      return nil if options[:at_cursor]

      dir = path[-2]

      return nil if ! dir

      # Remove trailing slash for now
      dir.sub! /\/$/, ''
      return Bookmarks[dir].sub('//', '/') if FileTree.handles?(dir)

      # Dir wasn't found, raise message when certain options...

      return File.expand_path("~/Desktop") if options[:or] == :desktop
      kind_required = options[:require]

      return nil if ! kind_required

      guessed_menu = path.last.split('/').first
      example = options[:example] || (kind_required == "file" ? "/tmp/file.txt" : "/tmp/dir/")

      if kind = options[:require]
        adjective = kind.is_a?(String) ? kind : "filesystem"
        raise "> This menu must be nested under a #{adjective} path, like:\n| - #{example}\n|   @#{guessed_menu}"
      end
      nil
    end

    # Post-unified way of grabbing the path for menus.  Maybe make this
    # replace .path?
    #     def self.path options={}
    def self.path options={}

      raw = self.construct_path :raw=>1
      raw_orig = raw.map{|o| o.dup}
      return raw if raw == [""]

      # Go through each line from tree, and escape |..., $..., >... and :... lines

      raw.each do |item|
        if item =~ /\A(:[^a-z]|\$ |% |& |> )..+/ && Path.unescape(item) !~ /\n/
          item.replace Path.escape item
        end
      end

      path = self.join_to_subpaths raw

      path

    end

    # Goes from [a/, @b/, c/] to [a/, b/c/]
    # Tree.join_to_subpaths(["a/", "@b/", "c/"]).should == ["a/", "b/c/"]
    # Tree.join_to_subpaths(["a/", "=b/", "c/"]).should == ["a/", "b/c/"]
    def self.join_to_subpaths path

      # Temporary implementation...

      # Step 1. Join to "a/@b/c"...
      # Maybe be more indirect about this? - maybe go directly to list of subpaths, instead of these 2 steps.
      # Do thing where we don't require slash before @ when file path? (probably not worth it)

      path = self.join_path path, :leave_blanks=>1

      # Step 2. Split to "a/", "b/c/"...
      path = Path.split path, :outer=>1
      path
    end

    # Goes from [a b c] to "abc".
    # Tree.join_path(["a/", "@b/", "c/"]).should == "a/@b/c/"
    #
    #   Possibly also from [a/, b/c/] to a/@b/c ?
    #   Tree.join(["a/", "@b/", "c/"]).should == ["a/@b/c/"]
    def self.join_path path, options={}
      self.add_slashes_except_last path, options.merge(:only_if_needed=>1)
      path.join ""
    end


    def self.paths_to_tree paths
      result = ""
      stack = []
      paths.sort.each do |path|   # For each path

        beginning_slash = path =~ /^\//
        ending_slash = path =~ /\/$/
        split = path.sub(/^\//, '').split('/')

        split[0].sub! /^/, '/' if beginning_slash   # Restore beginning slash after split
        split[-1].sub! /$/, '/' if ending_slash   # Restore beginning slash after split

        # put all slashes back first!

        # Pop from stack until path begins with stack
        while(stack.size > 0 && stack != split[0..(stack.size - 1)])
          stack.pop
        end
        indent = stack.length   # Get remainder of path after stack
        remainder = split[indent..-1]
        remainder.each do |dir|
          result << ("  " * indent) + dir
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
      tree.gsub! /^( *)([^ \n|+-].*\/)$/, "\\1#{dirs} \\2"
      tree.gsub! /^( *)([^ \n|+-].*[^\/\n])$/, "\\1#{files} \\2"
    end

    # Replace last path item with this string.
    #
    # Tree.replace_item "replaces whole line"
    # Tree.replace_item "replaces after slash"  # /hey
    # Tree.replace_item "replaces after slash"  # /hey/
    def self.replace_item txt

      new_ends_in_slash = txt =~ /\/$/   # If slash at end, remember to not add another one

      # If there's a slash (not counting end of line), replace after last slash

      if Line.value =~ /\/.+/
        (! new_ends_in_slash && Line.sub!(/(.+)\/.+\/$/, "\\1/#{txt}/")) ||
          Line.sub!(/(.+)\/.+/, "\\1/#{txt}")
      else   # else, just replace whole line minus bullet
        (! new_ends_in_slash && Line.sub!(/^([ +-]*).+\//, "\\1#{txt}\/")) ||
          Line.sub!(/^([ +-]*).+/, "\\1#{txt}")
      end

      nil
    end


    # Grab last path item.
    #
    # Tree.last_item
    # Tree.last_item   # /hey
    # Tree.last_item   # /hey/
    def self.last_item

      # If there's a slash (not counting end of line), use after last slash

      if Line.value =~ /\/.+/
        Line[/.+\/(.+)\/$/, 1] || Line[/.+\/(.+)/, 1]
      else   # else, just replace whole line minus bullet
        Line[/^[ +-]*(.+)\//, 1] || Line[/^[ +-]*(.+)/, 1]
      end
    end

    # Returns whether the next line is indented one lower
    #
    # p Tree.has_child?
    def self.has_child?
      Line.indent(Line.value)+"  " == Line.indent(Line.value 2)
    end

    def self.add_slashes_except_last list, options={}
      0.upto list.length-2 do |i|

        # Don't add slash if :only_if_needed and already has slash (unless it's an escaped slash)
        next if options[:only_if_needed] && list[i] =~ /\/\z/ && Path.split(list[i])[0] !~ /\/\z/

        # If last is slash, but not escaped
        next if options[:leave_blanks] && list[i] =~ /\A=?\z/   # If blank or just @

        list[i].sub! /$/, '/'
      end
    end


    # Returns the best source file path (and sometimes line number) from
    # options passed in.  The options have usually been parsed and
    # pre-expanded by Expander, ie. Expander.parse, Expander.expanders.
    #
    # This method is used by to+menu and @source.
    #
    # Tree.source(:menufied=>"/projects/xiki/spec/fixtures/menu/ddm")
    # Tree.source(:proc=>proc{1}) #> ["/projects/xiki/lib/xiki/tree.rb", 2104]
    def self.source options, method_options={}

      tree, locations = "", []

      # TODO: Use this if :sources exists, otherwise use below
      if options[:sources]
        Menu.climb_sources options

        tree = "#{options[:enclosing_source_dir]}\n"
        options[:sources][-1].each do |source|
          next if source =~ /\/#{options[:name]}_index/   # Don't add foo/index.... files, because files in dir will be shown anyway
          next if source =~ /\/index/   # Don't add foo/index.... files, because files in dir will be shown anyway
          tree << "  + #{source}\n"
        end

        if options[:sources].length == 1 && tree =~ /\A.+\n.+[^\/]\n\z/   # If just has 1 item that's a file, jump to it
          tree = nil
          locations << ["#{options[:enclosing_source_dir]}#{options[:sources][0][0]}"]
        end

      end

      locations << options[:proc].source_location if options[:proc]
      locations << [options[:file_path]] if options[:file_path]

      expanders = options[:expanders]

      expanders.each do |expander|
        next if ! expander.is_a? Hash
        locations << expander[:proc].source_location if expander[:proc]
      end

      # If tree and no locations, just return it
      return tree if tree.any? && locations.empty?

      # If one location, just return it
      return locations[0] if ! tree.any? && locations.length == 1

      return nil if locations.blank? && tree.blank?

      # Merge locations into tree when both
      "#{tree}#{locations.inspect}\n- improve this (when more than one pattern matched?)!"
    end

    # Updates tree to have contents of the path
    # Tree.update "a\n  b\nc", ["a", "XX"]
    #   "a\n  XX\nc"
    def self.update txt, path
      raise "currently only implemented to accept the last 2 items" if path.length > 2
      txt.sub(/^( *)([ +-]*#{path[0]}\/?)\n(^\1  .*\n)+/) do |indent, item|
        indent, item = $1, $2
        new_part = path[-1].gsub(/^/, "#{indent}  ")
        "#{indent}#{item}\n#{new_part}\n"
      end
    end

    # Finds start of the group of siblings at the bottom, that match the pattern.
    # Used to limit .sibling_bounds to only *..., etc.
    def self.matching_siblings_start_index txt, must_start_with
      return 0 if txt == ""

      indent = txt[/\A */]

      x = Xik.new txt, :leave_indent=>1
      top_match = x.lines.length

      # Make a Xik instance, and iterate up from the bottom...

      x.to_bottom

      # Keep moving up
      while ! x.at_top?
        x.previous

        next if x.indent != indent    # Siblingxs are at the axis

        # It's a sibling, so either remember and keep going, or exit...

        # if match > set top_match
        # next top_match = x.line if x =~ /^#{must_start_with}/
        next top_match = x.line if x =~ /^#{indent}(#{must_start_with})/

        # Non-matching sibling, so exit
        break
      end

      x.line = top_match
      x.cursor - 1   # Go back to zero-based

    end

    # Finds end of the group of siblings at the top, that match the pattern.
    def self.matching_siblings_end_index txt, must_start_with
      indent = txt[/\A */]   # Deduce indent from 1st line
      result = txt.index(/^#{indent}(?!#{must_start_with}| )/)   # Find indent and then something other than the pattern or a space
      result || txt.length
    end

    # Called once for each char typed after Tree.filter is called
    def self.search_number ch, options
      left, right = options[:left], options[:right]
      recursive, recursive_quotes = options[:recursive], options[:recursive_quotes]

      Keys.clear_prefix
      n = ch.to_i

      # Pull whole string out
      lines = View.txt(left, right).split "\n"
      View.delete left, right
      if recursive || recursive_quotes

        # Recursive is different because we only count children

        filtered = []
        file_count = 0
        include_regex = self.regex_for_items_to_keep options

        # Replace out lines that don't match (and aren't dirs)
        lines.each_with_index do |l, i|
          include_it = l =~ include_regex
          file_count += 1 unless include_it
          # If dir or nth, keep
          filtered << l if (include_it or (file_count == n))
        end

        # Remove dirs with nothing under them
        self.clear_empty_dirs! filtered, :quotes=>recursive_quotes

        # Put back into buffer
        View.insert(filtered.join("\n") + "\n")
        right = $el.point

        # Go to first file and go back into search
        $el.goto_char left
        recursive_quotes ?
          Move.to_quote :
          FileTree.select_next_file

      else
        nth = lines[n - 1]
        View.insert "#{nth}\n"
        $el.previous_line
      end
      Launcher.launch
    end

    # Delegated to by (xiki-filter-pre-command-handler) when typing most chars.
    def self.filter_each

      letter = $el.this_command_keys

      # Space or C-g, so just exit...

      options = JSON[$el.elvar.xiki_filter_options]
      options = TextUtil.symbolize_hash_keys options

      if options[:hotkey]
        return self.filter_hotkey letter, options
      end

      left, right = options[:left], options[:right]

      # C-g or C-m, etc, so end and maybe launch again...

      if letter =~ /[ \C-g\r\t\/$=#*\x1F\C-\\]/
        $el.elvar.xiki_filter_options, $el.elvar.xiki_bar_special_text = nil, nil
        $el.xiki_filter_completed

        if letter == "\r"

          # This is right after ^R from bash, so do a grab
          if options[:recent_history_external]
            DiffLog.grab
          else
            Launcher.launch
          end

        elsif letter == "\x1C"   # Ctrl+\
          ControlTab.clear_once

          # Do nothing, just continue

        elsif letter == "/"

          line = Line.txt

          # Don't kill siblings if "<<" or "<=" line

          if line =~ /^<+=? /
            Keys.clear_prefix
            Launcher.launch
            return
          end

          $el.delete_region(Line.left(2), right)  # Delete other files
          $el.delete_horizontal_space
          $el.delete_backward_char 1

          # delete -|+ if there
          if View.txt(View.cursor, Line.right) =~ /^[+-] /
            $el.delete_char 2
          end

          # For now, always launch when C-/
          Launcher.launch   # if line =~ /\/$/   # Only launch if it can expand

        elsif letter == "\t"
          $el.delete_region(Line.left(2), right)
          Launcher.launch   # if line =~ /\/$/   # Only launch if it can expand
        elsif letter == "\x1F"
          indent = Line.indent
          $el.delete_region left, right-1
          View.insert "#{indent}- "
          ControlLock.disable
        elsif letter == "$"
          indent = Line.indent
          $el.delete_region left, right-1
          View.insert "#{indent}$ "
          ControlLock.disable
        elsif letter == "="
          indent = Line.indent
          $el.delete_region left, right-1
          View.insert "#{indent}="
          ControlLock.disable
        elsif letter == "#"
          indent = Line.indent
          $el.delete_region left, right-1
          View.insert "#{indent}- ##/"
          Move.backward
          ControlLock.disable
        elsif letter == "*"
          indent = Line.indent
          $el.delete_region left, right-1
          View.insert "#{indent}- **/"
          Move.backward
          ControlLock.disable
        elsif letter == "\a"

          # "What is this? C-g during tree search? Why would it grab?"
          DiffLog.grab

        end
        return

      elsif letter =~ /[0-9]/

        # Number, so jump to nth...

        $el.elvar.xiki_filter_options, $el.elvar.xiki_bar_special_text = nil, nil
        $el.xiki_filter_completed
        self.search_number letter, options
        return

      elsif letter == "\a"

      elsif letter == ","

        # Comma, so do "or"...

        options[:filter] = ""
        $el.elvar.xiki_filter_options = options.to_json
        return
      end

      if letter == ";"
        letter = $el.char_to_string($el.read_char)
      end

      filter = options[:filter]
      filter << letter

      recursive, recursive_quotes = options[:recursive], options[:recursive_quotes]

      if ! left
        ignore, left, right = View.block_positions "^>"
      end

      # Extract special_text
      lines = $el.buffer_substring(left, right).split "\n"

      # Filter by letter

      if filter =~ /[A-Z]$/   # If upper, search in directory
        lines = search_dir_names(lines, /#{Regexp.quote filter}/i)
      else
        regexp = Regexp.quote filter
        regexp = "\\/$|#{regexp}" if recursive
        regexp = "^ *=|^ *:\\d|^ *[+-] [a-zA-Z0-9=:.\/]|#{regexp}" if recursive_quotes
        regexp = /#{regexp}/i
        lines = lines.grep(regexp)
      end

      # Remove dirs with nothing under them
      self.clear_empty_dirs! lines if recursive
      self.clear_empty_dirs!(lines, :quotes=>true) if recursive_quotes

      # Not sure why it was doing this > in recursive it can be 1 match
      # if lines.size == 0 || (lines.size == 1 && recursive)

      # If search not found, don't delete all
      if lines.size == 0
        View.flash "- only matches: #{filter}\n", :times=>1
        return
      end

      $el.delete_region left, right

      # Put back into buffer
      View.insert(lines.join("\n") + "\n")
      options[:right] = $el.point
      $el.goto_char left

      # Go to first file
      $el.goto_char left

      # Move to first file
      if recursive
        FileTree.select_next_file
      elsif recursive_quotes
        Search.forward ":\\|#"
        Line.to_beginning
      else
        Line.to_beginning
      end

      # Store options for next call
      $el.elvar.xiki_filter_options = options.to_json

    end

    # Maybe quits, and closes the view
    def self.filter_escape

      # Probably set this > in ControlTab
      ControlTab.last_escape_was_something_else = 1

      views_open = Buffers.list.map { |o| name = $el.buffer_name(o) }.select { |o| o !~ /^ ?\*/ && o != "views/" }

      # Return (gracefully stop search) if it's a permanent-named view, and it's not xsh with only one other view
      # If xsh and just one other view, it should continue.

      return if View.name !~ /\/$/ && ! (View.name == "xsh" && views_open.length == 1)

      # This was the 1st search done in this temporary view, so kill view

      if $el.elvar.xiki_filter_count == 1
        View.kill
        DiffLog.quit if views_open.length == 1
      end

    end

    def self.init_in_client

      $el.el4r_lisp_eval %`
        (progn

          ; For Tree.filter (normal type to narrow down)

          (defun xiki-filter-each () (interactive) (el4r-ruby-eval "Xiki::Tree.filter_each"))

          (defun xiki-filter-completed ()
            (if (boundp 'xiki-filter-count)
              (setq xiki-filter-count (+ 1 xiki-filter-count))
              (make-local-variable 'xiki-filter-count)
              (setq xiki-filter-count 1)
            )
          )

          (defun xiki-filter-pre-command-handler () (interactive)

            (when (and (boundp 'xiki-filter-options) xiki-filter-options)

              (let ((char (this-command-keys)))   ; Look at first key typed or event

                ; This might not be doing anything (maybe only necessary in gui emacs?)
                ; If control char, use actual key (bypasses control-lock)
                (when (and (stringp char) (string-match "[\C-a-\C-z]" char))
                  (setq char (char-to-string (elt (recent-keys) (- (length (recent-keys)) 1))))
                )

                (force-mode-line-update)

                ; If it was one of the characters we need

                (cond

                  ; A hotkey char to filter or exit, so pass control to ruby...

                  ((and xiki-filter-hotkey (stringp char) (string-match "[a-z0-9,\C-a-\C-z]" char))   ; Keys that delegate through to Tree.filter
                    (setq this-command 'xiki-filter-each)   ; It'll do hotkey because options[:hotkey]
                  )

                  ; A char to filter or exit, so pass control to ruby...

                  ((and (stringp char) (string-match "[a-z0-9.,;_$#*+=/ \C-g\C-m\t\C-_\C-\\-]" char))   ; Keys that delegate through to Tree.filter
                    (setq this-command 'xiki-filter-each)   ; This makes it the next command that will be run
                  )

                  ; Some other char typed, so clear out vars to turn off the Tree.filter...

                  (t
                    (if (equal char "\\C-[")
                      ; Esc (possibly the 1st pass for an arrow key etc) so set override that'll be called if its esc.
                      ; The normal-escape function clears the override, or calls it if it was just esc.
                      ; The override only closes the view or quits if it's the 1st filter in a temporary or 'xsh' view.
                      (setq xiki-escape-override 'xiki-filter-cancel-and-escape)
                      ; Else, just cancel the filter
                      (xiki-filter-cancel)
                    )
                  )

                )

              )
            )
          )

          (defun xiki-filter-cancel-and-escape ()
            (xiki-filter-cancel)
            (el4r-ruby-eval "Xiki::Tree.filter_escape")
          )

          (defun xiki-filter-cancel ()

            (setq xiki-filter-options nil)
            (xiki-filter-completed)

            ; Remember what we just did, for later
            (make-local-variable 'xiki-filter-just-finished)
            (setq xiki-filter-just-finished
              (if xiki-filter-hotkey 'hotkey 'filter)
            )

            (setq xiki-filter-hotkey nil)
            (setq xiki-bar-special-text nil)
            (el4r-ruby-eval "Xiki::Tree.delete_hotkey_underlines")
          )

          (defun xiki-filter-post-command-handler () (interactive)
            (when (boundp 'xiki-filter-just-finished)
              (makunbound 'xiki-filter-just-finished)
            )
          )

          (add-hook 'pre-command-hook 'xiki-filter-pre-command-handler)
          (add-hook 'post-command-hook 'xiki-filter-post-command-handler)

        )
      `
      nil
    end

  end
end
