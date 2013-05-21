require 'xiki/core/tree_cursor'

module Xiki
  # Class for manipulating tree-like strings.  Nesting is done by 2-space
  # indenting. Primary methods:
  # - .children
  # - .traverse
  class Tree

    # Eventually make this class have no editor dependencies
    # - That is, View or $el

    def self.menu
      "
      - api/
        > Get dir (or file) menu is nested under
        @ p Tree.file
        @ p Tree.file :require=>1   # Raise message if not nested under dir or file
        @ p Tree.file :require=>'file'   # Raise message if not nested under file
        @ p Tree.file :require=>'dir'   # Raise message if not nested under dir

        > Show siblings
        @ p Tree.siblings

        | Include all siblings (current line is usually ommited), just
        | siblings before, or just siblings after:
        @ p Tree.siblings :all=>1
        @ p Tree.siblings :after=>1
        @ p Tree.siblings :before=>1
        @ p Tree.siblings :string=>1   # Consecutive lines, quotes removed

        > Moving around
        @ Tree.to_parent   # Go to parent, regardless of blanks
        @ Tree.after_children   # Go after children of this element, crossing blank lines
        @ Tree.before_siblings   # Jumps to first sibling, crossing blank lines, but not double-blank lines
        @ Tree.after_siblings   # Go after last sibling, crossing blank lines, but not double-blank lines

        > All methods
        @ Tree.meths
      "
    end

    def self.search options={}
      return $xiki_no_search=false if $xiki_no_search

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

      Cursor.box

      error = ""

      pattern = ""
      lines = $el.buffer_substring(left, right).split "\n"

      ch = nil

      if options[:letter]
        ch, ch_raw = self.first_letter lines
        return if ! ch
      end

      Message << "filter... "

      ch, ch_raw = Keys.char if ch.nil?

      if ch.nil?
        return Cursor.restore(:before_file_tree)
      end

      # While chars to search for (alpha chars etc.), narrow down list...

      while ch.is_a?(String) && (ch =~ /[ -"&-),-.\/-:<?A-~]/ &&   # Be careful editing, due to ranges (_-_)
          (ch_raw < 67108912 || ch_raw > 67108921) && ch_raw != 67108909) # ||   # If not control-<number> or C--

        if ch == ' ' && pattern != ""   # If space and not already cleared out
          pattern = ''
        elsif ch_raw == 2   # C-b
          while(Line.previous == 0)
            next if FileTree.dir?   # Keep going if line is a dir
            Line.to_words
            break   # Otherwise, stop
          end

        elsif ch_raw == 6   # C-f
          while(Line.next == 0)
            next if FileTree.dir?   # Keep going if line is a dir
            Line.to_words
            break   # Otherwise, stop
          end

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
          regexp = "^ *:\\d|^ *[+-] [a-zA-Z0-9@:.\/]|#{regexp}" if recursive_quotes

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
        ch, ch_raw = Keys.char   # => :meta_return, 13

        if ch.nil?
          return Cursor.restore(:before_file_tree)
        end

      end

      Cursor.restore :before_file_tree   # Exiting, so restore cursor

      # Search exited, do something based on char that exited search...

      case ch
      when "0"
        file = self.construct_path   # Expand out ~
        # Open in OS
        $el.shell_command("open #{file}")
        #     when "\C-a"
        #       Line.to_left
      when "\C-t"   # to+Item, starting with a character

        ch = Keys.input :chars=>1
        Move.to_axis
        Search.forward "^ +[+-] #{ch}"#, :beginning=>1
        Move.backward
        CodeTree.kill_siblings
        Keys.clear_prefix
        Launcher.launch_unified

      when "\C-j"
        ch = Keys.input :chars=>1
        if ch == 't'   # just+time
          self.to_parent
          self.kill_under
          FileTree.dir :date_sort=>true
        elsif ch == 's'   # just+size
          self.to_parent
          self.kill_under
          FileTree.dir :size_sort=>true
        elsif ch == 'n'   # just+name
          self.to_parent
          self.kill_under
          FileTree.dir
        elsif ch == 'a'   # just+all

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
        end
      when :return   # Step in one level

        Keys.clear_prefix
        Launcher.launch_unified

      when :control_return, "\C-m" #, :right   # If C-., go in but don't collapse siblings
        Keys.clear_prefix
        Launcher.launch_unified

      when :meta_return, :control_period
        Keys.clear_prefix
        Launcher.launch_unified

      when "\t"   # If tab, hide siblings and go in
        $el.delete_region(Line.left(2), right)
        Keys.clear_prefix
        Launcher.launch_unified

      when :backspace#, :control_slash   # Collapse this item and keep searching
        self.to_parent
        self.kill_under
        self.search :left=>Line.left, :right=>Line.left(2)

      when :control_slash   # Move this line onto the end of its parent, and expand
        line = Line.txt

        # Don't kill siblings if "<<" or "<=" line

        if line =~ /^<+=? /
          Keys.clear_prefix
          Launcher.launch_unified
          return
        end

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
          return Launcher.launch_unified
        end

        $el.delete_region(Line.left(2), right)  # Delete other files
        $el.delete_horizontal_space
        $el.delete_backward_char 1

        # delete -|+ if there
        if View.txt(View.cursor, Line.right) =~ /^[+-] /
          $el.delete_char 2
        end

        # For now, always launch when C-/
        Launcher.launch_unified   # if line =~ /\/$/   # Only launch if it can expand

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
        Launcher.launch_unified

      when "\C-e"   # Also C-a
        return Line.to_right

      when "\C-a"   # Also C-a

        return Line.to_left

      when "\C-o"   # When 9 or C-o, show methods, or outline
        $el.delete_region(Line.left(2), right)   # Delete other files
        return FileTree.drill_quotes_or_enter_lines self.construct_path.sub(/\|.*/, ''), Line.=~(/^ *\|/)
      when "1".."9"   # If number, go to nth
        #       if ch == "7" and ! View.bar?   # Open in bar
        #         $el.delete_region(Line.left(2), right)  # Delete other files
        #         View.bar
        #         Keys.clear_prefix
        #         return Launcher.launch   # Expand or open
        #       end

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
          if options[:number_means_enter]   # If explicitly supposed to enter
            Launcher.launch_unified
          elsif FileTree.dir?   # If a dir, go into it
            Launcher.launch_unified
          else
            Launcher.launch_unified
            return

            Line.to_beginning
            # Get back into search, waiting for input
            self.search(:left=>Line.left, :right=>Line.left(2))
          end
        end

      when "\C-s"
        $el.isearch_forward

      when "\C-r"
        $el.isearch_backward

      when ";"   # Replace parent

        #       CodeTree.kill_siblings
        Tree.collapse :replace_parent=>1
        return Launcher.launch_unified


      # when "/"   # Append selected dir to parent dir
      # Just search for a slash


      when "="   # Drill into the file
        dir = self.construct_path  # Expand out ~
        View.open(dir)

        # Does something else above
        #     when "0"   # Drill into the file
        #       # TODO Is this ever used? - does it work?
        #       $el.delete_region(Line.left(2), right)   # Delete other files
        #       self.drill

      when "\a"   # Typed C-g
        View.beep
      when :left
        Move.backward
      when :right
        Move.forward
      when :up
        $el.previous_line
      when :down
        $el.next_line
      else
        $el.command_execute ch
      end
    end


    def self.first_letter lines
      letters = {}
      lines.each_with_index do |l, i|
        found = false
        l.length.times do |j|
          #         letter = l[/^  \S+ (\w)/, 1]   # Grab 1st letter
          letter = l[j].chr
          next if letter !~ /[a-z]/i
          next if letters[letter]

          letters[letter] = [i+1, j+1]   # Set letter to index, if not there yet
          break
        end
      end

      self.highlight_tree_keys letters, Line.number

      # Get input
      Message << "type 1st letter... "
      ch, ch_raw = Keys.char
      letterized = $el.char_to_string(Keys.remove_control ch_raw).to_s
      Message << ""   # Clear it so it doesn't stay around after
      Overlay.delete_all

      if ch_raw == 7   # C-g
        return Cursor.restore :before_file_tree
      end

      if letters[letterized]
        Cursor.restore :before_file_tree
        Line.next letters[letterized][0] - 1
        CodeTree.kill_siblings
        Launcher.launch_unified
        return nil
      end

      # If was a valid letter but no match
      if ch =~ /^[a-z0-9]$/i
        return [ch, ch_raw]   # We didn't do anything, so continue on
      end

      Cursor.restore :before_file_tree
      $el.command_execute ch

      nil   # We handled it
    end

    def self.highlight_tree_keys letters, line

      letters.each do |k, v|
        View.line = line + v[0] - 1
        cursor = View.cursor
        Overlay.face :tree_keys, :left=>cursor-1+v[1], :right=>cursor+v[1]
      end

      View.line = line

    end


    # Gets block of quoted text at cursor.
    def self.quoted
      self.leaf "|"
    end

    # Return text consecutive quoted lines
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


    #
    # Not used - apparently surplanted during Tree.search by
    # the methods C-a and C-o call.
    #
    #   def self.drill
    #     Line.to_left
    #     # Get indent between | and tabs
    #     match = Line.value.match(/^( *)\|( *)(.+)/)
    #     # Get content
    #     path = Bookmarks.expand(construct_path)  # Get path
    #     matches = ""
    #     if match   # It's a quoted line
    #       indent, indent_after_bar, rest = match[1,3]
    #       indent_after_bar.gsub!("\t", "        ")
    #       #"#{indent_after_bar}#{rest}"
    #       # Go through and find line
    #       found = nil
    #       IO.foreach(path[/(.+?)\|/, 1]) do |line|
    #         line.sub!(/[\r\n]+$/, '')
    #         # If not found yet, check for line
    #         if !found && line == "#{indent_after_bar}#{rest}"
    #           found = true
    #           next
    #         end
    #         next unless found

    #         # Exit if indented at same level (unless blank or comment)
    #         if((Line.indent(line).length <= indent_after_bar.length) &&
    #            (! (line =~ /^\s*$/)) &&
    #            (! (line =~ /^\s*#/))
    #            )
    #           break
    #         end

    #         # Skip unless indented 2 later
    #         next unless Line.indent(line).length == 2 + indent_after_bar.length

    #         matches << "#{indent}| #{line}\n"
    #       end
    #       self.insert_quoted_and_search matches
    #     else   # It's a file
    #       indent = Line.value[/^ */]
    #       this_was_used = last_was_used = false
    #       IO.foreach(path) do |line|  # Print lines with no indent
    #         last_was_used = this_was_used
    #         this_was_used = false
    #         line.sub!(/[\r\n]+$/, '')
    #         next if line =~ /^ +/  # Skip non top-level lines
    #         next if line =~ /^$/ and ! last_was_used  # Skip blank lines, unless following top-level
    #         matches << "#{indent}  | #{line}\n"
    #         this_was_used = true
    #       end
    #       self.insert_quoted_and_search matches
    #       # TODO Search in result

    #       # TODO Do some checking for duplicates
    #     end
    #   end

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

      return if indent == 0

      indent -= 2

      pattern = "\\(\n\n\n\\|^ \\{0,#{indent}\\}[^ \n]\\)"   # Find line with less indent

      Search.forward pattern, :go_anyway=>1
      Line.to_left

      Search.backward "^."
      Line.next
      nil
    end

    def self.kill_under options={}

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

    # Jumps to first sibling, crossing blank lines, but not double-blank lines
    def self.before_siblings

      indent = Line.value[/^  ( *)/, 1]

      # For now, don't handle if at root

      regex = "\\(\n\n\n\\|^#{indent}[^\t \n]\\)"
      Search.backward regex, :go_anyway=>true

      # Move.to_next_paragraph(:no_skip=>1)
      hit_two_blanks = View.cursor == Line.right

      return Move.to_next_paragraph(:no_skip=>1) if hit_two_blanks || Line.value(2).blank?
      Line.next

      # Can't get siblings of item at left margin - undecided how to implement it" if !indent
    end

    # Jumps to parent, regardless of blanks
    def self.to_parent prefix=nil #, options={}

      prefix ||= Keys.prefix :clear=>true

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

        # If failed, loop and search again until we get to root
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
    #   @b/
    #     c/
    #       p Tree.construct_path
    #         => "b/c/p Tree.construct_path"
    #       p Tree.construct_path :list=>1
    #         => ["b/", "c/", "p Tree.construct_path :list=>1"]
    #       p Tree.construct_path :all=>1, :slashes=>1
    #         => "a/@b/c/p Tree.construct_path :all=>1"


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

      line = Line.value

      clean = self.clean_path line, options
      while(line =~ /^ / && (options[:all] || clean !~ /^@/))
        line =~ /^  ( *)(.*)/
        spaces, item = $1, $2
        item = clean unless options[:labels]   # Removes labels and maybe ## and **
        if item != ""   # If item wasn't completely cleaned away
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
      root = self.clean_path(root, options) unless options[:labels]
      root.slice! /^@ ?/
      path.unshift root

      # At this point, items are broken up by line...

      0.upto(path.length-2) { |i|
        path[i].sub!(/$/, '/') if path[i] !~ /\/$/
      } if options[:slashes]


      View.cursor = orig
      if options[:indented]
        indentify_path path
      elsif options[:list]
        path
      else
        path.join
      end

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

      while(line =~ /^\s/ && line !~ /^ *([+-] )?@/) do
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
      path.sub!(/^([^|\n-]*)\*\*.+/, "\\1") unless options[:keep_hashes]
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
      txt.gsub(/^\| ?/, '')
    end

    # Add "| " to the beginnig of each line.
    def self.quote txt, options={}
      if options[:leave_headings]
        return TextUtil.unindent(txt).gsub(/^([^>])/, "| \\1").gsub(/^\| $/, '|')
      end
      TextUtil.unindent(txt).gsub(/^/, "| ").gsub(/^\| $/, '|')
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
      # Do a search

      return if options[:no_search]

      Tree.search(:left=>left, :right=>right)
    end

    def self.<< txt, options={}
      self.under txt.to_s, options
    end

    # Inserts indented underneath.
    # Not sure why it's calling View.under instead of Tree.under
    def self.after txt
      # Is anything calling this - maybe make them just call Tree.under.
      # View.under does a couple lines, then calls Tree.under.
      View.under txt, :after=>1
    end

    # Returns group of lines close to current line that are indented at the same level.
    # The bounds are determined by any lines indented *less* than the current line (including
    # blank lines).  In this context, lines having only spaces are not considered blank.
    # Any lines indented *more* than the current line won't affect the bounds, but will be
    # filtered out.
    #
    # p Tree.siblings              # Doesn't include current line
    # p Tree.siblings :string=>1   # As string, not list
    # p Tree.siblings :all=>1      # Includes current line
    # p Tree.siblings :everything=>1          # Includes current line and all children (returns string)
    # p Tree.siblings :cross_blank_lines=>1   # Includes current line, and crosses single blank lines (returns string)
    # p Tree.siblings :quotes=>1, :string=>1  # Consecutive |... lines as string, with pipes removed
    # p Tree.siblings :quotes=>":", :string=>1  # Consecutive :... lines as string, with pipes removed
    #
      # sample chile
    #
    def self.siblings options={}
      return self.siblings(:all=>true).join("\n").gsub(/^ *\| ?/, '')+"\n" if options[:string] && ! options[:cross_blank_lines] && ! options[:before] && ! options[:after] && ! options[:quotes]

      if options[:cross_blank_lines]
        left1, right2 = self.sibling_bounds :cross_blank_lines=>1
        # For now, if :cross_blank_lines, assume just left1, right2
        options[:all] = 1
      else
        left1, right1, left2, right2 = self.sibling_bounds# options
      end

      quote = options[:quotes].is_a?(String) ? options[:quotes] : '\\|'

      # Combine and process siblings
      if options[:all] || options[:everything]
        siblings = View.txt(options.merge(:left=>left1, :right=>right2))

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
        siblings = View.txt(options.merge(:left=>left1, :right=>right1))
      elsif options[:after]
        siblings = View.txt(options.merge(:left=>left2, :right=>right2))
      else
        # By default, don't include sibling on current line
        # TODO: swap this, so it includes all by default?
        # Go through and make new :exclude_current param, and make invocations use it
        siblings = View.txt(options.merge(:left=>left1, :right=>right1)) + View.txt(options.merge(:left=>left2, :right=>right2))
      end

      if options[:everything]
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

    #
    # Gets position before and after sibling bounds.
    #
    # Tree.sibling_bounds   # Returns: start, current line start, current line end, end
    # Tree.sibling_bounds :cross_blank_lines=>1   # Returns: top, bottom
    #
    def self.sibling_bounds options={}
      if options[:cross_blank_lines]   # If :cross_blank_lines, just jump to parent, then use .after_children
        orig = Location.new
        Tree.before_siblings
        left = Line.left
        Tree.after_siblings
        right = View.cursor

        orig.go
        return [left, right]
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

      [left1, right1, left2, right2]
    end

    def self.search_appropriately left, right, output, options={}

      View.cursor = left unless options[:line_found]
      Line.to_words

      # Determine how to search based on output!

      options.merge! :left=>left, :right=>right

      root_indent = output[/\A */]
      if output =~ /^#{root_indent}  /   # If any indenting
        if output =~ /^  +\|/
          Search.forward "^ +\\(|\\|- ##\\)", :beginning=>true
          Line.to_beginning
          options[:recursive_quotes] = true
        else
          FileTree.select_next_file
          options[:recursive] = true
        end
        Tree.search options
      else
        Tree.search options.merge(:number_means_enter=>true)
      end
    end

    # Iterate through each line in the tree
    # Tree.traverse("a\n  b") {|o| p o}
    def self.traverse tree, options={}, &block
      branch, line_indent, indent = [], 0, 0

      tree = tree.split("\n")
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
          line.sub! /^ +/, ''
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
        flattened = flattened.join('')#.gsub(/[.:]/, '')   # Why were :'s removed??

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


    #
    # Called by Tree.children and Tree.dotify
    #
    # p Tree.target_match "a/b", "a/b"
    # p Tree.target_match "a/b", "a"
    # p Tree.target_match "a", "a/b"
    #
    def self.target_match path, target
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
      return Path.split(path[0..pi], :trailing=>1).length-1   # Count up items in one of the paths
    end


    #
    # Cuts off 1st item in the path.
    #
    # Use instead of .leaf when you know all but the root is part of the leaf
    # (in case there are slashes).
    #
    # Tree.rest "hey/you/there"
    #
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
    def self.collapse options={}

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
    def self.ancestors_indented options={}

      all = options[:just_sub_tree] ? nil : 1
      path = Tree.construct_path(:list=>1, :ignore_ol=>1, :all=>all)
      result = ""
      path.each_with_index { |o, i|
        result << "#{'  ' * i}#{o}\n"
      }

      result
    end

    # Returns self and all siblings (without children).
    def self.unset_env_vars
      ENV['no_slash'] = nil
    end

    def self.output_and_search block_or_string, options={}

      line = options[:line]

      if $el
        buffer_orig = View.buffer
        orig = Location.new
        orig_left = View.cursor
      end

      error_happened = nil

      self.unset_env_vars

      output =
        if block_or_string.is_a? String
          block_or_string
        else   # Must be a proc
          begin
            block_or_string.call line
          rescue Exception=>e
            message = e.message

            error_happened = true
            CodeTree.draw_exception e, Code.to_ruby(block_or_string)
          end
        end

      return if !output.respond_to?(:blank?) || output.blank? # rescue nil

      if output.is_a?(String) && $el && output.strip =~ /\A<<< (.+)\/\z/
        Tree.replace_item $1
        Launcher.launch_unified
        return true
      end

      # TODO: move some of this crap into the else block above (block_or_string is proc)

      if $el
        buffer_changed = buffer_orig != View.buffer   # Remember whether we left the buffer

        ended_up = Location.new
        orig.go   # Go back to where we were before running code
      end

      output = output.dup   # So nothing below alters the string

      # Move what they printed over to left margin initally, in case they haven't
      output = TextUtil.unindent(output) if output =~ /\A[ \n]/
      # Remove any double linebreaks at end
      output = CodeTree.returned_to_s output

      if $el
        return View.prompt $1 if output =~ /\A\.prompt (.+)/
        return View.flash $1 if output =~ /\A\.flash (.+)/
      end

      output.sub!(/\n\n\z/, "\n")
      output = "#{output}\n" if output !~ /\n\z/

      return output if options[:just_return]


      # Add slash to end of line if not suppressed, and line isn't a quote
      line=options[:line]
      if !options[:no_slash] && ! ENV['no_slash'] && Line !~ /(^ *\||\/$)/
        Line << "/"
      end
      indent = Line.indent
      Line.to_left
      Line.next
      left = View.cursor

      output.gsub! /^./, "#{indent}  \\0"   # Add indent, except for blank lines

      View.<< output, :utf8=>1
      right = View.cursor

      orig.go   # Move cursor back  <-- why doing this?
      ended_up.go   # End up where script took us
      moved = View.cursor != orig_left

      # Move to :line_found if any
      if options[:line_found] && options[:line_found] > 0
        Line.next(options[:line_found])
      end

      if !error_happened && !$xiki_no_search &&!options[:no_search] && !buffer_changed && !moved
        Tree.search_appropriately left, right, output, options
      elsif ! options[:line_found]
        Line.to_beginning :down=>1
      end
      output
    end

    # Port to use Tree.path_unified
    def self.closest_dir path=nil
      path ||= Tree.path_unified

      dir = path.reverse.find{|o| FileTree.matches_root_pattern? o}

      dir = Bookmarks[dir]
      return nil if dir.nil?
      File.expand_path dir
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

      exclamations_normal = options[:exclamations_normal]

      # Read in file, if tree looks like a file path
      tree = File.read(File.expand_path tree) if tree !~ /\n/ && tree =~ /^~/

      include_subitems = options[:include_subitems]   # Include sub-items for all children

      return self.children_at_cursor(tree) if tree.nil? || tree.is_a?(Hash)   # tree is actually options

      target = target.join("/") if target.is_a? Array
      target = "" if target == nil || target == "/"   # Must be at root if nil
      tree = TextUtil.unindent tree

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

          next if target_match != :shorter && target_match != :same # &&   # If we found where patch matched

          options[:children_line] = i
          found = branch.length - 1   # Found, remember indent

        else
          current_indent = branch.length - 1
          # If found and still indented one deeper
          one_deeper = current_indent == found + 1

          if one_deeper || ((include_subitems || @@under_preexpand) && current_indent > found)

            next result << "\n" if blank

            item = branch[-1]
            item.sub!(/^- /, '+ ') if item =~ /\/$/
            item.sub!(/^([<+-][<=]* )?\./, "\\1")
            next if item =~ /^[+-] \*\/$/   # Skip asterixes

            # If @@under_preexpand, add on indent
            if include_subitems || @@under_preexpand
              item = "#{'  ' * (branch.length - found - 2)}#{item}"
            end

            @@under_preexpand = false if one_deeper

            # Pre-expand if @... or doesn't end in slash
            @@under_preexpand = true if one_deeper && (item =~ /^([+-] )?@/ || item !~ /\/$/)

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

    #
    # Returns children indented underneath.
    #
    # Tree.children.inspect
    # Tree.children(:string=>1).inspect
    # Tree.children(:cross_blank_lines=>1).inspect
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
        # return
        return View.txt left, right
      end

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
      ignore, right = Tree.sibling_bounds :cross_blank_lines=>1
      View.cursor = orig

      txt = View.txt left, right
      txt
    end

    #
    # Goes to spot (from as+spot) and grabs path?
    #
    def self.dir_at_spot options={}
      orig = Location.new   # Save where we are
      Location.to_spot

      path = Tree.construct_path
      # TODO: Make it use this instead:
      #     path = Tree.construct_path :string=>1 instead

      if options[:delete]
        Effects.glow :fade_out=>1
        Tree.kill_under
        Line.delete

        # Adjust orig if in same file and above
        if orig.file_or_buffer == View.file_or_buffer && orig.line > View.line_number
          orig.line = orig.line - 1
        end
      end

      orig.go   # Go back to where we were

      Bookmarks[path]
    end

    # Returns the dir that a @menu is nested under, or says
    # must be nested under a dir.
    #
    # Tree.dir
    def self.dir options={}
      self.file options.merge(:require=>'dir')
    end

    # Returns the dir or file that a @menu is nested under.
    # If cursor is on a file path, return just it.
    #
    # Tree.file
    # Tree.file :require=>1   # Shows message if not nested under something
    # Tree.file :require=>'dir'   # Shows message if not nested under a dir
    # Tree.file :require=>'file'   # Shows message if not nested under a file
    # Tree.file :at_cursor=>1   # Returns path only if cursor is on a file
    def self.file options={}
      path = self.path

      # If tree we're in is a file tree, they probably just wanted that

      return Bookmarks[path[-1]] if FileTree.handles? path[-1]

      return nil if options[:at_cursor]

      dir = path[-2]

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

    def self.path__new options={}
      return path.join if options[:string]
      path
    end

    def self.path__old options={}
      path = Tree.construct_path :all=>1, :slashes=>1
      options[:string] ? path : path.split(/\/@ ?/)
    end

    def self.path__old_refactored options={}
      path = Tree.construct_path :all=>1, :slashes=>1
      return path if options[:string]
      path = path.split(/\/@ ?/)
    end

    # Post-unified way of grabbing the path for menus.  Maybe make this
    # replace .path?
    def self.path_unified options={}
      raw = self.construct_path :raw=>1

      return raw if raw == [""]

      # Go through each line from tree, and escape |... and :... lines

      raw.each do |item|
        if item =~ /^\|.(.+)/
          item.replace Path.escape item
        end
      end

      # If :... grab siblings and merge together (but only if last line)
      if raw[-1] =~ /^:/
        txt = Tree.siblings :quotes=>":", :string=>1
        txt = Path.escape txt
        raw[-1] = txt
      end


      self.join_to_subpaths raw
    end

    # Goes from [a/, @b/, c/] to [a/, b/c/]
    # Tree.join_to_subpaths(["a/", "@b/", "c/"]).should == ["a/", "b/c/"]
    def self.join_to_subpaths path

      # Temporary implementation...

      # Step 1. Join to "a/@b/c"...
      # Maybe be more indirect about this? - maybe go directly to list of subpaths, instead of these 2 steps.
      # Do thing where we don't require slash before @ when file path? (probably not worth it)
      path = self.join_path path

      # Step 2. Split to "a/", "b/c/"...
      path = Path.split path, :outer=>1

      path
    end


    # Goes from [a b c] to "abc".
    # Tree.join_path(["a/", "@b/", "c/"]).should == "a/@b/c/"
    #
    #   Possibly also from [a/, b/c/] to a/@b/c ?
    #   Tree.join(["a/", "@b/", "c/"]).should == ["a/@b/c/"]
    def self.join_path path
      self.add_slashes_except_last path, :only_if_blank=>1
      path.join ""
    end

    # Deprecated.  After Unified refactor, delete this and replace it with implementation of .path_unified.
    # Pre-unified way of grabbing the path for menus.
    def self.path options={}
      path = Tree.construct_path :all=>1, :slashes=>1
      return path if options[:string]
      path = path.split(/\/@ ?/)
      self.add_slashes_except_last path
      path
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

    # Tree.to_html "p/\n  hi\n"
    def self.to_html txt
      html = ""

      txt = txt.gsub /^( *)([+-] )?(\w[\w ]*\/)(.+)/, "\\1\\3\n\\1  \\4"   # Preprocess to break foo/Bar into separate lines

      previous = []
      Tree.traverse(txt) do |l, path|

        last = l.last
        next if !last   # Blank lines

        self.add_closing_tags html, l, previous   # If lower than last, add any closing tags

        last = Line.without_label :line=>last
        if last =~ /([^*\n]+)\/$/
          tag = $1
          html.<<("  " * (l.length-1)) unless l[-2] =~ /[ +-]*pre\/$/

          next html << "<#{tag}>\n"
        end

        last.sub! /^\| ?/, ''

        if last =~ /\.\.\.$/   # If "Lorem..." or "Lorem ipsum..." etc. make progressively longer
          old_length = last.length
          last.gsub!(/\w+/){|o| @@lorem[o.downcase] || o}
          last.sub!(/\.\.\.$/, '') if last.length != old_length   # Remove ... if we replaced something
        end

        parent = l[-2]
        html.<<("  " * (l.length-1)) unless parent =~ /[ +-]*pre\/$/

        html << "#{last}\n"
      end


      self.add_closing_tags html, [], previous

      html
    end

    @@lorem = {
      "lorem"=>"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
      "ipsum"=>"Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
      "dolor"=>"Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
      "sit"=>"Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
    }

    def self.add_closing_tags html, l, previous

      if l.length <= previous.length
        left = l.length-1
        left = 0 if left < 0
        close_these = previous[left..-1]
        close_these.reverse.each_with_index do |tag, i|

          tag.sub! /^\| ?/, ''
          tag = Line.without_label :line=>tag
          next if tag !~ /(.*\w)\/$/ && tag !~ /^<([^<\n]*[\w"'])>$/
          tag = $1
          tag = tag.sub(/ \w+=.+/, '')
          next if ["img"].member? tag
          html << "  " * (previous.length - i - 1)
          html << "</#{tag}>\n"
        end
      end
      previous.replace l
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
        next if options[:only_if_blank] && list[i] =~ /\/$/
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

        tree = "#{options[:last_source_dir]}\n"
        options[:sources][-1].each do |source|

          next if source =~ /\/index/   # Don't add foo/index.... files, because files in dir will be shown anyway

          tree << "  + #{source}\n"
        end

        if options[:sources].length == 1 && tree =~ /\A.+\n.+[^\/]\n\z/   # If just has 1 item that's a file, jump to it
          tree = nil
          locations << ["#{options[:last_source_dir]}#{options[:sources][0][0]}"]
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
      "#{tree}#{locations.inspect}\n- improve this!"
    end

  end
end
