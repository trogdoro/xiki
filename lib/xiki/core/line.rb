module Xiki
  class Line

    def self.menu
      "
      - api/
        > Moving
        @ Line.previous   # up
        @ Line.next   # down
        |
        > Getting text
        @ p Line.value   # line text
        @ p Line.indent   # indent
        @ p Line.without_indent   # without indent
        @ p Line.label   # label
        @ p Line.without_label   # without label
        |
        > Getting Positions
        @ p Line.left   # line start
        @ p Line.right   # line ending
        |
        > Tests
        @ p Line.blank?   # blank
        @ Line[/s...thing/]   # matches something
      "
    end

    # Text on current line (minus linebreak)
    def self.[] regex, index=0
      self.value[regex, index]
    end

    def self.strip
      self.value.strip
    end

    def self.value n=1, options={}
      eol = "(point-at-eol #{n})"

      # Optionally include linebreak
      eol = "(+ 1 #{eol})" if options[:include_linebreak]

      result = $el.el4r_lisp_eval("(buffer-substring (point-at-bol #{n}) #{eol})")
      #       result = $el.buffer_substring $el.point_at_bol #{n}) #{eol})")

      if options[:delete]
        $el.el4r_lisp_eval("(delete-region (point-at-bol #{n}) #{eol})")
      end

      result
    end

    # Whether line matches pattern
    def self.matches o, line=nil
      line ||= self.value
      line[o]
    end

    # Return value of next line
    def self.next_value n=2
      #buffer_substring(point_at_bol(n), point_at_eol(n))
    end

    # Whether next line matches pattern
    def self.next_matches pattern
      self.next_value =~ pattern
    end

    def self.indent line=nil

      # If line is a number, get value
      line = self.value(line) if line.is_a?(Fixnum)

      line ||= self.value
      line[/^\s*/].gsub("\t", '        ')
    end

    def self.right down=1
      $el.point_at_eol down
    end

    def self.at_right?
      $el.point_at_eol == $el.point
    end

    def self.blank?
      self.matches /^$/
    end

    def self.left down=1
      $el.point_at_bol down
    end

    def self.rest
      View.txt(View.cursor, self.right)
    end

    def self.at_left?
      $el.point_at_bol == $el.point
    end

    def self.without_indent txt=nil
      txt ||= self.value
      txt.sub(/^\s+/, "")
    end

    def self.txt
      self.without_indent
    end

    def self.bounds options={}
      left, right = $el.point_at_bol, $el.point_at_eol
      right += 1 if options[:linebreak]
      [left, right]
    end

    def self.delete leave_linebreak=nil
      value = self.value
      left, right = Line.left, Line.right
      right += 1 unless leave_linebreak
      $el.delete_region(left, right)
      value
    end

    def self.delete_line_key
      prefix = Keys.prefix
      lines = prefix.is_a?(Fixnum) ? prefix : 1
      left, right = Line.left, Line.left(lines+1)

      $el.delete_region(left, right)
    end

    def self.kill_line
      prefix = Keys.prefix
      lines = prefix.is_a?(Fixnum) ? prefix : 1
      left, right = Line.left, Line.left(lines+1)

      # If no prefix, leave linebreak
      right -= 1 if ! prefix

      $el.delete_region(left, right)
    end

    # Gets symbol at point
    def self.symbol options={}
      symbol = $el.thing_at_point(:symbol)
      # Delete if option passed
      if options[:delete]
        $el.delete_region(* $el.bounds_of_thing_at_point(:symbol))
      end
      symbol
    end

    # Moves down, going to first column
    def self.to_next times=nil
      self.next times
    end

    # Moves up, going to first column
    def self.to_previous times=nil
      self.previous times
    end

    # Moves down, going to first column
    def self.next times=nil
      $el.forward_line times
      nil
    end

    # Moves up, going to first column
    def self.previous times=nil
      times = times ?
        -times :
        -1
      $el.forward_line times
      nil
    end

    def self.beginning
      $el.forward_line 0
      nil
    end

    def self.to_left
      self.beginning
    end

    def self.end
      $el.end_of_line
    end

    def self.to_right
      $el.end_of_line
    end

    def self.to_end
      self.to_right
    end

    def self.to_start
      self.to_left
    end

    def self.to_words
      $el.beginning_of_line
      $el.skip_chars_forward "[^ \t]"
    end

    def self.to_beginning options={}

      prefix = options[:prefix] || Keys.prefix
      down ||= prefix

      # If prefix go down n lines first
      Line.next options[:down] if options[:down]

      Line.to_left

      # If numeric prefix, move to nth word...

      if prefix.is_a? Fixnum
        Line.next prefix
      end

      # Otherwise, just move past indent...

      prefix == :u || options[:quote]?
        $el.skip_chars_forward("[^ \t]") :
        $el.skip_chars_forward("[^ \t|:!]")   # If quoted, skip quote unless :u

      nil
    end


    def self.label line=nil
      line ||= self.value
      # Space or blank can follow colon
      label = line[/^\s*[+-] (.+?): /, 1]
      label ||= line[/^\s*[+-] (.+?):$/, 1]
    end

    def self.without_label options={}
      line = options.has_key?(:line) ? options[:line] : self.value
      return nil if line.nil?

      # Delete comment (parenthesis)
      line = line.sub /^(\s*)(?:[+-]|<+) [^\n\(]*\) (.*)/, "\\1\\2"

      # If paren at end of line, delete label
      line.sub! /^(\s*)(?:[+-]|<+) [^\n\(]+?\)$/, "\\1"
      # If just bullet
      line.sub! /^(\s*)(?:[+-]|<+) (.+)/, "\\1\\2"
      # Remove whitespace by default
      line.sub!(/^ */, '') unless options[:leave_indent]

      line
    end

    def self.=~ regex
      self.value =~ regex
    end

    class << self
      alias :start :beginning
      alias :content :without_label
    end

    def self.is_bullet? line=nil
      line ||= self.value
      self.matches /^\s*[+-] /, line
    end

    # Line number from a cursor position.
    #
    # Line.number 0   # => 1
    # Line.number 20   # => 3
    # Line.number View.bottom   # => 439
    def self.number pos=nil
      # Was this better in some way?  Maybe re hidden lines?
      $el.line_number_at_pos pos || $el.point
    end

    def self.to_blank
      $el.re_search_forward "^[ \t]*$"
    end

    def self.move direction

      selection = View.selection

      # >... line, so move as block...

      if Line =~ /^>( |$)/ && ! selection
        if direction == :previous
          Keys.remember_key_for_repeat(proc { Notes.move_block(:up=>1) }, :movement=>1)
          Notes.move_block(:up=>1)
        else
          Keys.remember_key_for_repeat(proc { Notes.move_block }, :movement=>1)
          Notes.move_block
        end
        return
      end


      column = View.column
      many = Keys.prefix_times

      many = (0 - many) if direction == :previous

      range = View.range

      txt = selection ?
        View.delete(*range) :
        Line.value(1, :include_linebreak=>1, :delete=>1)   # No selection, so cut line

      Line.to_next many

      View.insert txt, :dont_move=>1

      if selection
        # Selection existed, so restore it
        cursor = View.cursor
        View.selection = cursor, cursor+selection.length
      else
        # No selection existed, so keep cursor on line
        View.column = column
      end

    end



    def self.move_word direction

      # column = View.column
      many = Keys.prefix_times

      many = (0 - many) if direction == :backward

      selection = View.selection

      # Try > if selection, move one char at a time
      if selection
        txt = View.delete(*View.range)
        $el.forward_char many
        View.insert txt
        cursor = View.cursor
        View.selection = [cursor, cursor-txt.length]
        return
      end


      # No selection, so move word
      if selection
        # Cut it
        txt = View.delete(*View.range)

        # Strip selection
        txt.strip!
      else
      # Selection, so move selected

        # Cut the word
        txt = View.delete(*$el.bounds_of_thing_at_point(:word))

      end

      # Delete char we're on if it's a space

      cursor = View.cursor


      # Delete space if before or after the cursor
      if View.txt(cursor-1, cursor) =~ /^[ ]$/ # =~ /^[ _-]$/
        deleted = View.delete(cursor-1, cursor)
      elsif View.txt(cursor, cursor+1) =~ /^[ ]$/ # =~ /^[ _-]$/
        deleted = View.delete(cursor, cursor+1)
      end


      $el.forward_word many

      if direction == :backward
        left = View.cursor
        View.insert txt
        View.insert(" ", :dont_move=>1)
      else
        txt View.insert, :dont_move=>1
        View.insert " "
        left = View.cursor
      end
      right = left + txt.length

      if selection
        View.selection = [left, right]
      end

    end


    def self.sub! from, to
      orig = Location.new
      value = Line.value
      return unless value.sub! from, to
      self.delete :leave
      View.insert value
      orig.go :assume_file=>1
      value
    end

    def self.gsub! from, to
      orig = Location.new
      value = Line.value
      return unless value.gsub! from, to
      self.delete :leave
      View.insert value
      orig.go :assume_file=>1
      value
    end

    def self.<< txt, options={}
      orig = View.cursor
      Keys.clear_prefix
      Move.to_end
      View.insert txt
      View.cursor = orig if options[:dont_move]
    end

    def self.< txt
      self.delete :leave
      View.insert txt
    end

    # Add slash to end of line, and optionally a string as well
    #
    # Line.add_slash
    # Line.add_slash "hi"
    def self.add_slash options={}

      line = Line.value

      orig = View.cursor

      line =~ /\/$/ || line.blank? ?
        Move.to_end :
        Line << "/"

      txt = options.is_a?(String) ? options : options[:txt]

      Line << txt if txt
      View.cursor = orig if options[:no_move]
      nil
    end

    def self.before_cursor
      View.txt self.left, View.cursor
    end

    def self.do_lines_sort
      old = $el.elvar.sort_fold_case# rescue true
      $el.elvar.sort_fold_case = true
      $el.sort_lines(nil, $el.region_beginning, $el.region_end)
      $el.elvar.sort_fold_case = old

      return if $el.region_beginning != View.cursor   # Can't delete blanks if not at beginning

      # Delete any blank lines (they'd be sorted to the top)
      Deletes.forward while View.char == "\n"

    end

    def self.do_lines_toggle
      prefix = Keys.prefix :clear=>1

      prefix ||= 1   # Default to one line

      if prefix.is_a? Fixnum   # If number, grab that many lines
        line_a = [Line.left, Line.left(1+prefix)]
        line_b = [Line.left(1+prefix), Line.left(1+prefix*2)]
        Effects.glow :fade_out=>1, :what=>line_b
        txt = View.delete *line_b

        View.to line_a[0]
        View.<< txt, :dont_move=>1
        line_a_size = line_a[1] - line_a[0]
        line_b.map! {|o| o - line_a_size}
        Effects.glow :fade_in=>1, :what=>line_b

        return
      end

    end

    def self.init

      return if ! $el   # Do nothing if not running under el4r

      # This seems to do the same this as line-number-at-pos (pos).
      # I think maybe it deals with hidden overlays better, but
      # we don't really use those any more.
      #
      # Is it any faster when near bottom of large files?
      #
      # Old comment:
      #   # # Define lisp function to get list of displayed lines
      #   # # In case something has been done to change them
      # $el.el4r_lisp_eval %q`
      #   (defun xiki-line-number (pos)
      #     (save-excursion
      #       (goto-char pos)
      #       (forward-line 0)
      #       (1+ (count-lines 1 (point))))
      #   )
      # `
    end

    def self.enter_docs

      line = Line.value

      if line.blank?   # If blank line, prompt for menu
        return Launcher.insert "docs"

      elsif line =~ /^(\w+\.\w+| +([+-] )?\.)/i   # If a method, grab docs from source and insert
        orig = Location.new

        txt = nil
        $el.with(:save_window_excursion) do
          Launcher.as_open

          # Grab comments
          right = View.cursor
          Move.to_previous_paragraph
          txt = View.txt View.cursor, right
          orig.go
        end

        if txt =~ /^ *[^ #]/   # Do nothing if it wasn't one big comment
          return View.flash "- No docs found for this method!"
        end

        txt = txt.gsub /^ *#( )?/, "@\\1"
        Tree.<< txt, :no_slash=>1, :no_search=>1
        return
      end

      Line.add_slash :txt=>"docs/", :unless_blank=>1
      Launcher.launch
    end

    def self.length
      self.value.length
    end

    def self.duplicate

      # If a file path, up+ will duplicate actual file, with a .1 suffix.
      # Inserts a duplicate of the line underneath.

      prefix = Keys.prefix :clear=>1

      # If in file tree and up+, actually duplicate file...

      if prefix == :u && FileTree.handles?
        Location.as_spot
        source = Tree.construct_path
        dest = Files.unique_name source

        command = "cp -r \"#{source}\" \"#{dest}\""

        Shell.run command, :sync=>true

        column = View.column
        Line << "\n#{Line[/[ +-]+/]}#{File.basename dest}"
        View.column = column

        return
      end

      column = View.column

      line = "#{Line.value}\n"
      Line.to_left

      if prefix == :u
        if line =~ /^ *:([+ -])/   # Toggle between :+... and :-...
          char = $1
          # Or, between :... and :+...
          line.sub!(/:([+ -])/){ ":#{$1 == "-" ? "+" : "-"}" }
        else   # Or just comment or uncomment
          Code.comment(:line)
        end
      end

      times = if prefix.nil?
          1
        elsif prefix == :u
          1   # Put commented line after
        elsif prefix == 0
          0
        elsif prefix > 0
          prefix + 1
        elsif prefix < 0
          prefix
        end

      Line.next times if ! char   # If was : ...
      View.insert line
      Line.previous

      Line.next if char
      View.column = column

    end

    def self.insert_date
      prefix = Keys.prefix :clear=>1

      return View.<<(Time.now.strftime("%Y-%m-%d %I:%M:%S%p").sub(' 0', ' ').downcase) if prefix == :u
      return View.<<(Time.now.strftime("%I:%M:%S%p").sub(/^0/, '').downcase) if prefix == :-

      View.<<(Time.now.strftime("%Y-%m-%d"))
    end

    def self.insert_time
      prefix = Keys.prefix :clear=>1

      # up+, so insert time and date...

      return View.<<(Time.now.strftime("%Y-%m-%d %I:%M:%S%p").sub(' 0', ' ').downcase) if prefix == :u

      # No prefix, so insert just time...

      View.<<(Time.now.strftime("%I:%M:%S%p").sub(/^0/, '').downcase)

    end

  end

  Line.init
end
