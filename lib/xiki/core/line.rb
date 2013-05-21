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
      line ||= self.value
      line[/^\s*/].gsub("\t", '        ')
    end

    def self.right
      $el.point_at_eol
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

    def self.bounds
      [$el.point_at_bol, $el.point_at_eol]
    end

    def self.delete leave_linebreak=nil
      value = self.value
      bol, eol = $el.point_at_bol, $el.point_at_eol
      eol += 1 unless leave_linebreak
      $el.delete_region(bol, eol)
      value
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
      down = options[:down]
      prefix = options[:prefix] || Keys.prefix
      down ||= prefix

      # If prefix go down n lines first
      Line.next down if down.is_a? Fixnum

      Line.to_left

      prefix == :u || options[:quote]?
        $el.skip_chars_forward("[^ \t]") :
        $el.skip_chars_forward("[^ \t|]")   # If quoted, skip quote unless :u

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
      line = line.sub /^(\s*)(?:[+-]|<+) [^\n\(]+\) (.*)/, "\\1\\2"

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
      $el.xiki_line_number pos || $el.point
    end

    def self.to_blank
      $el.re_search_forward "^[ \t]*$"
    end

    def self.duplicate_line

      prefix = Keys.prefix

      # If in file tree, actually duplicate file
      if prefix == :u && FileTree.handles?
        Location.as_spot
        FileTree.copy_to :prefix=>1
        return
      end

      column = View.column

      line = "#{Line.value}\n"
      Line.to_left
      Code.comment(:line) if prefix == :u
      times = if prefix.nil?
          1
        elsif prefix == :u
          1   # Put commented line after
          # 0   # Put commented line before
        elsif prefix == 0
          0
        elsif prefix > 0
          prefix + 1
        elsif prefix < 0
          prefix
        end

      Line.next times
      View.insert line
      Line.previous

      View.column = column
    end

    def self.move direction
      column = View.column
      many = Keys.prefix_times

      many = (0 - many) if direction == :previous

      line = Line.value 1, :include_linebreak=>true, :delete=>true   # Get line
      Line.to_next many
      View.insert line

      Line.previous
      View.column = column
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

    #
    # Add slash to end of line, and optionally a string as well
    #
    # Line.add_slash
    # Line.add_slash "hi"
    #
    def self.add_slash options={}

      line = Line.value

      line =~ /\/$/ || line.blank? ?
        Move.to_end :
        Line << "/"

      txt = options.is_a?(String) ? options : options[:txt]

      orig = View.cursor
      Line << txt if txt
      View.cursor = orig if options[:left]
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

      # Define lisp function to get list of displayed lines
      # In case something has been done to change them
      $el.el4r_lisp_eval %q`
        (defun xiki-line-number (pos)
          (save-excursion
            (goto-char pos)
            (forward-line 0)
            (1+ (count-lines 1 (point))))
        )
      `
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
      Launcher.launch_unified
    end

  end

  Line.init
end
