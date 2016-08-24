require 'xiki/core/keys'
require 'xiki/core/file_tree'

module Xiki
  # Provides copy and paste functionality
  class Clipboard

    # Stores things user copies
    @@hash ||= {}
    @@hash_by_first_letter ||= {}

    def self.names key=nil

      # /log/, so show keys...

      if ! key
        result = ""
        keys = @@hash.keys.sort! {|a, b| a.sub(/[!-\/]/, '~') <=> b.sub(/[!-\/]/, '~') }   # Move numbers to top
        keys.each do |k|
          val = Tree.quote @@hash[k]
          val.gsub! /^/, '  '
          result << ": #{k}\n"
        end
        return result.empty? ? "- Nothing was copied yet!" : result
      end

      # /log/foo, so show value...

      line = Line.value.sub(/^ *: /, "")

      Tree.quote @@hash[line], :char=>"|"
    end

    def self.copy loc=nil, txt=nil
      # Use string if user types it quickly
      if ! loc
        View.flash "Enter variable name:", :times=>1
        loc = Keys.input(:chars=>1, :prompt=>"Enter one char (variable name to store this as): ") || "0"
      end

      unless txt
        left, right = View.range
        Effects.blink :left=>left, :right=>right
        txt = $el.buffer_substring($el.region_beginning, $el.region_end)
      end
      self.set loc, txt # , Keys.prefix)
    end

    def self.cut loc=nil
      loc = loc.to_s
      prefix = Keys.prefix :clear=>true   # If numeric prefix, reset region

      if prefix == 0
        l, r = View.paragraph :bounds=>true
        View.cursor = l
        View.mark = r
      elsif prefix.is_a?(Fixnum)
        Line.to_left
        View.mark = Line.left 1+prefix
      end

      self.copy loc
      $el.delete_region($el.region_beginning, $el.region_end)

      Location.as_spot('killed')
    end

    def self.paste loc=nil
      # Use string if user types it quickly
      loc ||= Keys.input(:chars=>1, :prompt => "Enter one char: ") || 0

      $el.set_mark_command nil

      loc = loc.to_s
      txt = @@hash[loc] || @@hash_by_first_letter[loc]   # If nothing, try to grab by first letter

      # If nothing, try to grab from what's been searched
      txt ||= Search.searches.find{|o| o =~ /^#{loc}/i}

      return View.message("Nothing to search for matching '#{loc}'.", :beep=>1) if txt.nil?

      ($el.elvar.current_prefix_arg || 1).times do   # Get from corresponding register
        View << txt
      end
    end

    def self.get key='0', options={}

      # Default key, so grab from killring

      key = key.to_s
      val =
        if key == '0'
          $el.current_kill(0) rescue nil
        else
          @@hash[key]
        end
      if options[:add_linebreak]
        val = "#{val}\n" unless val[/\n$/]
      end

      val
    end

    def self.[] key
      self.get key.to_s
    end

    def self.[]= key, to
      self.set key.to_s, to
    end

    def self.display
      @@hash.each do |k, v|
        insert [k, v].to_s
      end
    end

    def self.hash
      @@hash
    end

    def self.hash_by_first_letter
      @@hash_by_first_letter
    end

    def self.list
      $el.switch_to_buffer "*clipboard*"
      $el.erase_buffer
      Notes.mode

      Clipboard.hash.sort.each do |a, b|
        $el.insert "| #{a}\n#{b}\n\n"
      end
      $el.beginning_of_buffer
    end

    def self.set loc, txt #, append=nil
      loc = loc.to_s

      # If not standard clipboard, store in hash...

      if loc != "0"
        return @@hash[loc] = txt
      end

      # Save in corresponding register (or append if prefix)
      if $el.buffer_file_name
        # Store as tree snippet
        @@hash["="] = FileTree.snippet :txt=>txt
      end
      $el.kill_new txt
      $el.x_select_text txt if loc == "0" && Environment.gui_emacs  # If 0, store in OS clipboard
    end

    def self.do_as_snake_case
      Keys.prefix_times.times do
        word = Line.symbol(:delete => true)
        $el.insert TextUtil.snake_case(word)
        Move.forward
      end
    end

    def self.do_as_camel_case
      Keys.prefix_times.times do
        word = Line.symbol(:delete => true)
        $el.insert TextUtil.camel_case(word)
        Move.forward
      end
    end

    def self.do_as_upper_case
      Keys.prefix_times.times do
        word = Line.symbol(:delete => true)
        $el.insert word.upcase
        Move.forward
      end
    end

    def self.do_as_lower_case
      Keys.prefix_times.times do
        word = Line.symbol(:delete => true)
        $el.insert word.downcase
        Move.forward
      end
    end

    # Mapped to as+paragraph and as+rest
    def self.copy_rest_of_line
      left, right = View.cursor, Line.right
      Clipboard[0] = View.txt(left, right)
      Effects.blink :what=>[left, right]
    end

    def self.copy_paragraph options={}
      prefix = Keys.prefix

      if prefix == :u or options[:rest]   # If U prefix, get rest of paragraph
        left, right = View.paragraph(:bounds=>true, :start_here=>1)

      elsif prefix == :u or options[:before]   # If U prefix, get rest of paragraph
        right, left = View.paragraph(:bounds=>true, :end_here=>1)

      elsif prefix == :u or options[:rest_of_line]
        left, right = View.cursor, Line.right
      else
        if prefix   # If numeric prefix
          Line.to_left
          top = View.cursor
          Line.next prefix
          View.selection = top, View.cursor
          return
        end
        # If no prefix, get whole paragraph
        left, right = View.paragraph(:bounds=>true)
      end

      if options[:just_return]
        return [View.txt(left, right), left, right]
      end

      if options[:just_copy]
        return Clipboard[0] = View.txt(left, right)
      end

      View.selection = left, right
      Effects.blink(:left=>left, :right=>right)
    end

    def self.copy_name
      path = Files.stem
      Clipboard.set("0", path)
      View.flash "- Copied: #{path}", :times=>3
    end

    def self.diff_1_and_2
      # Compare clipboard 1 with 2
      # Unquote if several spaces and |
      View.to_buffer "1", :clear => true
      one = Clipboard["1"]
      one.gsub!(/^ +\|/, '') if one =~ /\A   +\|/
      $el.insert Clipboard["1"]

      View.to_buffer "2", :clear => true
      one = Clipboard["2"]
      one.gsub!(/^ +\|/, '') if one =~ /\A   +\|/
      $el.insert Clipboard["2"]

      $el.ediff_buffers "1", "2"
    end

    def self.as_thing

      orig = Location.new

      # If at end of space, grab as tree
      if Line.indent.length == View.column - 1
        left = Line.left
        return
      end

      # If on blank spaces, copy them
      if $el.buffer_substring($el.point-1, $el.point+1) =~ /[ \n] /
        $el.skip_chars_forward " "
        right = $el.point
        $el.skip_chars_backward " "
        left = $el.point
      else
        $el.skip_chars_forward " "
        left, right = $el.bounds_of_thing_at_point(:sexp).to_a
      end

      if Keys.prefix_u?
        left += 1
        right -= 1
      end
      Effects.blink(:left=>left, :right=>right)

      txt = View.txt(left, right)
      Clipboard.set "0", txt
      View.to right

      orig.go

    end

    def self.as_object
      set("0", $el.thing_at_point(:symbol))
      left, right = $el.bounds_of_thing_at_point(:symbol).to_a
      Effects.blink(:left=>left, :right=>right)
    end

    def self.copy_everything

      # No prefix, so make selection of everything...
      if ! Keys.prefix_u
        return View.selection = [View.top, View.bottom]
      end

      # Dash+, so don't select...

      Clipboard.set("0", $el.buffer_string)
    end

    def self.as_line many=nil
      prefix = Keys.prefix :clear=>true

      # If Dash+, copy Foo.bar from quoted line
      if prefix == :-
        txt = Tree.construct_path
        View.flash "- copied #{txt}"
        return Clipboard.set("0", txt)
      end

      # If up+, copy path of file tree cursor is on, or otherwise the current view's
      return FileTree.copy_path if prefix == :u

      many ||= prefix || 1
      left = Line.left
      right = Line.left(many+1)

      line = View.txt(left, right)

      Clipboard.set("0", line)
      Effects.blink :left=>left, :right=>right
      $el.set_mark(right)

      View.deselect
    end

    def self.enter_replacement
      # If on whitespace, move to off of it
      $el.skip_chars_forward " "

      orig = $el.point
      Move.to_other_bracket
      View.delete orig, $el.point
      View.insert Clipboard['0']
    end

    def self.as_clipboard
      prefix = Keys.prefix :clear=>true
      if prefix == 0
        l, r = View.paragraph :bounds=>true
        Effects.blink :left=>l, :right=>r
        cursor = View.cursor
        View.cursor = l
        Location.as_spot('clipboard')
        Clipboard["0"] = View.txt(l, r)
        View.cursor = cursor
        return
      end

      Location.as_spot('clipboard')

      # If numeric prefix, get next n lines and put in clipboard
      if prefix.is_a?(Fixnum)
        l, r = Line.left, Line.left(prefix + 1)
        Effects.blink :left=>l, :right=>r
        Clipboard["0"] = View.txt(l, r)

        View.set_mark(r)
        return
      end

      Clipboard.copy("0")
    end

    def self.enter_yank
      ch = Keys.input :chars=>1
      value = @@hash_by_first_letter[ch]
      return unless value
      View.insert value
    end

    def self.whipe
      $el.kill_region $el.mark, $el.point
    end

    def self.kill
      prefix = Keys.prefix # :clear=>1

      # up+, so delete current line
      prefix = 1 if prefix == :u

      if prefix == 0
        # Delete the blank line, if nothing here
        return Line.delete if Line.blank?
        return Line.delete(:leave_linebreak)
      end

      if prefix # == :u
        Line.to_left
      end

      # up+, so just delete (don't save in clipboard)

      $el.kill_line prefix
    end

    def self.yank
      $el.yank
    end

    def self.select

      # Text already selected, so just just jump to other side...

      $el.exchange_point_and_mark

      if $el.elvar.mark_active
        return
      end

      # up+, so just re-select what was selected last...

      if Keys.prefix_u
        return $el.exchange_point_and_mark
      end

      right = View.cursor
      View.cursor = $el.mark
      left = View.cursor
      View.selection = [right, left]
      return

      # Nothing selected yet, so just select

    end

    def self.init_in_client

      # Make C-c, C-x, and C-v use the os clipboard...

      from_os_code, to_os_code = nil, nil

      if Environment.xsh? && Environment.os == "osx"
        from_os_code = '"pbpaste"'
        to_os_code = '"copy_from_osx" "*Messages*" "pbcopy"'
      end

      if Environment.xsh? && Environment.os == "linux"
        txt, error = Shell.sync("xclip -o -selection clipboard", :return_error=>1)
        # Only use xclip if it exists and it's not returning an error
        if ! error
          from_os_code = '"xclip -o -selection clipboard"'
          to_os_code = '"xclip" "*Messages*" "xclip" "-selection" "clipboard"'
        end
      end

      return if ! from_os_code

      $el.el4r_lisp_eval %`
        (progn
          (defun copy-from-osx ()
            (shell-command-to-string #{from_os_code}))
          (defun paste-to-osx (text &optional push)
            (let ((process-connection-type nil))
              (let ((proc (start-process #{to_os_code})))
                (process-send-string proc text)
                (process-send-eof proc)))
            (when (el4r-running-p)
              (let ((xiki-clipboard-txt text))
                (el4r-ruby-eval "Xiki::Clipboard.hash['='] = Xiki::FileTree.snippet :txt=>$el.elvar.xiki_clipboard_txt")
                )
              )
            )
          (setq interprogram-cut-function 'paste-to-osx)
          (setq interprogram-paste-function 'copy-from-osx)
        )
      `

    end


    # Clipboard.register "1"   < Returns register 1
    # Clipboard.register "1", "foo"   < Sets register 1
    def self.register key, value=nil
      # Fix keys ("1" to 49, etc)...

      key = key[0].sum if key.is_a? String

      # Just key, so return the register...

      return $el.get_register key if ! value

      # key, value, so set register

      $el.set_register key, value
      nil

    end


    def self.cua_rectangle_advice

      left, right = View.range

      txt = View.delete left, right

      final_spaces = txt[/ +\z/]   # Grab spaces on last line
      txt.gsub!(/ +$/, '')   # Remove all trailing
      txt << final_spaces if final_spaces   # Put last ones back

      View << txt

    end


    def self.cua_paste_advice

      # Not .notes file, so do nothing
      return if View.extension != "xiki"

      # Remember where pasted
      left, right = View.range

      # Cursor not at end of line (which means we pasted in the middle of a line), so do nothing
      return if right != Line.right

      line = View.line

      View.cursor = left   # Move to beginning of paste

      # Text we're pasting doesn't have at least one linebreak, so do nothing
      return View.cursor = right if line == View.line

      # No indent or "|..." before the cursor, so go back and do nothing
      before_cursor = Line.before_cursor
      indent = Line.indent before_cursor

      quoted_indent = before_cursor[/^ *[|:] /]

      after_bullet = before_cursor =~ /^ *[+-] /

      return View.cursor = right if indent == "" && ! quoted_indent && ! after_bullet

      # Pull out pasted, quote it (or just indent), and put it back
      txt = View.delete left, right

      # Unquote if quoted, or just unindent
      txt =~ /^ *[|:] / ?
        Tree.unquote!(txt) :
        txt.unindent!

      # txt_indent = Line.indent txt
      quoted_indent ?
        txt.gsub!(/^/, quoted_indent) :
        txt.gsub!(/^/, indent)

      # Line is just a blank quote, so remove indenting or quote from 1st line
      if before_cursor =~ /^ *([|:] )?$/
        txt.sub! /^ *([|:] )?/, ""

      else   # Line already has stuff after the quote, so add linebreak

        View << "\n"
        txt.sub!(/\n\z/, '')   # And > remove last linebreak
      end

      # "- foo" bullet, so indent one level lower
      txt.gsub!(/^/, "  ") if after_bullet

      View << txt

    end

  end
end
