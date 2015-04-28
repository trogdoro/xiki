require 'xiki/core/styles'
require 'xiki/core/line'
require 'xiki/core/effects'
require 'xiki/core/view'
require 'xiki/core/keys'
require 'xiki/core/clipboard'

module Xiki
  class Notes

    LABEL_REGEX = /(?:[a-zA-Z0-9 _-]+\) )?/

    def self.list *args
      Notes.drill "~/xiki/notes/", *args
    end

    # Returns contents block where cursor is.
    # Blocks are delimited by >... headings.
    # Notes.block
    def self.block regex="^> "
      left, after_header, right = View.block_positions regex
      View.txt after_header, right
    end

    def self.narrow_block options={}

      delimiter = options[:delimiter] || ">"

      # If nothing hidden, hide all but current
      if $el.point_min == 1 && ($el.buffer_size + 1 == $el.point_max)
        left, after_header, right = View.block_positions "^#{delimiter}\\( \\|$\\)"
        Hide.hide left, right
        return
      end
      # Otherwise, expand all, go to next heading, hide all but current
      $el.widen
      Notes.to_block
      left, after_header, right = View.block_positions "^#{delimiter}\\( \\|$\\)"
      $el.narrow_to_region left, right
    end

    def self.archive
      block = self.current_section
      block.archive
    end

    def self.show_text
      block = self.current_section
      block.show_text
    end

    def self.hide_text
      block = self.current_section
      block.hide_text
    end

    def self.to_block options={}
      prefix = Keys.prefix :clear=>1

      kind = :h1
      kind = :h0 if prefix == :u
      regex = self.heading_regex kind

      times = prefix.is_a?(Fixnum) ? prefix : 1

      if options[:up]
        times.times do
          Line.to_left
          Search.backward regex
        end
      else
        times.times do
          Line.next if $el.string_match regex, Line.value   # Use elisp matcher
          Search.forward regex
          Line.to_left
        end
      end

      # Run from the key shortcut, so remember it for C-,
      if options[:key]
        prock = options[:up] ? proc {Xiki::Notes.to_block :up=>1} : proc {Xiki::Notes.to_block}
        Keys.remember_key_for_repeat(prock, :movement=>1)
      end
    end

    #
    # Returns type of heading (:h1, :h2, etc.) given a key
    # argument.  Assumes h0 if you're on a >...: line.
    #
    # Notes.heading_kind
    #
    def self.heading_kind kind=nil
      line_override = Line.value if kind.nil?   # Only use heading like current line if none was passed

      if kind == :h2 || kind == :uu || line_override =~ /^>>( |$)/
        :h2
      elsif kind == :h0 || line_override =~ /^> .*:$/
        :h0
      else   # Probably nil or 1
        :h1
      end
    end

    #
    # :match_only_self - return regex that only matches same type of heading
    #
    def self.heading_regex kind=nil, options={}
      kind = self.heading_kind kind
      if kind == :h1
        options[:match_only_self] ?
          "^[>=]\\( .*[^\n:]$\\| ?$\\)" :
          "^[>=]\\( \\|$\\)"
      elsif kind == :h2
        "^[>=]\\{1,2\\}\\( \\|$\\)"
      elsif kind == :h0
        "^> .*:$"
      end
    end

    def self.move_block up=false

      Keys.remember_key_for_repeat(proc {Notes.move_block up})

      prefix = Keys.prefix :clear=>1
      kind = self.heading_kind
      times = Keys.prefix_times prefix
      regex = self.heading_regex kind #, :match_only_self=>1

      if kind == :h0
        regex = self.heading_regex :h0
        prefix = 0
      end

      orig = Location.new

      section = self.current_section prefix
      section.blink if kind == :h0 || kind == :h2

      section.delete

      if up
        times.times do
          Search.backward regex, :go_anyway=>true
        end

        $el.insert section.content
        Search.backward regex
      else

        move_regex = prefix == :u ? "^>" : regex

        Search.forward move_regex
        times.times do
          Search.forward move_regex, :go_anyway=>true
        end
        Move.to_axis

        View.insert section.content
        Search.backward move_regex
      end
      times == 1 ?
        self.current_section(regex).fade_in :
        orig.go

    end

    def self.insert_heading options={}

      Keys.remember_key_for_repeat(proc {Xiki::Notes.insert_heading})

      Line.start
      orig = Line.value

      times = Keys.prefix_n || 1
      prefix = options[:prefix] || Keys.prefix(:clear=>1)
      times = 1 if times > 5

      # Put comment above line
      Move.to_axis
      View.insert "#{">"*times} "

      linebreaks = 4
      linebreaks = 1 if orig.any? && orig !~ /^>/   # If on non-blank line, and not heading, don't put space after heading

      if prefix == :u
        View << ":"#, :dont_move=>1

        linebreaks = 1 if orig =~ /^>(.*[^:\n]|)$/   # If on ">..." line, don't add space after this ">...:" heading

        View.insert "\n"*linebreaks, :dont_move=>1
        View.column = -1
        return
      end

      if prefix == nil
        View.insert "\n"*linebreaks, :dont_move=>1
      end

      if prefix == 1
        View << "!:"#, :dont_move=>1
        View.insert "\n"*4, :dont_move=>1
        View.column = -2
        return
      end

      if prefix == 8
        View.insert("\n"*4, :dont_move=>1)
        View << "today > :"#, :dont_move=>1
        Move.to_column -1
        ControlLock.disable
        return
      end

      if prefix == 9
        View.insert("\n"*2, :dont_move=>1)
        View << "feature > :"#, :dont_move=>1
        Move.to_column -1
        ControlLock.disable
        return
      end

      if prefix == :-
        View.insert("\n", :dont_move=>1) if orig != ""
      end

      raise "- Don't understand the prefix: #{prefix.inspect}" if prefix

    end

    def self.cut_block no_clipboard=false

      Keys.remember_key_for_repeat(proc {Xiki::Notes.cut_block})

      block = self.current_section Keys.prefix

      block.blink

      unless no_clipboard
        Clipboard.set("0", block.content)
      end
      block.delete
    end

    def self.copy_block
      prefix = Keys.prefix
      block = self.current_section prefix == :u ? 2 : 1
      block.blink
      Clipboard.set("0", Keys.prefix_u ? block.text : block.content)
    end

    def self.move_block_to_top no_clipboard=false

      prefix_u = Keys.prefix_u :clear=>true
      block = self.current_section

      if prefix_u
        line = View.line_number
        scroll = View.scroll_position
        orig_right = block.right
      end

      block.fade_out unless prefix_u

      block.delete

      $el.beginning_of_buffer
      $el.insert block.content

      if prefix_u
        View.to_line line
        View.scroll_position = scroll
        View.cursor = orig_right
        Line.next
      else
        View.to_line 1
      end

      moved_section = self.current_section

      moved_section.fade_in unless prefix_u
    end

    def self.keys

      # Get reference to map if already there (don't mess with existing buffers)
      $el.elvar.notes_mode_map = $el.make_sparse_keymap unless $el.boundp :notes_mode_map

      Xiki.def("custom+next", :map=>:notes_mode_map){ Notes.to_block :key=>1 }
      Xiki.def("custom+previous", :map=>:notes_mode_map){ Notes.to_block :up=>1, :key=>1 }

      Xiki.def("custom+copy", :map=>:notes_mode_map){ Notes.copy_block }
      Xiki.def("custom+xut", :map=>:notes_mode_map){ Notes.cut_block }
      Xiki.def("custom+delete", :map=>:notes_mode_map){ Notes.cut_block :no_clipboard }
      Xiki.def("custom+zoom", :map=>:notes_mode_map){ Notes.zoom }

      Xiki.def("custom+back", :map=>:notes_mode_map){ Notes.move_block :backwards }
      Xiki.def("custom+forward", :map=>:notes_mode_map){ Notes.move_block }
      Xiki.def("custom+top", :map=>:notes_mode_map){ Notes.move_block_to_top }

      Xiki.def("custom+heading", :map=>:notes_mode_map){ Notes.insert_heading }

      $el.define_key(:notes_mode_map, $el.kbd("<double-mouse-1>"), :notes_mouse_double_click)
      $el.define_key(:notes_mode_map, $el.kbd("<mouse-1>"), :notes_mouse_toggle)

      $el.define_key(:notes_mode_map, $el.kbd("<M-mouse-1>"), :notes_mouse_double_click)
      $el.define_key(:notes_mode_map, $el.kbd("<S-mouse-1>"), :notes_mouse_double_click)
      $el.define_key(:notes_mode_map, $el.kbd("<C-mouse-1>"), :notes_mouse_double_click)

      # custom+N, to jump to nth visible label
      # custom+1, custom+2, etc
      (1..9).each do |n|
        $el.define_key(:notes_mode_map, $el.kbd("C-c C-#{n}")){ Launcher.do_last_launch :nth=>n, :here=>1, :dont_launch=>1 }
      end

      # For now, don't over-ride return > it's messing up pasting in the terminal
      #       $el.define_key(:notes_mode_map, $el.kbd("C-m")) { View.insert_line }

      $el.define_key(:notes_mode_map, $el.kbd("C-i")) { Notes.tab_key }

      $el.define_key(:notes_mode_map, $el.kbd("C-m")) { Notes.return_wrapper }

    end

    def self.define_styles

      raise "maybe put this line back:" if ! $el

      # - foo (r): <here>
      Styles.define :variable, :face => 'verdana'

      # >...
      h1_size = "+3"

      if Styles.dark_bg?
        bg = "555"
        pipe = "666"
      else
        bg = "909090"
        pipe = "777"
      end

      label = "bbb"

      Styles.define :notes_h1, :face=>'arial', :size=>h1_size, :fg=>'ccc', :bg=>bg, :bold=>nil
      Styles.define :notes_h1_pipe, :face=>'arial', :size=>h1_size, :fg=>pipe, :bg=>bg, :bold=>true
      Styles.define :notes_h1_label, :face=>'arial', :size=>h1_size, :fg=>label, :bg=>bg, :bold=>true

      Styles.define :notes_h1_agenda_pipe, :face => 'arial', :size => h1_size, :fg => '88cc88', :bg => '336633', :bold =>  true
      Styles.define :notes_h1_agenda, :face => 'arial', :size => h1_size, :fg => 'ffffff', :bg => '336633', :bold => true

      # >>>...
      Styles.define :notes_h3,
        :face => 'arial', :size => "-1",
        :fg => '999',#, :bg => "9999cc",
        :bold =>  true
      Styles.define :notes_h3_pipe,
        :face => 'arial', :size => "-1",
        :fg => '333'

      # >>>>...
      Styles.define :notes_h4,
        :face => 'arial', :size => "-3",
        :fg => '55b',
        :bold =>  true
      Styles.define :notes_h4_pipe,
        :face => 'arial', :size => "-3",
        :fg => '224'


      if Styles.dark_bg?   # If black and white
        label_color = "e70"
      else
        label_color = "f70"
      end

      # Labels, emphasis
      Styles.define :notes_label,
        :face=>'arial black', :size=>"0",  # Mac
        :fg=>label_color, :bold=>true

      Styles.define :notes_bullet_parens,
        :face => 'arial', :size => "-2",
        :fg => "ee7700", :bold => true

      # Styles.define :hidden, :fg=>"777", :bg=>"777"
      Styles.define :hidden, :fg=>"333", :bg=>"333"

      # Strikethrough
      Styles.define(:strike, :strike=>true)

      Styles.define :notes_g, :fg=>"6cf", :face=>'arial black', :size=>"0", :bold=>true
      Styles.define :notes_blue, :fg=>"69f", :face=>'arial black', :size=>"0", :bold=>true
      Styles.define :notes_red, :fg=>"c55", :face=>'arial black', :size=>"0", :bold=>true
      Styles.define :notes_yellow, :fg=>"cb0", :face=>'arial black', :size=>"0", :bold=>true
      Styles.define :notes_green, :fg=>"3c3", :face=>'arial black', :size=>"0", :bold=>true

      bg_color = Styles.attribute(:default, :background)

      # =commit/White bg > make faces more compatible with it

      if Styles.dark_bg?   # If black bg
        Styles.define :notes_h2, :face=>'arial', :size=>"-1", :fg=>'999'
        Styles.define :notes_h2_pipe, :face=>'arial', :size=>"-1", :fg=>pipe
        Styles.define :notes_h0_pipe, :face=>'arial', :size=>"+8", :fg=>pipe, :bg=>bg, :bold=> true
        Styles.define :notes_h0, :fg=>"fff", :bg=>bg, :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h0_green, :fg=>"8f4", :bg=>bg, :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h0_green_pipe, :fg=>pipe, :bg=>bg, :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h1_green, :fg=>"8f4", :bg=>bg, :face=>'arial', :size=>"+3", :bold=>nil
        Styles.define :notes_h1_green_pipe, :fg=>pipe, :bg=>bg, :face=>'arial', :size=>"+3", :bold=>true

        Styles.define :escape_glyph, :fg=>"666"   # Red special char was too ugly
        Styles.define :trailing_whitespace, :bg=>'555'

      else   # If white bg

        Styles.define :notes_h2, :face=>'arial', :size=>"-1", :fg=>'fff'
        Styles.define :notes_h2_pipe, :face=>'arial', :size=>"-1", :fg=>'b0b0b0'
        Styles.define :notes_h0_pipe, :face=>'arial', :size=>"+8", :fg=>'b0b0b0', :bg=>"909090", :bold=>true
        Styles.define :notes_h0, :fg=>"fff", :bg=>"909090", :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h0_green, :fg=>"af0", :bg=>"909090", :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h0_green_pipe, :fg=>"b0b0b0", :bg=>"909090", :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h1_green, :fg=>"af0", :bg=>"909090", :face=>'arial', :size=>"+3", :bold=>1
        Styles.define :notes_h1_green_pipe, :fg=>"b0b0b0", :bg=>"909090", :face=>'arial', :size=>"+3", :bold=>true

        Styles.define :escape_glyph, :fg=>"999"   # Red special char was too ugly
        Styles.define :trailing_whitespace, :bg=>'ccc'
      end

      Styles.define :quote_hidden, :fg=>bg_color

      if Styles.dark_bg?   # If black bg
        Styles.dotted :bg=>'080808', :fg=>'111', :strike=>nil, :underline=>nil, :border=>['111', -1]
      else
        Styles.dotted :bg=>'eee', :fg=>'ddd', :strike=>nil, :underline=>nil, :border=>['ddd', -1]
      end

      notes_exclamation_color = Styles.dark_bg? ? "4c4" : "5a0"

      Styles.define :notes_exclamation,  # Green bold text
        :face=>'arial black', :size=>"0",
        :fg=>notes_exclamation_color, :bold=>true

      Styles.define :notes_link, :fg=>(Styles.dark_bg? ? "39b" : "08f"), :bold=>false

      Styles.define :shell_prompt, :fg=>'#888', :bold=>1

    end

    def self.apply_styles

      # Don't format quotes (it overrides the following formatting)
      Styles.clear

      # >... lines (headings)
      Styles.apply("^\\(>\\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)

      Styles.apply("^\\(> \\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
      Styles.apply("^\\(>> \\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)

      # > Green!
      Styles.apply("^\\(> \\)\\(.*!\\)\\(\n\\)", nil, :notes_h1_green_pipe, :notes_h1_green, :notes_h1_green)

      # > Large:
      Styles.apply("^\\(> \\)\\(.*:\\)\\(\n\\)", nil, :notes_h0_pipe, :notes_h0, :notes_h0)

      Styles.apply("^\\(> \\)\\(.*!:\\)\\(\n\\)", nil, :notes_h0_green_pipe, :notes_h0_green, :notes_h0_green)

      Styles.apply("^\\(>\\)\\( .+?: \\)\\(.+\n\\)", nil, :notes_h1_pipe, :notes_h1_label, :notes_h1)


      # >>... lines
      Styles.apply("^\\(>>\\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)

      # Commented
      Styles.apply("^\\(>> .+?: \\)\\(.+\n\\)", nil, :notes_h2_pipe, :notes_h2)

      # >>>... lines
      Styles.apply("^\\(>>>\\)\\(.*\n\\)", nil, :notes_h3_pipe, :notes_h3)

      # >>>... lines
      Styles.apply("^\\(>>>>\\)\\(.*\n\\)", nil, :notes_h4_pipe, :notes_h4)

      # foo: > style like dir
      Styles.apply("\\(^[\A-Za-z0-9 ][\A-Za-z0-9 '_.,>?-]*[\A-Za-z0-9.?]:\\)\\($\\| \\)", nil, :ls_dir)   # foo:
      Styles.apply("\\(^[\A-Za-z0-9 ]:\\)\\($\\| \\)", nil, :ls_dir)   # f:   < just 1 letter

      #    >... headings (indented)
      Styles.apply("^ +\\(> ?\\)\\(.*\n\\)", nil, :quote_heading_bracket, nil)
      Styles.apply("^ +\\(> ?\\)\\(.*!\n\\)", nil, :quote_heading_bracket, :quote_heading_h1_green)

      # - bullets with labels and comments


      # Todo > instead of having both here > make one syntax that catches them both
      # - only match when: 1st char after space can't be [>|]
      #   - probably > 1st char after space has to be alphanumeric

      Styles.apply("^[ \t]*\\([<+-][<+=-]*\\) \\([^/:\n]+:\\) ", nil, :ls_bullet, :ls_dir)   # - foo: bar

      Styles.apply("^[ \t]*\\([<+-][<+=-]*\\) \\([^(\n]*?)\\)\\( \\|$\\)", nil, :ls_bullet, :notes_label)   # - foo) bar

      Styles.apply("^[ \t]*\\([a-z][^:|(\n]*)\\)\\( \\|$\\)", nil, :notes_label)   # - foo) bar

      Styles.apply("^[ \t]*\\([<+-][<+=-]*\\) \\(.*:\\)$", nil, :ls_bullet, :notes_label)   # - foo:


      Styles.apply("^\\([ \t]*\\)\\([<+-]\\) \\(.+?:\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)
      Styles.apply("^\\([ \t]*\\)\\([<+-]\\) \\([^(\n]+?)\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)

      Styles.apply("^ *\\(!.*\n\\)", nil, :ls_quote)   # ^!... for code

      # exclamation! / todo
      Styles.apply("^[ \t]*\\([<+-]\\) \\(.*!\\)$", nil, :ls_bullet, :notes_exclamation)
      Styles.apply("^ +\\(!\\+.*\n\\)", nil, :diff_green)   # Whole lines
      Styles.apply("^ +\\(!-.*\n\\)", nil, :diff_red)

      Styles.apply("\\(\(-\\)\\(.+?\\)\\(-\)\\)", nil, :diff_small, :diff_red, :diff_small)
      Styles.apply("\\(\(\\+\\)\\(.+?\\)\\(\\+\)\\)", nil, :diff_small, :diff_green, :diff_small)

      # google/
      Styles.apply "^ *\\(-?\\) ?\\([@=]?\\)\\(g\\)\\(o\\)\\(o\\)\\(g\\)\\(l\\)\\(e\\)\\(/\\)", nil, :ls_bullet, :ls_dir,   # google
        :notes_blue, :notes_red, :notes_yellow, :notes_blue, :notes_green, :notes_red,
        :ls_dir

      Styles.apply "^hint/.+", :fade6

      # $..., %..., etc.
      Styles.apply "^[< ]*[@=]? ?\\([%$&!]\\)\\( \\|$\\)", nil, :shell_prompt   # Colorize shell prompts after "@"

      # $$...
      Styles.apply "^[< ]*[@=]? ?\\(\\$\\$\\)", nil, :shell_prompt   # Colorize shell prompts after "@"

      # Make |~... lines be Dotsies
      Styles.apply("^ *\\(|~\\)\\([^\n~]+\\)\\(~?\\)", nil, :quote_heading_pipe, :dotsies, :quote_heading_pipe)

      # |#... invisible
      Styles.apply("^ *\\(|[#*].*\n\\)", nil, :hidden)

      Styles.apply "^ *|\\^.*\n", :quote_medium

      Styles.apply("^ *\\(<\\) \\(.*\n\\)", nil, :ls_bullet, :ls_quote)   # <...

      # Experimental
      Styles.apply("^ *\\(<[a-z]+\\) \\(.*\n\\)", nil, :ls_bullet, :ls_quote)   # <...
    end

    # Startup
    def self.init

      $el.defun(:notes_mouse_meta_click, :interactive => "e") do |e|
        $el.mouse_set_point(e)
        View.insert "hey"
      end

      $el.defun(:notes_mouse_double_click, :interactive => "e") do |e|
        Launcher.double_click
      end

      $el.defun(:notes_mouse_toggle, :interactive => "e") do |e|
        $el.mouse_set_point(e)
        Notes.mouse_click
      end

      $el.defun(:notes_mode, :interactive => "", :docstring => "Apply notes styles, etc") {# |point|
        $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"

        FileTree.apply_styles
        Notes.apply_styles
        FileTree.apply_styles_at_end
        $el.use_local_map $el.elvar.notes_mode_map
        Xsh.chdir_when_xsh_session
        View.tab_width 12
      }
      $el.el4r_lisp_eval %q<
        (progn
          (add-to-list 'auto-mode-alist '("\\\\.notes\\\\'" . notes-mode))
          (add-to-list 'auto-mode-alist '("\\\\.xik\\\\'" . notes-mode))
          (add-to-list 'auto-mode-alist '("/conf/.*\\\\.conf\\\\'" . notes-mode))
        )
        >
    end

    def self.mode
      $el.notes_mode
    end

    def self.enter_label_bullet
      Line.to_left
      View.insert "- : "
      Move.backward 2
    end

    def self.enter_junior

      prefix = Keys.prefix :clear=>1

      Move.to_end if Line.before_cursor =~ /^ +$/   # If at awkward position, move

      cursor = View.cursor
      line = Line.value
      indent = Line.indent line

      prepend = line[/^ *([!|:#] +)/, 1]
      prepend ||= ""

      if Line.left == cursor || Line.right == cursor   # If beginning or end, leave current line alone
        Move.to_end
      else   # In middle of line
        Deletes.delete_whitespace
      end

      if prepend =~ /^[|:] /
        prefix = prefix == :u ? nil : :u
      end

      prepend << "  " if prefix == :u
      prepend = "  #{prepend[/. /]}" if prefix == nil   # up+, so make quote char be indented

      prepend = "  " if prefix == 0   # up+ means to not carry over prepends etc.
      prepend = "  | " if prefix == 1
      prepend = "  : " if prefix == 2
      prepend = "  # " if prefix == 3
      prepend = "  - " if prefix == :-

      View << "\n#{indent}#{prepend}"
    end

    def self.bullet bullet_text="- "
      prefix = Keys.prefix :clear=>true

      if prefix == :u
        Move.forward if Line.at_right?
        return Tree.collapse_upward
      end

      if prefix == :uu
        return Tree.collapse_upward :replace_parent=>1
      end

      line = Line.value
      indent = Line.indent indent

      if line.present?   # If non-blank line
        column = View.column

        Move.to_end if line =~ /^ / && (column-1) <= indent.length   # If just entered a bullet, go to end first
        Move.to_end if line =~ /^>/ || Line.at_left?   # If line is heading, or if at beginning of the line

        if View.cursor != Line.right
          Deletes.delete_whitespace
        end
        View.insert "\n"

        # Do simple case if quoted
        return View.<<("#{line[/^[ :|#]*/]}  - ") if line =~ /^ *[:|#]/

        # Do simple case if on heading
        return View.<<("- ") if line =~ /^>/
      end

      if prefix.is_a? Fixnum   # If numeric prefix, indent by n
        View.insert((" " * (prefix*2)) + bullet_text)
      else   # Get bullet indent of previous line
        prev = Line.value(0)
        prev_indent = prev[/^( *)/]

        # Indent further, unless it we're doing bullets and not following bullet
        prev_indent << "  " if line != "" && (bullet_text != "- " || prev =~ /^ *[+-]/ || prev !~ /^>/)
        prev_indent = "#{prev_indent}#{bullet_text}"
        View.insert prev_indent

        if prefix == :-
          View.insert "(): "
          Move.backward 3
        end
      end

      return if bullet_text == ""   # Don't indent rest if not using bullets (enter_junior)

      following_line = Line.value(2)
      return if following_line !~ /^ /   # Don't indent rest of lines if at left margin
      return if Line.matches /^[ -]*$/   # Exit if new bullet was blank

      indent = Line.indent

      return if indent.empty?   # Don't indent anything if new bullet wasn't indented

      cursor = View.cursor
      Line.next
      top = View.cursor

      # Find next line not indented underneath
      Search.forward "^ \\{0,#{indent.size-1}\\}\\($\\|[^ \n]\\)", :beginning=>true
      $el.indent_rigidly top, Line.left, 2
      View.cursor = cursor

    end

    def self.mouse_click

      # Clicked "+" or "-" on a "+ foo..." line, so expand or collapse

      # If next line is indented more, kill children
      # If starts with plus or minus, and on plus or minus, launch
      if Line.matches(/^\s*[+-]/) and View.char =~ /[+-]/
        # Temp > disable plus bullet
        plus_or_minus = Tree.toggle_plus_and_minus
        if ! Tree.children?

          $el.deactivate_mark   # So aquamacs doesn't highlight something after double-click

          if FileTree.dir? or ! FileTree.handles?   # If on a dir or code_tree
            Launcher.launch
          else   # If on a file in a FileTree
            FileTree.enter_lines
          end

        else   # If -, kill under
          Tree.collapse
          Line.to_beginning
        end
        return
      end

      # Clicked at same point as last click, so treat as double-click...

      @@last_click_position ||= nil   # Undefined when the 1st time run
      @@last_click_time ||= 0   # Undefined when the 1st time run
      if @@last_click_position == View.cursor && (Time.now.to_f - @@last_click_time < 0.7)
        @@last_click_position, @@last_click_time = nil, 0
        Launcher.double_click
      else

        # Store position of last click

        @@last_click_position, @@last_click_time = View.cursor, Time.now.to_f
      end

    end

    #
    # Returns an instance of Section representing the block the point
    # is currently in.
    #
    #   def self.current_section regex="^[|>]\\( \\|$\\)"
    def self.current_section regex=nil
      regex = self.heading_regex if ! regex || ! regex.is_a?(String)

      left, after_header, right = View.block_positions regex
      Section.new(left, after_header, right)
    end

    def self.to_html txt
      txt = txt.
        gsub(/^> (.+)/, "<h1>\\1</h1>").
        gsub(/(^|[^\n>])$/, "\\0<br>")
    end

    def self.as_file

      prefix = Keys.prefix :clear=>true
      txt = ""

      if prefix == :u || prefix == :uu
        txt = Code.grab_containing_method
      end

      label = nil

      Effects.blink(:what=>:line)

      was_visible = View.file_visible? Bookmarks[':n']

      if prefix == :u
        label = Keys.input :prompt=>"label: "
        label = "do" if label.blank?
        label = Notes.expand_if_action_abbrev(label) || label

        # Only add ")" at end of label if not "!" at end
        label = "#{label})" if label !~ /!$/

        prefix = nil
      end

      if prefix == :uu   # up+up means add function and line
        txt << "\n#{Line.value}"
      elsif prefix != :u
        txt = View.txt_per_prefix prefix, :selection=>1, :default_is_line=>1, :just_txt=>1
      end

      orig = Location.new

      # If file has bullet or ends with slash, grab path

      options = {}

      keep_tweeking = true
      if ! prefix && FileTree.handles?   # Grab tree

        txt = Tree.construct_path :raw=>1
        # Remove ##... if there
        txt.delete_if{|o| o =~ /^##/}

        options[:path] = txt[0..-2].join
        if Search.fit_in_snippet(txt[-1], :path=>txt[0..-2].join)   # Insert it in existing tree if there
          View << "    - #{label}\n" if label
          return orig.go
        end

        txt = "#{txt[0..-3].join}\n  - #{txt[-2]}\n    #{txt[-1]}\n"
        keep_tweeking = false
      end

      file = View.file

      if Search.fit_in_snippet(txt)   # Insert it in existing tree if there
        View << "    - #{label}\n" if label

        orig.go if was_visible   # Go to original place, if :n was already visible
        return
      end

      # Make it quoted, unless already a quote
      if keep_tweeking && (txt !~ /\A([+-] |\/)/ || txt !~ /^ +/)   # If txt isn't already a tree, make it one
        txt = FileTree.snippet :txt=>txt, :file=>file
        txt.sub! /^    /, "    - #{label}\n    " if label
      end

      # Else, add it to top...

      View.to_highest

      if prefix == 6   # Only move under existing >...: header if 6+
        Line.next if Line =~ /^> .*:$/   # If at >...: line, move after it
      end

      if prefix == 8
        if Line =~ /^>/
          Line.next
        end
        result = "#{txt}\n"
      else
        result = ">\n#{txt}\n"
      end

      View.<< result, :dont_move=>1
      Line.next 3

      orig.go if was_visible   # Go to original place, if :n was already visible
    end


    def self.as_todo

      prefix = Keys.prefix :clear=>1

      # Nothing was selected...

      txt = nil

      # Something was selected, so use it...

      selection = View.selection

      # If method, make it Foo.bar method call
      line = Line.value

      Effects.blink(:what=>:line)

      if ! selection && View.file =~ /_spec.rb/ && line =~ /^ *(it|describe) /
        return Specs.enter_as_rspec
      end

      buffer_name = $el.buffer_name
      file_name = View.file_name
      path = Tree.path rescue nil

      if prefix == :u   # up+, so add as quoted, with "- todo!"

        txt = FileTree.snippet :txt=>Line.value, :file=>View.file
        txt.sub! /^/, ">\n"
        txt.sub! /:/, "- todo!\n    :"

      elsif ! selection && prefix.nil?   # So 1+ or numeric prefix just grab normally
        if buffer_name == "*ol"   # Make it into "foo = bar" format
          txt = line[/\) (.+)/, 1]
          txt.sub!(": ", " = ") if txt
          txt ||= line[/ *- (.+?) /, 1]

        elsif path && path.last =~ /(\w+)\.rb\/: *def ([\w\.?]+)/
          clazz = $1
          method = $2
          clazz = TextUtil.camel_case clazz if method.slice! /^self\./
          txt = "#{clazz}.#{method}"

        elsif line =~ /^ +def (.+)/   # Make it into Foo.bar format
          method = $1
          clazz = file_name[/\w+/]
          clazz = TextUtil.camel_case clazz if method.slice! /^self\./

          txt = "#{clazz}.#{method}"

        elsif line =~ /^ *[|:]/   # Make it into Foo.bar format
          txt = line.sub /^ *[|:][+-]? ?/, ''
        elsif FileTree.handles?
          txt = Tree.dir
        elsif line =~ /(^ *[+-] |\/$)/   # Make it into Foo.bar format
          txt = Tree.path.last
        elsif line =~ /^ *Ol.>> (.+?)   # => (.+)/   # Make it into Foo.bar format
          txt = "#{$1} = #{$2}"
        elsif line =~ /^ *Ol.+, (.+?)   # => (.+)/   # Make it into Foo.bar format
          txt = "#{$1} = #{$2}"
        end
      end

      # No text yet, so maybe there's a selection, or a numeric prefix...

      txt ||= selection
      txt ||= View.txt_per_prefix(prefix, :selection=>1, :just_txt=>1, :default_is_line=>1)

      txt.strip! if txt =~ /\A.+\n\z/   # Strip when only 1 linebreak

      options = prefix == :uu ? {:append=>1} : {}
      Search.move_to ":t", txt, options
    end


    class Section

      attr_accessor :left, :after_header, :right
      attr_accessor :header, :text

      def initialize(left, after_header, right)
        @left, @after_header, @right = left, after_header, right
        @header = $el.buffer_substring left, after_header
        @text = $el.buffer_substring after_header, right
      end

      def positions
        [left, after_header, right]
      end

      def content
        header + text
      end

      def to_s
        content
      end

      def blink
        Effects.blink :left => after_header, :right => right
      end

      def fade_out
        Effects.glow :fade_out=>1, :what=>[left, right]
      end

      def fade_in
        Effects.glow :fade_in=>1, :what=>[left, right]
      end

      def delete
        $el.delete_region left, right
      end

      # initialize an overlay for this notes block
      # it has a special hook that updates name to be header always
      # this way we can always find the overlay corresponding to header

      def show_text
        @header_overlay ||= Overlay.find_or_make(left, after_header - 1)

        @body_overlay ||= Overlay.find_or_make(after_header, right)
        @body_overlay.invisible = false
      end

      def hide_text
        @header_overlay ||= Overlay.find_or_make(left, after_header - 1)

        @body_overlay ||= Overlay.find_or_make(after_header, right)
        @body_overlay.invisible = true
      end

      # cuts the block, and stores it in archive.file.notes
      # example: ruby.notes -> archive.ruby.notes
      def archive
        delete
        filename = 'archive.' + $el.file_name_nondirectory(buffer_file_name)
        timestamp = "--- archived on #{Time.now.strftime('%Y-%m-%d at %H:%M')} --- \n"
        $el.append_to_file timestamp, nil, filename
        $el.append_to_file content, nil, filename
      end
    end

    def self.enter_note options={}

      if Line.blank? && Keys.prefix != :u
        return self.open_note :insert=>1
      end

      # :u prefix...

      # If on blank line, just insert it
      indent = ""
      if ! Line.blank?
        line = Line.value
        indent, first_char = line.match(/^( *)(.)/)[1..2]

        Move.to_axis
        $el.open_line 1
      end

      Line << "#{indent}- )"
      Move.backward

      txt = options[:txt] || Keys.input(:prompt=>'Your note (or a letter as a shortcut): ')

      expanded = self.expand_if_action_abbrev txt

      # If ends in exclamation, remove paren

      if expanded =~ /[:!]$/
        View.delete :char
      end

      View << (expanded || txt)

      if expanded =~ /^[!:]$/
        Move.left
      end

      if expanded
        # Do nothing
      else
        View << "#{indent}- !"
        Move.backward
        View << txt
      end

      nil
    end

    def self.drill_split args

      items = [[], [], []]   # file, heading, content

      items_index = 0   # Increase it as we move through
      args.each do |arg|
        # If content, they'll always be content
        next items[2] << arg if items_index == 2

        items_index = 1 if arg =~ /^> /   # If >..., bump up to headings
        items_index = 2 if arg =~ /^:|\n/   # If |..., bump up to content

        items[items_index] << arg
      end

      items
    end


    #
    # Makes .notes file navigable as tree.
    #
    # Example usage:
    #
    # - /projects/xiki/menu/
    #   - accounts.rb
    #     | class Accounts
    #     |   def self.menu *args
    #     |     Notes.drill ':accounts', *args
    #     |   end
    #     | end
    #
    def self.drill file, *args

      prefix = Keys.prefix :clear=>true

      file = Bookmarks[file]

      # Pull off options, to get prefix

      options = args[-1].is_a?(Hash) ? args.pop : {}
      task = options[:task]

      # Divide up path.  Could look like this:
        # dir/dir/> heading/| contents

      # Pull off contents if any (quoted lines)

      # This file_items stuff might not be necessary after the unified refactor.  Delegating to the expander should take care of dirs.

      file_items, heading, content = Notes.drill_split args

      heading = heading.any? ? heading.join("/") : nil
      content = content.any? ? content.join("/") : nil
      line_orig = Line.value
      file << file_items.join("/")+"/" if file_items.any?

      # Check for whether foo.notes or foo/foo.notes exists for this path...

      if file =~ /\/$/ && File.exists?(found = file.sub(/\/$/, ".notes"))
        file = found
      end

      if file =~ /\/$/ && File.exists?(found = file.sub(/\/$/, "/index.notes"))
        file = found
      end

      # Somewhere in here, make it look for index.html files as well

      if file_items.any? && File.exists?(found = "#{file}#{file_items[-1]}.notes")
        file = found
      end

      # If it's a dir, show dirs inside...

      if File.directory? file

        # If no topic, just show all dirs

        entries = Dir.new(file).entries
        entries = entries.select{|o| o =~ /^\w/}
        entries.each{|o| o.sub! /\..+/, ''}
        return entries.map{|o| "#{o}/"}
      end


      # If a file...

      if file =~ /^\$(\w+)/   # If bookmark that wasn't found, complain
        bm = $1
        return "| Set the following bookmark first. Then you'll be able to use this menu to\n| browse the file. The file should have '> ...' headings.\n\n@ :#{bm}\n"
      end

      if ! File.exists? file
        return "
          : File doesn't exist yet.  Type Ctrl+T to create it:
          =#{file}
            : > Heading
            : Stuff
          "
      end

      txt = File.open(file, 'rb') {|f| f.read}

      # /, so show just headings...

      if ! heading

        return "~ headings\n~ navigate" if task == []

        if task == ["navigate"]   # If as+open, just jump there
          View.open file
          return ""
        end

        txt = txt.split("\n")
        txt = txt.grep /^\>( .+)/
        return ": This file has no '>...' headings:\n=#{file}" if txt.empty?
        return txt.join("\n")  #.gsub /^> /, '| '
      end

      heading.sub!(/^\: /, '> ')
      escaped_heading = Regexp.escape heading

      # /> Heading, so nav to it, or show text under heading...

      if ! content

        return "~ navigate\n~ expand" if task == []

        # No '~ navigate' task, so always expand...

        if task != ['navigate']
          txt = self.extract_block txt, heading
          txt.sub! /\n\z/, ''

          # If blank, assume it wasn't there, and show default message...

          txt = "Create this new section by typing\nsome stuff here and expanding.\n" if txt.blank?

          # Don't do this for now, since it'll probably look ugly/confusing to new xsh users, and headings for ~/xiki/notes files navigate instead of expanding now anyway
          # Change "| foo/" to "=foo/" > add equals char for certain patterns
          #           txt = Tree.quote txt, :unquote_menus=>1, :char=>"|"

          txt = Tree.pipe "#{txt}\n"

          return txt
        end

        # No comment, so just navigate...

        View.open file
        View.to_highest
        Search.forward "^#{$el.regexp_quote heading}$", :beginning=>1
        View.recenter_top
        return ""

      end

      # /> Heading/| content, so navigate or save...

      return "~ save\n~ navigate" if task == []


      # Save, so replace or add...

      if task == ["save"]

        # Extract parts from the file that won't change
        index = txt.index /^#{escaped_heading}$/

        if index

          index += heading.length

          before = txt[0..index]

          after = txt[index..-1]   # Part we're replacing and everything afterward
          heading_after = after =~ /^> /
          after = heading_after ? after[heading_after..-1] : ""   # If no heading, we replace all

          # Find heading, if there is one

          content = Tree.siblings :children=>1   # Content we're saving
          content.gsub! /^[|:] ?|^=/, ''
          txt = "#{before}#{content}#{after}"

          DiffLog.save_diffs :patha=>file, :textb=>txt

        else   # If no index, it's not there, so append to the end

          # =commit/Notes > create notes and headings if don't exist yet, and save by default.

          content = Tree.siblings :children=>1   # Content we're saving
          content.gsub! /^[|:] ?|^=/, ''
          txt = "#{heading}\n#{content.strip}\n\n#{txt.strip}\n\n"

        end

        File.open(file, "w") { |f| f << txt }

        return "<! saved!"

      end

      # No task (or ~navigate), so navigate to heading and put cursor on line...

      View.open file
      View.to_highest
      Search.forward "^#{$el.regexp_quote heading}$"
      View.recenter_top

      line_orig.sub!(/^ *[|:]./, '')
      Search.forward "^#{$el.regexp_quote line_orig}"
      Move.to_axis

      return ""

    end


    def self.extract_block txt, heading
      txt = txt.sub /.*^#{Regexp.escape heading}\n/m, ''   # Delete before block
      txt.sub! /^>( |$).*/m, ''   # Delete after block
      txt
    end

    def self.read_block file, heading
      self.extract_block File.read(file), heading
    end

    def self.tab_key

      path = Tree.path

      # $... path, so run Xiki command for the shell command

      if path[-1] =~ /^\$( |$)/ &&   # A $... line,
        Line.at_right?   # and the cursor is at the right

        return Shell.tab
      end

      # /file/path, so just expand

      if Line.at_right? && FileTree.handles?
        return Launcher.launch
      end

      indent = Line.indent(Line.value 0)
      Line.sub! /^ */, indent
      Line.to_beginning
    end

    # If the string is "t" or "i", or a few others, return "todo" or "imprement" etc. respectively.
    def self.expand_if_action_abbrev txt
      @@single_letter_abbrev[txt] || txt
    end

    @@single_letter_abbrev = {
      " "=>"",
      "p"=>"",
      "b"=>"borrow!",
      "c"=>":",
      "d"=>"do!",
      "de"=>"delete!",
      "e"=>"!",
      "er"=>"error!",
      "f"=>"fix!",
      "fa"=>"favorite!",
      "fi"=>"finish!",
      "i"=>"implement!",
      "r"=>"rename!",
      "t"=>"todo!",
      "te"=>"test",
      "tr"=>"try",
      "u"=>"update!",
    }

    def self.do_as_quote
      # Make this add or remove quotes
    end

    def self.from_markdown_format txt
      txt = txt.gsub(/^#+/){"#{'>' * $&.length}"}
    end

    # Returns whether notes mode is enabled.
    def self.enabled?
      $el.current_local_map == $el.elvar.notes_mode_map
    end

    # Mapped to open+note.
    # Prompts for some chars, and opens "@notes/foo" where "foo"
    # matches your chars.  Matches are determined by Keys.filter().
    def self.open_note options={}

      open_or_insert = options[:insert] ? Launcher.method(:insert) : Launcher.method(:open)

      # Get user input...

      keys = Keys.input :optional=>1, :message=>"Which note? (Type a key or two): "
      return open_or_insert.call "notes/" if ! keys

      # Get possible matches - files in ~/notes without extensions...

      files = Dir.new(File.expand_path("~/xiki/notes/")).entries.grep /\A[^.]/
      files.map!{|o| o.sub /\..+/, ''}

      # Narrow down by input
      found = Keys.filter files, keys

      return open_or_insert.call "notes/" if ! found

      open_or_insert.call "notes/#{found}/"
      nil
    end

    def self.open_todo options={}

      prefix = Keys.prefix :clear=>1

      file = Bookmarks[options[:bookmark] || ":t"]

      FileUtils.mkdir_p File.dirname(file)

      View.open file, :stay_in_bar=>1

      # up+, so make heading...

      if prefix == :u
        View.to_highest
        Notes.insert_heading :prefix=>:u
        return
      end

    end

    def self.append_to_section txt

      orig = View.cursor
      Search.forward "^>", :beginning=>true
      View << ">>\ntxt\n"
      View.cursor = orig

    end

    # Delegate to View.  Should probably move .block_positions into notes.rb.
    def self.block_positions *args
      View.block_positions *args
    end

    def self.next_paren_label

      # Move forward if at beginning of line
      Move.forward if Line.at_left? && ! View.at_top?
      found = Search.forward "^ *[+-] [a-zA-Z0-9 +'.>]*)\\($\\| \\)", :beginning=>true
      return Move.backward if ! found

      # Label for the next line, so move to next line
      self.next_line_when_paren_label
    end

    def self.next_line_when_paren_label
      Line.to_next if Line[/^ *[+-] [a-zA-Z0-9 +'.>]*\)$/]   # '
    end

    def self.extract_paren_labels options={}

      txt = ""

      file, limit, indent = options[:file], options[:limit], options[:indent]

      path = View.as_file_or_temp_file(:file=>file)
      extract_from_next_line = nil
      label = nil
      IO.foreach(path, *Files.encoding_binary) do |line|
        break if limit <= 0

        # The previous line was a blank label, ...

        if label == ""
          label = line[/\w.*/]
          next if ! label
          label.sub! /\/$/, ''   # No slashes at end
          limit -= 1
          txt << "#{indent}+ #{label}\n"
          label = nil
          next
        end

        label = line[/^ *[+-] ([\w .+'>]*)\)( |$)/, 1]   # ' > label regex
        next if ! label

        if label == ""
          label = line[/[.]?\w.+/]   # - ) foo, so use 1st word as label...
          label.sub! /\/$/, '' if label   # No slashes at end
          next label = "" if ! label
        end

        txt << "#{indent}+ #{label}\n"
        limit -= 1
      end

      txt

    end


    def self.zoom
      ignore, left, right = View.block_positions "^>"   # Regex works with >\n lines
      $el.narrow_to_region left, right
    end


    def self.heading
      # Move to heading if there
      cursor = View.cursor
      found = Search.backward "^>"
      return if ! found

      # Return heading and move back
      line = Line.value
      View.cursor = cursor
      line
    end


    # Grabs the command out of our heading, if its format is "> foo/" or "> foo/ > something".
    def self.command_heading options={}

      heading = nil

      # :check_current_line, so use current line if it's a heading...

      if options[:check_current_line]
        line = Line.value
        heading = line if line =~ /^>/
      end

      # Grab heading from above

      heading ||= self.heading
      if heading =~ /^> (.+?)\/:?($| > )/   # Heading above, so return it
        return $1
      end

      if heading =~ /^> =(.+?):?($| > )/   # Heading above, so return it
        return $1
      end

      nil
    end


    def self.return_wrapper

      Keys.remember_key_for_repeat(proc {Notes.return_wrapper})

      # Add a linebreak

      line = Line.value
      View << "\n"

      # Weren't at end of a "$ foo", so do nothing else...

      return if line !~ /^\$ ./ || ! Line.at_right?

      # Var not bound yet, so sync with disk...

      if ! $el.boundp(:xiki_return_warning)
        disk_value = Conf.get "xsh", "return warning"
        # Set to disk value or default (4)
        $el.elvar.xiki_return_warning = disk_value || "4"
      end

      # Just because reading from the elisp var is slightly slower than the ruby var
      value_orig = $el.elvar.xiki_return_warning

      # 0, so just return...

      return if value_orig == "0"

      # Decrement it
      value_new = (value_orig.to_i - 1).to_s
      $el.elvar.xiki_return_warning = value_new

      # Write new value to disk

      Conf.put "xsh", "return warning", value_new

      # Flash that many times

      View.flash "- Typing return just adds a linebreak!", :times=>value_orig.to_i
      View.pause 0.6
      View.flash "- Use Ctrl+X to execute a command!", :times=>value_orig.to_i

    end

  end

  # This only runs in emacs standalone mode

  if $el
    Notes.define_styles
    Notes.init
    Notes.keys  # Define local keys
  end

  # TODO - how to turn these on conditionally?
    # What's the best approach for presentations?
      # Probably make .deck files use notes mode
        # Why wasn't working before?
  # require 'deck'
  # Deck.keys :notes_mode_map   # Temporary - only when doing presentations
end
