require 'xiki/core/styles'
require 'xiki/core/line'
require 'xiki/core/effects'
require 'xiki/core/view'
require 'xiki/core/keys'
require 'xiki/core/clipboard'

module Xiki
  class Notes

    # Marker regex
    MARKER_REGEX = /(?:[a-zA-Z0-9 '"!?*_-]+\) )?/

    def self.list *args
      self.drill "~/xiki/", *args
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
      block = self.current_section_object
      block.archive
    end

    def self.show_text
      block = self.current_section_object
      block.show_text
    end

    def self.hide_text
      block = self.current_section_object
      block.hide_text
    end

    def self.to_block options={}
      prefix = options[:prefix] || Keys.prefix #:clear=>1

      kind = :h1
      kind = :h0 if prefix == :u
      regex = self.heading_regex kind

      times = prefix.is_a?(Fixnum) ? prefix : 1

      if options[:up]

        return Ruby.custom_previous if View.mode == :ruby_mode

        times.times do
          Line.to_left
          Search.backward regex
        end
      else

        return Ruby.custom_next if View.mode == :ruby_mode

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
          "^>\\( .*[^\n:]$\\| ?$\\)" :
          "^>\\( \\|$\\)"
      elsif kind == :h2
        "^>\\{1,2\\}\\( \\|$\\)"
      elsif kind == :h0
        "^> .*:$"
      end
    end

    def self.move_block options={}

      Keys.remember_key_for_repeat(proc {Notes.move_block options})

      prefix = options[:prefix] || Keys.prefix(:clear=>1)
      kind = self.heading_kind
      times = Keys.prefix_times prefix
      regex = self.heading_regex kind #, :match_only_self=>1

      if kind == :h0
        regex = self.heading_regex :h0
        prefix = 0
      end

      orig = Location.new

      section = self.current_section_object prefix

      section.delete

      if options[:up]
        times.times do
          Search.backward regex, :go_anyway=>true
        end

        View.insert section.content
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
        self.current_section_object(regex).fade_in :
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

      if prefix == :u || prefix == :-
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

      block = self.current_section_object Keys.prefix

      # block.blink

      unless no_clipboard
        Clipboard.set("0", block.content)
      end
      block.delete
    end

    def self.copy_block
      prefix = Keys.prefix
      block = self.current_section_object prefix == :u ? 2 : 1
      Clipboard.set("0", Keys.prefix_u ? block.text : block.content)
    end

    # def self.move_block_to_top no_clipboard=false
    def self.move_block_to_top options={}

      prefix_u = Keys.prefix_u :clear=>true
      block = self.current_section_object

      if prefix_u
        line = View.line_number
        scroll = View.scroll_position
        orig_right = block.right
      end

      # Do fade while moving if > no hyphen prefix and no :no_fade option

      do_fade = ! prefix_u && ! options[:no_fade]

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

      moved_section = self.current_section_object

      moved_section.fade_in if do_fade
    end

    def self.keys

      # Get reference to map if already there (don't mess with existing buffers)
      $el.elvar.notes_mode_map = $el.make_sparse_keymap unless $el.boundp :notes_mode_map

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

      $el.define_key(:notes_mode_map, $el.kbd("C-i")) { Notes.tab_key :flashing_ok=>1 }
      $el.define_key(:notes_mode_map, $el.kbd("<backtab>")) { Notes.tab_key :prefix=>:u, :flashing_ok=>1 }

      $el.define_key(:notes_mode_map, $el.kbd("C-m")) { Notes.return_wrapper }   # Return key

      # For now, don't over-ride return > it's messing up pasting in the terminal
      #       $el.define_key(:notes_mode_map, $el.kbd("C-m")) { View.insert_line }

    end

    def self.define_styles

      Code.cache(:notes_define_styles) do

        # - foo (r): <here>
        Styles.define :variable, :face => 'verdana'

        # >...

        h1_size = "+3"
        bg = "555"

        pipe = "777"
        label = "bbb"

        Styles.define :notes_h1, :face=>'arial', :size=>h1_size, :fg=>'ccc', :bg=>bg, :bold=>nil
        Styles.define :notes_h1_pipe, :face=>'arial', :size=>h1_size, :fg=>pipe, :bg=>bg, :bold=>true

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

        label_color = "e70"


        # Labels, emphasis
        Styles.define :notes_label,
          :face=>'arial black', :size=>"0",  # Mac
          :fg=>label_color, :bold=>true

        Styles.define :notes_bullet_parens,
          :face => 'arial', :size => "-2",
          :fg => "ee7700", :bold => true

        Styles.define :hidden, :fg=>"000", :bg=>"000"

        Styles.define :notes_g, :fg=>"6cf", :face=>'arial black', :size=>"0", :bold=>true
        Styles.define :notes_blue, :fg=>"69f", :face=>'arial black', :size=>"0", :bold=>true
        Styles.define :notes_red, :fg=>"c55", :face=>'arial black', :size=>"0", :bold=>true
        Styles.define :notes_yellow, :fg=>"cb0", :face=>'arial black', :size=>"0", :bold=>true
        Styles.define :notes_green, :fg=>"3c3", :face=>'arial black', :size=>"0", :bold=>true

        bg_color = Styles.attribute(:default, :background)


        Styles.define :notes_h2, :face=>'arial', :size=>"-1", :fg=>'999'
        Styles.define :notes_h2_pipe, :face=>'arial', :size=>"-1", :fg=>pipe
        Styles.define :notes_h0_pipe, :face=>'arial', :size=>"+8", :fg=>pipe, :bg=>bg, :bold=> true
        Styles.define :notes_h0, :fg=>"fff", :bg=>bg, :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h0_green, :fg=>"8f4", :bg=>bg, :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h1_green, :fg=>"9d4", :bg=>bg, :face=>'arial', :size=>"+3"

        Styles.define :notes_h0_yellow, :fg=>"ff0", :bg=>bg, :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h1_yellow, :fg=>"dd0", :bg=>bg, :face=>'arial', :size=>"+3"
        Styles.define :notes_h0_red, :fg=>"d00", :bg=>bg, :face=>'arial', :size=>"+8", :bold=>true
        Styles.define :notes_h1_red, :fg=>"d00", :bg=>bg, :face=>'arial', :size=>"+3"

        Styles.define :escape_glyph, :fg=>"666"   # Red special char was too ugly
        Styles.define :trailing_whitespace, :bg=>'555'

        Styles.define :quote_hidden, :fg=>bg_color

        Styles.dotted :bg=>'080808', :fg=>'111', :strike=>nil, :underline=>nil, :border=>['111', -1]

        exclamation_color = Styles.dark_bg? ? "4c4" : "5a0"

        Styles.define :notes_exclamation,  # Green bold text
          :face=>'arial black', :size=>"0",
          :fg=>exclamation_color, :bold=>true

        Styles.define :notes_double_exclamation,  # Purple bold text
          :face=>'arial black', :size=>"0",
          :fg=>"aa0", :bold=>true

        Styles.define :notes_triple_exclamation,  # Purple bold text
          :face=>'arial black', :size=>"0",
          :fg=>"d22", :bold=>true

        Styles.define :notes_link, :fg=>(Styles.dark_bg? ? "39b" : "08f"), :bold=>false

        Styles.define :shell_prompt, :fg=>'#888', :bold=>1

      end

    end

    def self.apply_styles

      # Don't format quotes (it overrides the following formatting)

      Code.cache(:notes_apply_styles) do
        Styles.clear

        # >... lines (headings)
        Styles.apply("^\\(>\\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)

        Styles.apply("^\\(> \\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
        Styles.apply("^\\(>> \\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)

        # > Green!
        Styles.apply("^\\(> \\)\\(.*!\\)\\(\n\\)", nil, :notes_h1_pipe, :notes_h1_green, :notes_h1_green)
        Styles.apply("^\\(> \\)\\(.*!!\\)\\(\n\\)", nil, :notes_h1_pipe, :notes_h1_yellow, :notes_h1_yellow)

        # > Large heading:
        Styles.apply("^\\(> \\)\\(.*:\\)\\(\n\\)", nil, :notes_h0_pipe, :notes_h0, :notes_h0)

        Styles.apply("^\\(> \\)\\(.*!:\\)\\(\n\\)", nil, :notes_h0_pipe, :notes_h0_green, :notes_h0_green)
        Styles.apply("^\\(> \\)\\(.*!!:\\)\\(\n\\)", nil, :notes_h0_pipe, :notes_h0_yellow, :notes_h0_yellow)
        Styles.apply("^\\(> \\)\\(.*!!!:\\)\\(\n\\)", nil, :notes_h0_pipe, :notes_h0_red, :notes_h0_red)

        # >>... lines
        Styles.apply("^\\(>>\\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)

        # Commented
        Styles.apply("^\\(>> .+?: \\)\\(.+\n\\)", nil, :notes_h2_pipe, :notes_h2)

        # >>>... lines
        Styles.apply("^\\(>>>\\)\\(.*\n\\)", nil, :notes_h3_pipe, :notes_h3)

        # >>>... lines
        Styles.apply("^\\(>>>>\\)\\(.*\n\\)", nil, :notes_h4_pipe, :notes_h4)

        # foo: > style like dir
        Styles.apply("\\(^[A-Za-z0-9 ][A-Za-z0-9 !-$'_.,>+?+-]*[\A-Za-z0-9.?]:\\)\\($\\| \\)", nil, :ls_dir)   # foo:
        Styles.apply("\\(^[\A-Za-z0-9 ]:\\)\\($\\| \\)", nil, :ls_dir)   # f:   < just 1 letter

        #    >... headings (indented)
        Styles.apply("^ +\\(> ?\\)\\(.*\n\\)", nil, :quote_heading_bracket, :quote_heading_h1)
        Styles.apply("^ +\\(> ?\\)\\(.*!:?\n\\)", nil, :quote_heading_bracket, :quote_heading_h1_green)

        Styles.apply("^ +\\(>>\\)\\(.*\n\\)", nil, :quote_heading_bracket, nil)


        # - bullets with labels and comments


        # Todo > instead of having both here > make one syntax that catches them both
        # - only match when: 1st char after space can't be [>|]
        #   - probably > 1st char after space has to be alphanumeric

        # Orange now, not gray
        Styles.apply("^[ \t]*\\([<+-][<+=-]*\\) \\([^/:\n]+:\\) ", nil, :ls_bullet, :notes_label)   # - foo: bar

        Styles.apply("^[ \t]*\\([0-9]+\\.\\)\\( \\|$\\)", nil, :ls_bullet, :notes_label)   # 1. foo
        Styles.apply("^[ \t]*\\([<+-]\\) \\([0-9]+\\.\\)\\( \\|$\\)", nil, :ls_bullet, :notes_label)   # - 1. foo

        Styles.apply("^[ \t]*\\([<+-][<+=-]*\\) \\([^(\n]*?)\\)\\( \\|$\\)", nil, :ls_bullet, :notes_label)   # - foo) bar
        Styles.apply("^[ \t]*\\([<+-][<+=]*\\) \\(.*:\\)$", nil, :ls_bullet, :notes_label)   # - foo:


        Styles.apply("^\\([ \t]*\\)\\([<+-]\\) \\(.+?:\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)
        Styles.apply("^\\([ \t]*\\)\\([<+-]\\) \\([^(\n]+?)\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)

        # exclamation! / todo
        Styles.apply("^[ \t]*\\([<+-]\\) \\(.*!\\)$", nil, :ls_bullet, :notes_exclamation)   # - foo!
        Styles.apply("^[ \t]*\\([<+-]\\) \\(.*!!\\)$", nil, :ls_bullet, :notes_double_exclamation)   # - foo!!
        Styles.apply("^[ \t]*\\([<+-]\\) \\(.*!!!\\)$", nil, :ls_bullet, :notes_triple_exclamation)   # - foo!!


        Styles.apply("^[ \t]*\\([0-9]+\\.\\) .+!$", :notes_exclamation)   # 1. foo!
        # Leave number orange > looks wierd

        Styles.apply("^ +\\(!\\+.*\n\\)", nil, :diff_green)   # !-Foo
        Styles.apply("^ +\\(!-.*\n\\)", nil, :diff_red)   # !+Foo

        Styles.apply("\\(\(-\\)\\(.+?\\)\\(-\)\\)", nil, :diff_small, :diff_red, :diff_small)
        Styles.apply("\\(\(\\+\\)\\(.+?\\)\\(\\+\)\\)", nil, :diff_small, :diff_green, :diff_small)

        Styles.apply "^ *\\(-?\\) ?\\([@=]? ?\\)\\(g\\)\\(o\\)\\(o\\)\\(g\\)\\(l\\)\\(e\\)\\(/\\|\n\\)", nil, :ls_bullet, :ls_dir,   # google
          :notes_blue, :notes_red, :notes_yellow, :notes_blue, :notes_green, :notes_red,
          :ls_dir

        Styles.apply "^hint/.+", :fade6

        # $..., %..., etc.
        Styles.apply "^[< ]*[@=]? ?\\([%$&#]\\)\\( \\|$\\)", nil, :shell_prompt   # "$ foo", "% foo", "& foo", and "# foo"

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


        Styles.apply "^\\(Upvote\\) \\(for @[a-z]+\\.\\)$", nil, :notes_green, :ls_quote
        Styles.apply "^\\(Upvote\\) \\(and comment for @[a-z]+:\\)$", nil, :notes_green, :ls_quote
        Styles.apply "^Comment for @[a-z]+:$", :ls_quote

      end

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

        false
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

    def self.mode options={}
      $el.notes_mode

      View.wrap(:off) if options[:wrap] == false
      nil
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

      if prepend =~ /^[!|:] /
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

      # Clicked "+" or "-" on a "+ foo" line, so expand or collapse...

      if Line.matches(/^\s*[+-]/) and View.char =~ /[+-]/

        children = Tree.children?

        # No childen and on file, so do "* outline"...

        if FileTree.handles? && !FileTree.dir?
          return FileTree.enter_lines
        end

        # Childen, so handle just like double-click...

        return Launcher.launch_or_collapse # if Tree.children?

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

    # Remove this > useless creation of objects.
    # Use View.block_positions instead, and just pass array of the 3 positions.
    def self.current_section_object regex=nil
      regex = self.heading_regex if ! regex || ! regex.is_a?(String)

      left, after_header, right = View.block_positions regex
      Section.new(left, after_header, right)
    end

    def self.to_html txt
      raise "Todo > use Html.to_html instead?!"
      txt = txt.
        gsub(/^> (.+)/, "<h1>\\1</h1>").
        gsub(/(^|[^\n>])$/, "\\0<br>")
    end

    def self.as_file options={}

      prefix = Keys.prefix :clear=>true

      label = nil

      Effects.blink(:what=>:line)

      was_visible = View.file_visible? Bookmarks['%links']

      if prefix == :u || options[:prompt_for_label]
        label = Keys.input :prompt=>"label: "
        label = "do" if label.blank?
        label = Notes.expand_if_action_abbrev(label) || label
        label.strip!

        # Only add ":" at end of label if not "!" at end
        label = "#{label}:" if label !~ /[:!)]$/

        prefix = nil
      end

      txt = View.txt_per_prefix prefix, :selection=>1, :default_is_line=>1, :just_txt=>1

      orig = Location.new

      # If file has bullet or ends with slash, grab path

      options = {}

      keep_tweeking = true
      if ! prefix && FileTree.handles?   # Grab tree

        txt = Tree.construct_path :raw=>1

        # Remove ##... if there
        txt.delete_if{|o| o =~ /^[#*]/}

        options[:path] = txt[0..-2].join
        if Search.try_merging_link(txt[-1], :path=>txt[0..-2].join, :label=>label)
          return orig.go
        end

        txt = "#{txt[0..-3].join}\n  - #{txt[-2]}\n    #{txt[-1]}\n"
        keep_tweeking = false
      end

      file = View.file

      if Search.try_merging_link(txt, :label=>label)   # Insert it in existing tree if there

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

      modified = View.modified?

      View.<< result, :dont_move=>1
      Line.next 3

      # If wasn't modified, save > since we may do ^; or other key that looks at :n on disk

      DiffLog.save :no_diffs=>1 if ! modified

      orig.go if was_visible   # Go to original place, if :n was already visible
    end


    def self.as_task options={}

      prefix = Keys.prefix :clear=>1

      # Nothing was selected...

      txt = nil

      # Something was selected, so use it...

      selection = View.selection

      # If method, make it Foo.bar method call
      line = Line.value

      if ! selection && View.file =~ /_spec.rb/ && line =~ /^ *(it|describe) /
        return Specs.enter_as_rspec
      end

      buffer_name = $el.buffer_name
      file, file_name = View.file, View.file_name
      path = Tree.path rescue nil


      if options[:link]   # || prefix == :u   # up+, so add as quoted, with "- todo!"

        # N+as+path, so just navigate...

        if prefix.is_a?(Fixnum)

          # Go to ^n
          View.open "%n"
          # Jump to nth quote
          View.to_top
          prefix.times do
            Search.forward "^ +:"
          end

          # Expand
          Effects.blink(:what=>:line)
          Launcher.launch

          return ""
        end


        # No prefix, so prompt for label and add to ^n...

        txt = FileTree.snippet :txt=>Line.value, :file=>file
        txt.sub! /^/, ">\n"


        label = Keys.input :prompt=>"Enter label: "
        label = Notes.expand_if_action_abbrev(label) || label
        label = "#{label}:" if label =~ /\w$/

        # Only add ":" at end of label if not "!" at end
        txt.sub!(/:/, "- #{label}\n    :") if label.any?

      elsif ! selection && prefix.nil?   # No 1+ or numeric prefix just grab normally

        if buffer_name == "ol"   # Make it into "foo = bar" format
          txt = line[/\) (.+)/, 1]
          txt.sub!(": ", " = ") if txt
          txt ||= line[/ *- (.+?) /, 1]

        elsif file =~ /\.xiki$/

          # File is in notes, or is linked to from notes, so use 'topic > Heading' syntax"

          if File.dirname(file) == File.expand_path("~/xiki") || self.expand_link_file(file)

            # On note "> Heading", so add as "foo > bar"

            # Only do something if > no ancestors
            if path.length == 1

              path = Path.split path[0]

              if path.length == 1 && path[0] =~ /^>/

                # "> Foo", so add "filename > Foo"
                topic = File.basename(file, ".*").gsub("_", " ")
                heading = line

                # "> .Foo" > so convert to path
                if heading =~ /^> \./
                  heading = Notes.heading_to_path heading
                  txt = "#{topic}/#{heading}"
                else
                  txt = "#{topic} #{heading}"
                end

              elsif path.length == 2 && path[1] =~ /^>/
                txt = "#{path[0].sub(/^-/, "")} #{path[1]}"
              end

            end
          end


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
        elsif line =~ /^ *Ol.>> (.+?)   #> (.+)/   # Make it into Foo = bar format
          txt = "#{$1} = #{$2}"
        elsif line =~ /^ *Ol.+, (.+?)   #> (.+)/   # Make it into foo = bar format
          txt = "#{$1} = #{$2}"
        end


      elsif selection && prefix == 1
        # 1+as+task, so make lines ":- ..."
        txt = selection.gsub(/^/, ":-")

      elsif selection && prefix == 2
        # 2+as+task, so make lines ":+ ..."
        txt = selection.gsub(/^/, ":+")

      end

      # No text yet, so maybe there's a selection, or a numeric prefix...

      txt ||= selection
      txt ||= View.txt_per_prefix(prefix, :selection=>1, :just_txt=>1, :default_is_line=>1)

      txt.strip! if txt =~ /\A.+\n\z/   # Strip when only 1 linebreak

      Search.move_to "%n", txt, options
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

      # cuts the block, and stores it in archive.file.xiki
      # example: ruby.xiki -> archive.ruby.xiki
      def archive
        delete
        filename = 'archive.' + $el.file_name_nondirectory(buffer_file_name)
        timestamp = "--- archived on #{Time.now.strftime('%Y-%m-%d at %H:%M')} --- \n"
        $el.append_to_file timestamp, nil, filename
        $el.append_to_file content, nil, filename
      end
    end

    def self.enter_note options={}

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
      txt.strip!

      expanded = self.expand_if_action_abbrev txt

      # If ends in colon, remove ")"

      if expanded =~ /[!:]$/
        View.delete :char
      end

      View << (expanded || txt)

      if txt =~ /^[0-9]$/
        Line << " "
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

    # Split into sections
    def self.split txt

      # Replace in special char to be marker
      # Might cause problem with utf strings, if this character is found? > Maybe mak sure they're utf encoded?
      txt = txt.gsub(/^>($| )/, "\c4\\0")
      sections = txt.split("\c4")

      # Remove blank string at beginning
      sections.shift
      sections

    end


    # 2015-07-12 > Todo > probably rename this to .expand?!

    def self.drill file, *args

      # Pull off options, to get prefix
      options = args[-1].is_a?(Hash) ? args.pop : {}

      prefix = Keys.prefix :clear=>true
      option_item = options[:task]

      username = XikihubClient.username

      line_orig = $el ? Line.value : nil

      # Text passed instead of the filename, so set the name
      if ! file
        file_exists = nil

      elsif file =~ /\n/
        file_exists = true
        txt = file
        name = options[:name]

      else


        # File is a word, so add default path for them
        if file =~ /^[a-z]/i
          # "* share", so use xikihub path
          file = option_item == ["share"] ?
            "~/xiki/hub/#{username}/#{file}.xiki" :
            "~/xiki/#{file}.xiki"
        end

        name = file[/(\w+)\.xiki$/, 1]

        file = Bookmarks[file]
        file = File.expand_path file

        # .link file, so follow link (use path inside it)
        file = Notes.expand_if_link file

        if file =~ /\/$/ && File.exists?(found = file.sub(/\/$/, ".xiki"))
          file = found
        end

        if file =~ /\/$/ && File.exists?(found = file.sub(/\/$/, "/menu.xiki"))
          file = found
        end



        # Somewhere in here, make it look for menu.html files as well

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

        file_exists = File.exists? file

        txt = file_exists ? File.open(file, 'rb') {|f| f.read} : nil
      end

      # /, so show just headings...
      # if ! heading && ! content
      if args.length == 0

        # Todo later > deal with this: these could be called multiple times for topic

        # ^O on foo, and notes exist, so show option items
        return "
          * add note
          * view source
          * web search
        " if option_item == []

        # /~ or / (and no notes exist), so show 1 option item

        if option_item == ["add note"]
          return self.add_note_task options
        end

        if option_item == ["view source"] #|| options[:go]   # ~ navigate
          View.open file
          return ""
        end

        if option_item == ["web search"]   # ~ navigate
          txt = "#{name}".gsub(" > ", " ")
          Xiki::Google.search txt, :via_os=>1
          return ""
        end

        # Render actions as paths

        headings = self.extract_headings_and_actions txt

        # No headings, so show all content
        return Tree.quote txt if headings.empty?

        return headings
      end

      # Commented out > Don't know when heading would have been ": foo"

      # Figure out whether args has "> Heading", "content...", etc

      raise "Notes.drill doesn't know how to deal with being passed content without a heading first" if args.length == 1 && args[0] =~ /\n/   #> "load"
      heading = args[0]   # Heading might also be just a path
      content = args[-1] if args[-1] =~ /\n/   #> nil

      if options[:change_to_heading] || options[:change_to_heading_and_launch]   #> continue here!!
        # Just change line and return
        heading = self.find_heading_from_path txt, args[0]   #> |||

        Ol "heading", heading   #> "> @boo .brown background"

        # Remove username if any
        heading.sub!(/^> \@\w* /, "> ")

        Line.sub!(/.+/, Line.value.gsub(/( *)[+-] (.+)/){"#{$1}#{heading}"})
        Launcher.launch if options[:change_to_heading_and_launch]

        return ""
      end


      # /> Heading (or path and possible other paths), so show note content (or navigate to it)...

      # Todo > Hmm > If option_item passed, just delegate on to the action

      is_heading = args[0] =~ /^> /

      if args.length == 1 || ! is_heading

        # Heading might be "> Foo" or "foo"
        option_item = ["view source"] if options[:go]

        # ^O on "> Foo", so show options

        if is_heading
          return "
            * top
            * view source
            * web search
          ".unindent if option_item == []

          if option_item == ["make action"]
            # Passed programatically by ^G > change but don't run
            Line.sub!(/.+/, Line.value.gsub(/( *)> \.?(.+)/){"#{$1}+ #{Notes.heading_to_path $2}"})
            return ""
          elsif option_item == ["as action"]
            Line.sub!(/.+/, Line.value.gsub(/( *)> \.?(.+)/){"#{$1}+ #{Notes.heading_to_path $2}"})
            Launcher.launch
            return ""
          elsif option_item == ["top"]
            return "- todo > move to top!"
          elsif option_item == ['web search']
            txt = "#{name} #{heading}".gsub(" >", "").downcase
            Xiki::Google.search txt, :via_os=>1
            return ""
          end
        end

        # No '~ view source' option_item, so always expand

        # No option item, or option item on "+ action", so expand...

        if (option_item == ["view source"] || options[:go]) && heading =~ /^(@\w* )?[a-z]/

          heading = self.find_heading_from_path txt, heading

        elsif ! option_item || heading =~ /^(@\w* )?[a-z]/

          # "> Heading" without option, or "+ action", so extract section

          return if txt.blank?   # File doesn't exist, return nil so it'll tell them to type ^O

          txt = self.extract_section txt, heading, options

          return nil if ! txt

          txt.sub! /\n\z/, ''

          # No text for this heading, so assume it wasn't there, so prompt them to enter it...

          # There was some output, so clean it up and return

          if txt.any?

            txt = Tree.pipe "#{txt}\n"

            # Todo > make $ at margins be unquoted
            # Expose dollar prompts > and percent > unquote shell prompts
            self.expose_some_prompts txt

            return txt
          end

          # No output, so continue on and navigate

        end

        # No content, so just navigate...

        View.open file
        View.to_highest

        if heading !~ /^>/
          # It's an action, so turn path into a heading
          heading = self.find_heading_from_path txt, heading
        end

        Search.forward "^#{$el.regexp_quote heading}$", :beginning=>1
        View.recenter_top
        return ""

      end

      heading_exists =
        if ! txt
          false   # Contents would have existed if file existed
        else
          section = self.extract_section txt, heading   #> |||
          section.any?
        end

      # /> Heading/content, so navigate or save...

      # ~, so show options...
      if option_item

        # If more than one item, error out!

        items = options[:items]
        return "<* - Collapse this first!" if items && items.length > 2   # They expanded the 'notes don't exist' message

        menu = self.option_items(options.merge(
          :context=>:expanded,
          :heading_exists=>heading_exists,
          :name=>name,
          :heading=>heading,
        ))

        xik = Xik.new(menu)

        option_item = OptionItems.prepend_asterisk option_item


        # These'll be needed in case it's "* save"
        options[:save_args] = {
          :txt=>txt,
          :file=>file,
          :heading=>heading,
          :content=>content,
        }

        return xik[option_item, :eval=>options]
      end

      # File doesn't exist, so suggest ^n to save

      options[:line_found], options[:no_search] = 9, 1
      return "
        - Did you mean Ctrl+T?:
        |
        | These notes don't exist yet.
        |
        | You can't navigate to notes that don't exist.
        | If you are trying to create a note, you should type
        | Ctrl+T and select 'save' instead of expanding.
        |
        = ok/
      " if ! file_exists


      # No tasks, so navigate to heading (and move cursor to it)...


      # In xikihub, so send them contents or checksum...

      orig = View.file

      View.open file   #> ||||

      View.to_highest

      found = self.jump_to_heading heading

      # Not found, so show message...

      if heading && ! found

        if options[:inline_warning_messages]
          View.open orig
          options[:line_found], options[:no_search] = 9, 1
          return "
            - Did you mean Ctrl+T?:
            |
            | This heading doesn't exist yet.
            |
            | You can't navigate to a heading that doesn't exist.
            | If you are trying to create a note, you should type
            | Ctrl+T and select 'save' instead of expanding.
            |
            = ok/
          "
        end
        return "<*- Heading doesn't exist yet!"

      end

      # Jump to text underneath heading...

      self.jump_to_text_under_heading line_orig

      return ""

    end

    def self.expose_some_prompts txt

      txt.gsub! /^\| ([$%&] )/, "\\1"
      txt.gsub! /^\| (<+=? )/, "\\1"

    end


    def self.jump_to_heading heading
      return if ! heading
      View.to_highest
      found = Search.forward "^#{$el.regexp_quote heading}$" if heading
    end

    def self.jump_to_text_under_heading line
      View.recenter_top
      if line
        line.sub!(/^ *[|:].?/, '')
        found = Search.forward "^#{$el.regexp_quote line}" if line.any?
        Move.to_axis
      end
    end

    def self.add_note_task options

      self.add_note_prompt #:single_word_topic=>1
      return ""

    end


    def self.find_heading_from_path txt, target, options={}

      # Extract all "> .foo" headings

      lines = txt.scan(/^> .+/)

      # Find one who's path version matches the target

      found = []

      target = target.downcase
      lines.each do |heading|

        # Ignore "(file option)" at beginning of heading   #> "> .test"
        path = heading


        path = self.remove_ignorable_heading_parens(path)#.downcase

        if options[:find_all_usernames]
          path = self.heading_to_path path, :remove_username=>1

          if path == target
            found << heading
          end
        else
          path = self.heading_to_path path
          if path == target
            return heading
          end

          # Find one, so check return single exact match   #> "load"

        end

      end

      return nil if found == []

      found

    end


    def self.remove_ignorable_heading_parens path
      return nil if ! path
      path.sub(/^> \(file option\) /, '> ')   #> "set desktop image"
    end

    def self.extract_section txt, target, options={}

      # Heading isn't "> ...", so get heading that corresponds to this path   #> "@ private and shared"
      if target !~ /^> /   #> ||||-
        target = self.find_heading_from_path txt, target   #> |||
      end

      # Delete everything before and after the target section   #> nil

      txt = txt.dup
      before = txt.slice!(/.*^#{Regexp.escape target}\n/m)   # Delete before block

      # Section doesn't exist > so return nil to indicate that
      return nil if ! before

      # Todo > set :heading_line_number
      # Pass in options, so we can set it there?
      options[:heading_line_number] = before.scan(/\n/).length   #> 4

      options[:heading_found] = target

      txt.sub! /^>( |$).*/m, ''   # Delete after block
      txt
    end

    def self.read_block file, heading
      self.extract_section File.read(file), heading
    end

    def self.tab_key options={}

      Keys.remember_key_for_repeat(proc {Notes.tab_key})

      # Selection is active, so just indent it...

      return Code.indent_to(options) if View.selection?

      path = Tree.path
      first = Path.split path[0]
      last = Path.split path[-1]
      line = Line.value

      # Blank line or just whitespace, so just make indent like previous line

      if line =~ /^ *$/

        indent = Line.indent
        indent_prev = Line.indent(Line.value 0)

        # Indented lower, so clear indent
        if indent.length > indent_prev.length
          Line.sub! /.*/, ''

        # Indented the same, so indent 2 lower
        elsif indent.length == indent_prev.length
          Line.sub! /^/, '  '

        # Indented less, indent same
        else
          Line.sub! /.*/, indent_prev
        end

        Line.to_right

        return
      end




      # $... path, so run Xiki command if cursor at right, otherwise collapse and run...

      if path[-1] =~ /^ *\$( |$)/

        # The cursor is at the right, so run Xiki command for the shell command...
        return Shell.tab if Line.at_right?

        line = Line.value

        Launcher.collapse_items_up_to_dollar line.sub(/^ */, '')#, :or_root=>1

        return

      end

      # |... quote under topic, so make quote replace topic

      if last[-1] =~ /\n/
        quote = last[-1]
        return self.zoom_to_quote #quote
      end

      # item under "f..." (auto-complete), so collapse upward and replace parent...

      if last.length == 2 && last[0] =~ /\A[a-z0-9][a-z0-9 ]*\.\.\.\z/
        # foo..., so collapse to parent
        bounds = Tree.sibling_bounds(:must_match=>"[a-z]")
        View.delete bounds[0], bounds[-1]
        Move.up
        Line.gsub! /.+/, last[-1]
        return
      end

      # Topic, so show completions or collapse in correct way...

      if Topic.matches_topic_syntax?(last[0])

        # Tab on "  = foo", so remove equals and replace parent
        if line =~ /^ +=/
          Tree.collapse_upward :replace_root=>1
          return Launcher.launch
        end

        # A single topic, so do tab completion
        return Topic.tab_key(:flashing_ok=>1) if last.length == 1

        # Tab on "topic\n  > heading", so collapse into parent adding space
        if last.length == 2 && last[-1] =~ /^>/
          Tree.collapse_upward :add_between=>" "
          return Launcher.launch
        end

        # commands/foo, so just collapse and don't launch...

        if last[0] == "xiki"
          Tree.collapse_upward :replace_root=>1
          return
        end

        # foo/bar, so collapse up to parent and run...

        if last.length > 1 && Topic.matches_topic_word?(last[1])

          # Add tab at end if not one there

          Tree.collapse_upward :slash_separator=>1

          return Launcher.launch
        end


        # Probably a topic under a topic, so just launch it to do what tab completion does

        return Launcher.launch(:tab=>1)   #> |

      end

      # "$ command" nested under topic...

      if path.length == 2 && Topic.matches_topic_syntax?(first[0]) && last[0] =~ /^\$ /

        # Tab on "topic\n  $ command", so collapse into parent...

        if first.length == 1
          Tree.collapse_upward :replace_parent=>1
          return Launcher.launch
        end

        # Tab on "topic\n  > Heading\n    $ command", so collapse into parent...

        if first.length == 2 && first[1] =~ /^> /
          Tree.collapse_upward :replace_parent=>1
          Line.to_left
          Tree.collapse_upward :replace_parent=>1
          return Launcher.launch
        end

      end

      # Something on the line besides whitespace...

      at_right = Line.at_right?

      # Cursor at right, so just expand...

      return Launcher.launch(:tab=>1) if at_right

      indent = Line.indent line

      # No indent, so do normal expand...

      return Launcher.launch if ! indent.any?


      # Cursor not at right and line indented, so collapse into parent and expand...

      Tree.collapse_upward

      # Original line was "=...", so replace parent

      if line =~ /^ *=/
        line.sub!(/^  /, '')   # Indent one line less
        line.sub!(/^= ?/, '')   # Remove "= " if at left margin
        Line.sub!(/.*/, line)
      end

      Launcher.launch

      # It's a =... line, so delete parent

    end


    # If the string is "t" or "i", or a few others, return "todo" or "imprement" etc. respectively.
    def self.zoom_to_quote #quote=nil

      quote = Tree.siblings
      quote = quote.join("\n")

      quote.gsub!(/^\| ?/, '')   # Remove quotes

      # grab_target_view var set, so jump back to where ^n was done and insert...

      return if Grab.to_grab_target_view quote

      # Collapse up to left margin or "="...
      Tree.to_root

      Tree.collapse
      Line.sub! /.*/, quote
      ""
    end

    def self.expand_if_action_abbrev txt
      @@single_letter_abbrev[txt] || txt
    end

    @@single_letter_abbrev = {
      " "=>"",
      "p"=>")",
      "b"=>"borrow:",
      "o"=>"original:",
      "m"=>"mock:",
      "g"=>"given:",
      "re"=>"reshuffle:",
      "w"=>"works:",

      "t"=>"try",
      "tt"=>"try > ",
      "ss"=>"should > ",

      "s"=>"save!",
      "e"=>"!",
      "a"=>"add!",
      "ch"=>"check!",
      "cr"=>"create!",
      "d"=>"do!",
      "de"=>"delete!",
      "er"=>"error!",
      "f"=>"fix!",
      "fa"=>"favorite!",
      "fi"=>"finish!",
      "i"=>"implement!",
      "r"=>"rename!",
      "to"=>"todo!",
      "te"=>"test!",
      "tem"=>"temp!!!!!!!!!!!!!",

      "pr"=>"problem here!!!",

      "c"=>"continue here",
      "c!"=>"continue here!",
      "cc"=>"continue here > ",
      "u"=>"update!",
      "1"=>"1",
      "2"=>"2",
      "3"=>"3",
      "4"=>"4",
      "5"=>"5",
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
    # matches your chars.  Matches are determined by Keys.fuzzy_filter().
    def self.open_note options={}

      open_or_insert = options[:insert] ? Launcher.method(:insert) : Launcher.method(:open)

      # Get user input...

      keys = Keys.input :optional=>1, :message=>"Which note? (Type a key or two): "
      return open_or_insert.call "notes/" if ! keys

      # Get possible matches - files in ~/notes without extensions...

      files = Dir.new(File.expand_path("~/xiki/")).entries.grep /\A[^.]/
      files.map!{|o| o.sub /\..+/, ''}

      # First do a couple hard-coded mappings
      if shortcuts = {"r"=>"ruby", "e"=>"elisp", "j"=>"javascript"}[keys]
        found = shortcuts if files.member?(shortcuts)   # Only use if it's in the list of notes we actually have
      end

      # Narrow down by input

      found ||= Keys.fuzzy_filter files, keys

      return open_or_insert.call "notes/" if ! found

      open_or_insert.call ":#{found}"

      nil
    end

    def self.open_todo options={}

      prefix = Keys.prefix :clear=>1

      file = Bookmarks[options[:bookmark] || "%n"]

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

    def self.next_marker options={}

      times = options[:nth] || Keys.prefix_n(:clear=>1)

      # Numeric prefix, so go to top, then run that many times

      View.to_top if times
      times ||= 1

      found = nil
      times.times do
        # Move forward if at beginning of line, so we don't find the one on the current line
        Move.forward if Line.at_left? && ! View.at_top?
        # found = Search.forward "^ *[+-] [a-zA-Z0-9 +'.>_-]*)\\($\\| \\)", :beginning=>true
        # New behavior > only go to ones on line by themselves
        # Marker regex
        found = Search.forward "^ *[+-] [a-zA-Z0-9 +'\"!?*~^.,>_-]*)$", :beginning=>true
      end

      return Move.backward if ! found

      # Label for the next line, so move to next line
      self.next_line_when_marker
    end

    def self.next_line_when_marker
      # Marker regex
      Line.to_next if Line[/^ *[+-] [a-zA-Z0-9 +'"!?*~^.,>_-]*\)$/]   # '
    end

    def self.extract_markers options={}

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

        # Marker regex > Label regex
        label = line[/^ *[+-] ([a-zA-Z0-9 +'"!?*~^.,>_-]*)\)$/, 1]
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


    # Usually just inserts linebreak, but does extra stuff when return on shell prompt
    # line for the first time, or when typing on quotes with a quoted line following
    def self.return_wrapper


      # Text selected, so delete it and insert return...

      if selection = View.selection
        View.delete *View.range
        View << "\n"
        return
      end


      line = Line.value

      # Cursor on shell prompt line, after the prompt, so maybe show message
      if line =~ /^ *\$ / && View.column > line.index("$")
        # Show tip about Ctrl+X vs Ctrl+G for shell prompt, unless already shown
        return if Shell.shell_prompt_keys_tip
      end

      Keys.remember_key_for_repeat(proc {Notes.return_wrapper})

      # Add a linebreak

      # Remove trailing space if any after pipe
      Line.sub!(/ $/, '') if line =~ /^ *\| +/


      next_line = Line.value 2
      indent = Line.indent next_line

      Keys.prefix_times do
        View << "\n"
      end


      # Current and next line are "  | ...", so add quote to this line
      return View << "#{indent}| " if Line.at_right? && line =~ /^*\|/ && next_line =~ /^*\|/

    end


    # Adds section to .xiki file on disk.
    # Default is at the top
    def self.add_section filename, section

      # File exists, so prepend...

      txt = File.open(filename, 'rb') {|f| f.read} rescue ""
      txt = "#{section.strip}\n\n#{txt.strip}\n\n"
      File.open(filename, "w") { |f| f << txt }
    end

    def self.extract_links options={}

      source_file = options[:file] || Bookmarks["%links"]

      # limit = 45
      # limit = 200
      # limit = options[:limit] || 1000
      limit = options[:limit] || 2000

      # limit = 5
      result, dir, filename, filename_indent = [], [], nil, nil
      heading = nil

      txt = options[:txt] ||  File.read(source_file)

      txt.split("\n").each do |line|

        indent = Tree.indent_size line

        next if line.blank?
        line.sub! "\n", ''

        # .../, so append as dir...
        if line =~ /^[^:|#]*\/$/
          dir = dir[0..indent]
          next dir[indent] = line
        end

        # Filename found, so add to path if it's ours...

        if like_filename = line[/^ *[+-] (.+\w)$/, 1]

          # Construct full path
          full = dir.compact
          full.map!{|o| Line.without_label :line=>o.strip}
          full = full[0..indent]
          full[indent] = like_filename
          full = full.join
          full = Bookmarks[full]

          # Not target file, so clear out filename and skip

          filename = full
          filename_indent = line[/^ */]


          # Heading existed, so add it before file path
          if heading && ! options[:filter]
            result << [heading]
            heading = nil
          end

          next

        end

        # Heading, so remember it
        if line =~ /^>/
          heading = line
          heading = nil if heading =~ /^> ?:?$/
          next
        end

        next if ! filename   # Can't do anything if no filename yet

        next if options[:filter] && line !~ options[:filter]   # Ignore stuff that doesn't match :filter, if passed


        next if line !~ /^ /   # Ignore stuff at beginning of line that isn't a dir

        # It's a quote or bullet, so add it to result...

        # Subtract off of indenting the amount the file was indented
        line.sub!(/^#{filename_indent}  /, '')

        result << [filename, line]

        limit -= 1   # Todo > when finished > move down to where > we find quotes
        break if limit < 1
      end
      result

    end

    def self.jump_to_label name
      label = Emacs.regexp_quote name
      Search.forward "^ *[+-] \\(#{label})\\|)\n[^a-zA-Z_]*#{label}/?$\\)", :beginning=>1
    end

    # Notes.heading_to_path "> Foo bar > bah"
    #   "foo bar bah"
    def self.heading_to_path heading, options={}

      path = heading.sub(/^> /, '')
      path.downcase!

      # Remove username

      user = path.slice! /^\@\w* /

      path.gsub!(/['"()]/i, '')   # Remove apostrophes and certain other characters
      path.gsub!(/[^a-z0-9]/i, ' ')   # Turn other characters into space
      path.gsub!(/  +/, ' ')   # Collapse subsequent punctuation into one space
      path.sub!(/ +$/, '')   # Remove spaces from end

      path.strip!

      # :url, so use dashes instead of spaces
      path.gsub!(" ", "-") if options[:url]

      # Put @user back on if was there
      if user && ! options[:remove_username]
        path = "#{user}#{path}"
      end

      path
    end


    def self.add_note_prompt options={}

      txt = %`
        > My note
          | Type Ctrl+T after typing or pasting your note here.
          | (Optionally change "My note" first)
          |
      `.unindent

      # Add "+ save" and "+ share" if in noob mode

      # :shell, command passed, so use it in note...

      top_lines = ""
      top_lines_length = (top_lines.scan(/\n/) || "").length + 1

      insert_options = {:no_search=>1}
      insert_options[:following] = 1 if options[:no_indent]

      Tree.<< txt, insert_options

      Line.next; Line.to_beginning
      # Make green background for hints > "Note heading" and "note text"
      cursor = View.cursor
      Overlay.face :diff_green, :left=>View.cursor, :right=>Line.right
      Line.next; Line.to_beginning
      Overlay.face :diff_green, :left=>View.cursor, :right=>Line.right
      View.cursor = cursor

      # Pause until they type any key
      key = Keys.press_any_key :message=>"Type any key...."

      # Delete green stuff and add spaces

      View.delete View.cursor, Line.right
      Line.next; Line.to_beginning; Move.left
      View.delete View.cursor, Line.right
      View.cursor = cursor
      View.<<(key[0].chr) if key.length == 1 && key[0] >= 32 && key[0] <= 126

    end


    # Run when "* add note".
    def self.add_note_prompt_shell command

      topic = Topic.shell_command_to_topic command
      topic.sub!(/ .*/, '')   # Only use first word

      # Open the topic file, and insert there at top
      View.open File.expand_path("~/xiki/#{topic}.xiki")

      txt = "> Note name here\n$ #{command}\n\nOptional description here.\n\n\n"

      Move.top

      # Get topic name from command
      View >> txt

      # Make green background for > "Optional description"
      View.line = 4
      Overlay.face :diff_green, :left=>View.cursor, :right=>Line.right

      # Make green background for > "Note heading"
      View.line, View.column = 1, 3
      Overlay.face :diff_green, :left=>View.cursor, :right=>Line.right

      # Pause until they type any key
      key = Keys.press_any_key :message=>"Type any key....."

      # Delete green stuff and add spaces
      View.line = 4
      Line.delete :leave_linebreak
      View.line, View.column = 1, 3
      View.delete View.cursor, Line.right
      View.<<(key[0].chr) if key.length == 1 && key[0] >= 32 && key[0] <= 126

    end


    def self.save_on_blank options

      task = options[:task]


      # "* save" and was saved already, so just resave

      if task == ["save"] && View.file
        return DiffLog.save
      end

      # "* share" and was saved already, so just __

      if task == ["share"] && View.file
        return View.flash "Already shared"
      end


      # No username, so say they need to create a username...

      user = XikihubClient.username
      if ! user && task == ["share"]
        options[:dont_nest] = 1
        View.<< "* share\n  :-You need to set up a username before you can share!\n  = ok/\n\n", :dont_move=>1
        Line.next 2
        View.column = 3

        return ""
      end


      # ~ share/command name, so add to file, save, and show message...

      if task.length == 2

        topic = task[1].sub(/^: /, '').downcase

        # Weird characters in topic name, so remove and show validation messages

        if topic !~ /^[a-z][a-z0-9 ]*$/
          topic.gsub! /[^a-z0-9 ]/, ""
          View.<< "* share\n  |-Topics can only contain letters and numbers. Type Ctrl+O when finished.\n  : #{topic}\n\n", :dont_move=>1
          Line.next 2
          Line.to_right

          return ""
        end

        topic.gsub!(" ", "_")

        if topic == ""
          options[:nest] = 1
          # Fix this > it moves down > use buffer local variable to remember blank lines to put back in
          return "<* You must provide a name"
        end

        txt = View.txt
        txt = "#{txt.strip}\n\n\n"
        orig = View.name
        topic_file = "#{Dirs.commands}/#{topic}.xiki"

        view_orig, file_orig = View.name, View.file
        View.open topic_file

        # Error out if view has unsaved changes...

        if View.modified?
          View.open(:txt=> "
            > Couldn't share, because #{topic}.xiki has unsaved changes
            views/
              - 1. Save your unsaved changes first:
              #{topic}.xiki

              - 2. Then go back and try to share again:
              #{orig}

          ".unindent, :line_found=>2)
          return ""
        end



        # Prepend generic heading > if none exists
        txt = "> My note\n#{txt}" if txt !~ /\A> ./


        # ~ share, so add "@" to headings > so it'll be shared
        txt.gsub! /^> (?!@)/, "> @ " if task[0] == "share"



        # Prepend to actual file, and revert to show new addition
        View.to_top
        all_txt = File.read(topic_file) rescue ""
        File.open(topic_file, "w") { |f| f << "#{txt}#{all_txt}" }
        Files.revert
        View.message " "


        # Delete original view if it was temporary
        $el.kill_buffer(view_orig) if ! file_orig



        # Save to xikihub (if were editing shared)
        if task[0] == "share"
          XikihubClient.delegate_to_save_async(topic_file)
        end


        # ~ share when first shared heading in topic, so show green box at top with message and url...

        if task[0] == "share" && all_txt !~ /^> @ /

          # http://xiki.com/@#{user}/#{topic.gsub("_", "-")}
          # Shared tasks ("> @ ...") can be viewed on the web:
          key = Hint.top_box %`
            Sharing...  Notes starting with "> @ " are viewable on the web:

              #{XikihubClient.url}/@#{user}/#{topic.gsub("_", "-")}

            To see web version or save further changes, type Ctrl+C at any time.
          `.unindent

          # They typed Ctrl+C, so delay and then simulate ^C
          if key == [3]
            View.pause n=0.3
            Keys.expand ["content"]
          end

        end

        return ""
      end


      # ~ share, so prompt them to enter command name...

      options[:dont_nest] = 1

      message = self.share_or_save_prompt_text

      # Add linebreak before if non-blank line above

      txt = "* #{task[0]}\n  | #{message}\n  : \n\n"

      # Not at top line, and line before is non-blank so add \n before
      if View.line > 1 && ! Line.value(0).blank?
        View.<< "\n"
        Launcher.added_option_item_padding_above = 1   # So it deletes it again
      end


      View.<< txt, :dont_move=>1
      Line.next 2
      View.column = 5

      View.message " "

      ""

    end

    def self.share_or_save_prompt_text
      "Type a topic name, then type Ctrl+O"
    end

    def self.save_note_or_share_items options, key=:args
      save_note = options[key][-1] == "save"
      save_note ? View.delete(Line.left, Line.left(3)) : View.delete(Line.left(0), Line.left(2))
      Move.previous
      options[:task] = save_note ? ["save"] : ["share"]
      txt = Tree.siblings :string=>1
      options[key][-1] = txt
    end

    def self.option_items options

      topic_vs_search = ! options[:delegated_from_search]   #> false

      heading = options[:heading]
      Ol "heading", heading   #> nil
      your_username = XikihubClient.username

      # Context is expanded note

      if options[:context] == :expanded

        save_or_create = "save"
        name = options[:name]
        name_hyphens = name.gsub('_', '-')

        txt = %`
          * #{save_or_create}
            ! Notes.save options
          * share_placeholder
          * web
            ! url = "#{XikihubClient.url}/@#{your_username}/#{name_hyphens}/#{Notes.heading_to_path heading, :url=>1, :remove_username=>1}"
            ! Ol "!!!"
            ! Browser.url url, :os_open=>1
            ! ""
        `.unindent

        # Show '~ share' if > new > or not '> @ ...' heading...
        # Show '* share' when > My note > or existing and your private or shared...

        if ! options[:heading_exists] || options[:heading] !~ /^> @\w/   #> add 'share' option!!!
          txt.sub! "* share_placeholder\n", "
              * share
                ! Notes.save options
                ! ""
            ".unindent
        else
          txt.sub! "* share_placeholder\n", ""
        end


        return txt

      end

      if options[:context] == :blank_line

        txt = %`
          * save
            ! #DiffLog.save
            ! Notes.save_on_blank options
            ! ""
          * share
            ! Notes.save_on_blank options
          * as xiki topic
            ! # "- Todo > Implement > maybe add '* save' at top? > like content+save in untitled view?!"
            ! # Notes.as_xiki_file options
            ! DiffLog.save_newly_created
            ! #"* share\\n  | #{Notes.share_or_save_prompt_text}\\n  : "
            ! #""
          * close
            ! View.kill
            ! ""
        `.unindent

        # We're not in a non-topic file, so suppress "* as topic"
        txt.sub!(/^\* as xiki topic\n.+\n/, '') if ! View.file || View.topic_file?

        # We're not in an untitled view, so suppress share
        txt.sub!(/^\* share\n.+\n/, '') if View.file

        txt

      end

    end


    def self.save options={}

      save_args = options[:save_args]
      content, txt, heading, file, name = save_args[:content], save_args[:txt], save_args[:heading], save_args[:file], save_args[:name]

      items, task = options[:items], options[:task]

      # Add @ to heading if > '> ~ share' > and > not already there

      if task == ["share"] && heading !~ /^> @/
        heading.sub! /^> /, '> @ '
      end

      # "* save", so replace or add...

      txt_old = (txt||"").dup

      # Replace existing or prepend to top...

      content = Tree.siblings :children=>1
      content.gsub! /^:.*\n/, ''   # Don't save :... lines   #> "| + Animalzz\n|\n: Your shared version\n"
      content.gsub! /^[=|:] ?/, ''

      txt ||= ""   # In case new

      self.replace_section txt, heading, content

      diffs = DiffLog.save_diffs :patha=>file, :textb=>txt

      # Save to xikihub (if were editing shared)
      if items[0] =~ /^> @ /
        XikihubClient.delegate_to_save_async(file)
      end

      # Create dir if it doesn't exist
      FileUtils.mkdir_p File.dirname(file)

      # Save the actual file
      File.open(file, "w") { |f| f << txt }

      return task == ["share"] ? "<* - shared!" : "<* - saved!"

    end


    # If there's a ~/xiki/foo.link for this file, update its timestamp
    # as well (so it appears at the top of list+topics).
    def self.update_link_timestamp_if_any filename

      # In the ~/xiki dir, so do nothing
      return if filename.start_with? File.expand_path("~/xiki")+"/"

      # Not a .xiki or .xiki file, so do nothing
      extension = filename.slice(/\.(notes|xiki)$/)
      return if ! extension

      link_filename = self.expand_link_file filename
      return if ! link_filename

      # Update timestamp of the .link file
      FileUtils.touch link_filename
      nil

    end



    # Returns path to ^n/foo.link file that links to filename, if there is one.
    def self.expand_link_file file

      # Directory, so look for .xiki file in it
      if Dir.exists? file
        if File.exists?("#{file}menu.xiki")
          file = "#{file}menu.xiki"
        else
          basename = File.basename file
          if File.exists?("#{file}#{basename}_menu.xiki")
            file = "#{file}#{basename}_menu.xiki"
          else
            # No matching file in dir
            return
          end
          # Todo > check for dir_name.xiki & dir_name_menu.xiki also

        end
      end

      # Look for corresponding .link file (assume it has the same name)
      base = File.basename file, ".*"

      link_file = File.expand_path "~/xiki/#{base}.link"

      # Link exists, so return it...

      if File.exists? link_file
        txt = File.expand_path File.read(link_file).strip
        return link_file if file == txt
      end

      # Link exists when no "_menu", so return it...

      link_file_without_menu = link_file.sub(/_menu\.link$/, '.link')
      if link_file_without_menu != link_file && File.exists?(link_file_without_menu)
        txt = File.expand_path File.read(link_file_without_menu).strip
        return link_file_without_menu if file == txt
      end

      # Check for .link files named similar to file or dir

      pattern = base
      if pattern == "menu"
        pattern = File.basename(File.dirname(file))
        Dir[File.expand_path "~/xiki/#{pattern}*.link"].each do |candidate|
          txt = File.expand_path(File.read(candidate)).strip
          return candidate if file == txt
        end
      end

      nil

    end

    def self.expand_if_link file

      if file =~ /\.link$/
        file = File.read(file).strip
        file = File.expand_path file
      end
      file
    end


    # Looks at quoted lines where cursor is, and shows green "| > Add heading here" prompt,
    # if the first line isn't "| > ...".
    def self.prompt_for_heading_if_missing

      indent = Line.indent

      orig = View.cursor   # Remember where we started, so we can just come back here if > it's already there
      # Don't need > ## Line.previous while Line =~ /^ *\|/   # Move back until |... line
      Line.previous while Line.value(0) =~ /^ *\|/   # Move back until above line isn't |... line

      # It's already ">...", so jump back and do nothing
      if Line =~ /^ *\| > ./
        View.cursor = orig
        return
      end

      # Add "^ save\n^ share" back
      top = View.cursor
      View.cursor = orig
      Line << "\n#{indent}^ save\n#{indent}^ share"


      View.cursor = top
      Line.to_left
      View.<< "#{indent}| > Type a task name\n", :dont_move=>1
      Line.to_beginning
      Move.right 2

      Overlay.face :diff_green, :left=>View.cursor, :right=>Line.right

      # Pause until they type any key
      key = Keys.press_any_key :message=>"Type any key......."

      # Delete green stuff and add spaces
      View.delete View.cursor, Line.right
      View.<<(key[0].chr) if key.length == 1 && key[0] >= 32 && key[0] <= 126

      # Indicate that we prompted them (caller will do nothing, so they can type the heading)
      return true

    end

    def self.indent_note_under_heading

      # Fake heading
      indent = Line.indent
      orig = View.line   # Remember where we started, so we can just come back here if > it's already there

      # Move up to 1st quoted line
      Line.previous while Line.value(0) =~ /^ *\|/
      # Delete quote before heading
      Line.to_beginning
      $el.delete_backward_char 2

      # Indent the rest of the lines
      Line.next
      left = View.cursor
      Line.next while Line.value =~ /^ *\|/
      txt = View.delete(left, View.cursor)
      View << txt.gsub(/^/, "  ")

      View.to_line orig

    end


    def self.enter_task
      path = Clipboard.get("=")

      # Extract topic
      topic = path[/^  [+-] ([\w]+)/, 1]
      topic.gsub! /_/, ' '
      # Extract task
      task = path[/^    : (> .*)/, 1]
      Line << "<- #{topic} #{task}"

    end

    # For now, only extracts non-blank ones
    def self.extract_headings txt, options={}

      if options[:exclude_just_upvotes]
        # Have to read whole file into hash, and look at each body

        hash = Notes.wiki_headings_to_hash(txt)

        # Remove ones that are just upvotes...

        label_regex = XikihubClient.comment_or_upvote_label_regex "\\w+"
        hash.delete_if do |k, v|
          v =~ /\A\n*#{label_regex}\n*\z/   #> nil
        end

        return hash.keys.map{|o| o.sub(/^> /, '')}
      end

      result = txt.scan(/^> (.+)/).flatten

      # :paren, so filter out only items with text in parens   #> "> Private and shared\nThe shared one\n\n\n> One shared\n+ Animalzz\n\n> Shared and installed\nShared\n\n\n> Private, shared, installed\nShared\n\n\n"
      if paren = options[:paren]   #> "> Private and shared\nThe shared one\n\n\n> One shared\n+ Animalzz\n\n> Shared and installed\nShared\n\n\n> Private, shared, installed\nShared\n\n\n"
        result = result.select{|o| o =~ /^\(#{paren}\) /}
      end

      result
    end

    def self.diff_headings before, after

      before = Notes.extract_headings before, :exclude_just_upvotes=>1
      after = Notes.extract_headings after, :exclude_just_upvotes=>1

      create = after - before
      delete = before - after

      [create, delete]
    end



    def self.in_home_xiki_dir? filename
      filename.start_with? File.expand_path("~/xiki")+"/"
    end

    def self.replace_section txt, heading, section_txt

      # Add linebreaks to end if not enough
      section_txt = "#{section_txt.sub(/\n+\z/, '')}\n\n\n" if section_txt[/(\n*)\z/].length < 3

      # Look for heading
      index = txt ? txt.index(/^#{Regexp.escape heading}$/) : nil

      # If doesn't exist yet, add to top
      if ! index
        return txt.replace "#{heading}\n#{section_txt}#{txt}"
      end


      index += heading.length

      before = txt[0..index]
      after = txt[index..-1]   # Part we're replacing and everything afterward
      heading_after = after =~ /^> /
      # Text to grab after section begins at next heading (if there is one)   #> "New\nsection added!"
      after = heading_after ? after[heading_after..-1] : ""   # If no heading, we replace all   #> 0

      txt.replace "#{before}#{section_txt}#{after}"
    end


    def self.prepend_section txt, section
      # Add linebreaks to end if not enough
      section = "#{section.sub(/\n+\z/, '')}\n\n\n" if section[/(\n*)\z/].length < 3
      txt.replace "#{section}#{txt}"
    end


    def self.delete_section txt, heading

      # Look for heading
      index = txt ? txt.index(/^#{Regexp.escape heading}$/) : nil

      # If doesn't exist, do nothing
      return nil if ! index

      before = txt[0..index]
      # Intentionally include 1st char of match (for weirdness when [0..0]) then chop off
      before.sub! /.\z/, ''   #> ""

      after = txt[index+1..-1]   # Part we're replacing and everything afterward   #> " One\nHey.\n\n> Two\nHey hey.\n"
      heading_after = after =~ /^>($| )/

      # Text to grab after section begins at next heading (if there is one)   #> "New\nsection added!"
      after = heading_after ? after[heading_after..-1] : ""   # If no heading, we replace all   #> 0

      txt.replace "#{before}#{after}"

    end


    def self.fix_numbers
      section = View.block_positions
      txt = View.txt section[1], section[2]
      i = 0
      txt.gsub!(/^( *)[0-9]+\.( |\n)/) do |match|
        i += 1
        "#{$1}#{i}.#{$2}"
      end

      orig = Location.new
      View.delete section[1], section[2]
      View << txt
      orig.go
      Move.to_end

    end


    def self.wiki_headings_to_hash txt, options={}

      filter = options[:filter]

      # Reading the file and extract headings and txt's

      chunks = txt.split(/(^>(?:| .*)\n)/)[1..-1]

      # None found (maybe blank file)
      return {} if ! chunks

      hash, previous_heading = {}, nil
      # Store last heading

      chunks.each do |chunk|
        # Heading, so remember and go next
        if chunk =~ /\A>(| .*)\n\z/
          previous_heading = chunk.strip
          next
        end

        # It's a section body > so add to hash using last heading

        # Ignore if blank
        next if previous_heading !~ /\w/

        # Only add if matches the filter

        if filter
          next if previous_heading !~ filter
        end

        # Ad to hash if doesn't exist yet
        hash[previous_heading] ||= chunk

      end

      hash
    end


    def self.blank_out_wrapper_patterns txt
      # |:... under shell commands
      # |:... by itself

      # txt.gsub!(/t/, "--")
      # txt.gsub!(/^[^ \n].*\n  \|:.+\n+/, "")
      txt.gsub!(/^[^ \n].*\n  \|:.+\n/, "\n\n")

      txt.gsub!(/^ *\|:.+\n/, "\n")

      # txt.sub!(/(^[$%&] .+\n  \|:.+\n+)+/, '')

    end


    def self.extract_upvotes command, txt

      headings = Notes.wiki_headings_to_hash txt, :filter=>/\A> [^@\n]/

      label_regex = XikihubClient.comment_or_upvote_label_regex "\\w+"

      upvotes = []
      headings.each do |heading, txt|

        matches = txt.scan(/#{label_regex}/)
        matches.flatten.each do |match|
          user = match[/@(\w+)/, 1]
          path = Notes.heading_to_path heading.sub(/@ /, ''), :url=>1
          key = "@#{user}/#{command.gsub(" ", "_")}/#{path}"
          upvotes << key
        end

      end

      upvotes
    end


    # def self.extract_upvoted_headings txt
    def self.extract_upvoted_headings section_hash

      label_regex = XikihubClient.comment_or_upvote_label_regex "\\w+"

      headings = []
      section_hash.each do |heading, txt|
        next if heading !~ /^> @ /
        next if txt !~ /#{label_regex}/

        headings << heading
      end

      headings
    end



    def self.extract_headings_and_actions txt

      hash = Notes.wiki_headings_to_hash(txt)
      headings = hash.map do |heading, txt|

        match = heading.match(/> (@\w* )?(\..+)/)

        next heading if ! match
        user, words = match[1..2]

        # Catch case where just upvotes
        next heading if Notes.blank_out_upvotes_and_comments(txt) !~ /./


        "+ #{user}#{Notes.heading_to_path(words).downcase}"
      end

      headings.delete_if{|o| o =~ /^> - /}

      headings.join("\n")

    end

    def self.blank_out_upvotes_and_comments txt

      upvote_regex = XikihubClient.comment_or_upvote_label_regex "\\w+", :upvotes=>1
      comment_regex = XikihubClient.comment_or_upvote_label_regex "\\w+", :comments=>1

      txt = txt.gsub /#{upvote_regex}/, ""
      txt = txt.gsub(/#{comment_regex}\n.*?(\n\n\n|\z)/m) do
        "\n" * $&.scan("\n").size
      end

      txt
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

