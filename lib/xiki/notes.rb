require 'xiki/styles'
require 'xiki/line'
require 'xiki/effects'
require 'xiki/view'
require 'xiki/keys'
require 'xiki/clipboard'

class Notes

  LABEL_REGEX = /(?:[a-zA-Z0-9 _-]+\) )?/

  def self.menu
    %`
    - api/
      > Turn notes wiki text into html
      @ Notes.to_html "> Heading\\njust text\\n"
    `
  end

  def self.block regex="^> "
    left, after_header, right = View.block_positions regex
    View.txt after_header, right
  end

  def self.narrow_block options={}

    delimiter = options[:delimiter] || ">"

    # If nothing hidden, hide all but current
    if $el.point_min == 1 && ($el.buffer_size + 1 == $el.point_max)
      left, after_header, right = View.block_positions "^#{delimiter}\\( \\|$\\)"
      $el.narrow_to_region left, right
      return
    end
    # Otherwise, expand all, go to next heading, hide all but current
    $el.widen
    Notes.to_block
    left, after_header, right = View.block_positions "^#{delimiter}\\( \\|$\\)"
    $el.narrow_to_region left, right
  end

  def self.archive
    block = get_block
    block.archive
  end

  def self.show_text
    block = get_block
    block.show_text
  end

  def self.hide_text
    block = get_block
    block.hide_text
  end

  def self.to_block up=false

    regex = self.heading_regex Keys.prefix

    prefix = Keys.prefix :clear=>1

    times = prefix.is_a?(Fixnum) ? prefix : 1

    if up
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
  end

  def self.heading_regex prefix=nil
    prefix == :u ?
      "^[>=]\\{1,2\\}\\( \\|$\\)" :
      "^[>=]\\( \\|$\\)"
  end

  def self.move_block up=false
    regex = self.heading_regex Keys.prefix

    times = Keys.prefix_times

    orig = Location.new

    prefix = Keys.prefix :clear=>1

    block = get_block prefix == :u ? 2 : 1
    block.delete_content

    if up
      times.times do
        Search.backward regex, :go_anyway=>true
      end
      $el.insert block.content
      Search.backward regex
    else

      move_regex = prefix == :u ? "^>" : regex

      Search.forward move_regex
      times.times do
        Search.forward move_regex, :go_anyway=>true
      end
      Move.to_axis

      View.insert block.content

      Search.backward move_regex
    end
    moved_block = get_block regex

    times == 1 ?
      moved_block.fade_in :
      orig.go
  end

  def self.insert_heading options={}
    Line.start
    orig = Line.value

    times = Keys.prefix_u? ? 1 : (Keys.prefix || 1)
    times.times { $el.insert ">" }
    View.insert " " # unless times > 1

    if options[:extra_space] || Keys.prefix_u?   # If U create blank lines.
      View.insert("\n"*4, :dont_move=>1)
      return
    end

    View.insert("\n", :dont_move=>1) if orig != ""
  end

  def self.cut_block no_clipboard=false
    block = get_block Keys.prefix

    block.blink

    unless no_clipboard
      Clipboard.set("0", block.content)
    end
    block.delete_content
  end

  def self.copy_block
    prefix = Keys.prefix
    block = get_block prefix == :u ? 2 : 1
    block.blink
    Clipboard.set("0", Keys.prefix_u ? block.text : block.content)
  end

  def self.move_block_to_top no_clipboard=false

    prefix_u = Keys.prefix_u :clear=>true
    block = get_block

    if prefix_u
      line = View.line_number
      scroll = View.scroll_position
      orig_right = block.right
    end

    block.fade_out unless prefix_u

    block.delete_content

    $el.beginning_of_buffer
    $el.insert block.content

    if prefix_u
      View.to_line line
      View.scroll_position = scroll
      View.cursor = orig_right
      Line.next
    else
      View.to_line 2
    end

    moved_block = get_block

    moved_block.fade_in unless prefix_u

  end

  def self.keys

    return if ! $el

    # Get reference to map if already there (don't mess with existing buffers)
    $el.elvar.notes_mode_map = $el.make_sparse_keymap unless $el.boundp :notes_mode_map

    Keys.custom_archive(:notes_mode_map) { Notes.archive }
    Keys.custom_back(:notes_mode_map) { Notes.move_block :backwards }   # Move block up to before next block
    Keys.custom_clipboard(:notes_mode_map) { Notes.copy_block }   # block -> clipboard
    Keys.custom_delete(:notes_mode_map) { Notes.cut_block :backwards }   # block -> clear
    #     Keys.custom_expand(:notes_mode_map) { Notes.narrow_block }   # Show just block
    Keys.custom_forward(:notes_mode_map) { Notes.move_block }   # Move block down to after next block
    Keys.custom_heading(:notes_mode_map) { Notes.insert_heading }   # Insert |... etc. heading
    Keys.custom_item(:notes_mode_map) { Agenda.quick_add_line }
    # j
    Keys.custom_kill(:notes_mode_map) { Notes.cut_block }   # block -> cut
    # l
    Keys.custom_mask(:notes_mode_map) { Notes.hide_text }   # block -> hide
    Keys.custom_next(:notes_mode_map) { Notes.to_block }   # Go to block after next block
    Keys.custom_open(:notes_mode_map) { Notes.show_text }   # block -> reveal
    Keys.custom_previous(:notes_mode_map) { Notes.to_block :backwards }   # Go to block before next block
    # q
    # r
    Keys.custom_stamp(:notes_mode_map) { $el.insert Time.now.strftime("- %Y-%m-%d %I:%M%p: ").downcase.sub(' 0', ' ') }
    Keys.custom_top(:notes_mode_map) { Notes.move_block_to_top }   # block -> top
    # u
    # v
    # w
    Keys.custom_x(:notes_mode_map) { Notes.cut_block }   # block -> cut
    # y
    # z

    $el.define_key(:notes_mode_map, $el.kbd("<double-mouse-1>"), :notes_mouse_double_click)
    $el.define_key(:notes_mode_map, $el.kbd("<mouse-1>"), :notes_mouse_toggle)

    $el.define_key(:notes_mode_map, $el.kbd("<M-mouse-1>"), :notes_mouse_double_click)
    $el.define_key(:notes_mode_map, $el.kbd("<S-mouse-1>"), :notes_mouse_double_click)
    $el.define_key(:notes_mode_map, $el.kbd("<C-mouse-1>"), :notes_mouse_double_click)


    $el.define_key(:notes_mode_map, $el.kbd("C-i")) { Notes.tab_key }
  end

  def self.define_styles

    return if ! $el

    # - foo (r): <here>
    Styles.define :notes_light_gray, :fg => "bbb"

    # - foo (r): <here>
    Styles.define :variable, :face => 'verdana'

    # - foo (r): <here>
    Styles.define :notes_label_parens,
      :fg => "bbb",
      :size => "-2",
      :face => 'arial'

    # |...
    h1_size = "+3"

    # Colors of "| ..." headings
    if Styles.dark_bg?   # If black bg
      @@h1_styles = {
        :notes_h1 =>"333",
        :notes_h1r=>"611",   # | r This will be red
        :notes_h1o=>"841",   # | o This will be orange
        :notes_h1y=>"871",
        :notes_h1e=>"363",
        :notes_h1g=>"363",
        :notes_h1b=>"678",
        :notes_h1p=>"636",
        :notes_h1m=>"622",
        :notes_h1x=>"345",
        :notes_h1t=>"055",
        }
    else
      # Colors of headings
      @@h1_styles = {
        :notes_h1 =>"aaa",
        :notes_h1r=>"b66",   # | r This will be red
        :notes_h1o=>"b83",   # | o This will be orange
        :notes_h1y=>"bb3",
        :notes_h1e=>"363",
        :notes_h1g=>"7b6",
        :notes_h1b=>"678",
        :notes_h1p=>"b8b",
        :notes_h1m=>"944",
        :notes_h1x=>"678",
        :notes_h1t=>"055",
        }
    end

    @@h1_styles.each do |k, v|
      pipe = v.gsub(/./) {|c| (c.to_i(16) + "3".to_i(16)).to_s(16)}
      label = v.gsub(/./) {|c| (c.to_i(16) + "6".to_i(16)).to_s(16)}
      Styles.define k,
        :face=>'arial', :size=>h1_size, :fg=>'ffffff', :bg=>v, :bold=> true
      Styles.define "#{k}_pipe".to_sym, :face=>'arial', :size=>h1_size, :fg=>pipe, :bg=>v, :bold=>true
      Styles.define "#{k}_label".to_sym, :face=>'arial', :size=>h1_size, :fg=>label, :bg=>v, :bold=>true
    end

    Styles.define :notes_h1_agenda_pipe, :face => 'arial', :size => h1_size, :fg => '88cc88', :bg => '336633', :bold =>  true
    Styles.define :notes_h1_agenda, :face => 'arial', :size => h1_size, :fg => 'ffffff', :bg => '336633', :bold => true

    # |||...
    Styles.define :notes_h3,
      :face => 'arial', :size => "-1",
      :fg => '999',#, :bg => "9999cc",
      :bold =>  true
    Styles.define :notes_h3_pipe,
      :face => 'arial', :size => "-1",
      :fg => '333'

    # ||||...
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
      #:face=>'courier', :size=>"0",  # Mac
      :fg=>label_color, :bold=>true

    Styles.define :notes_bullet_parens,
      :face => 'arial', :size => "-2",
      :fg => "ee7700", :bold => true

    # Strikethrough
    Styles.define(:strike, :strike=>true)

    # - <here> (r): foo
    Styles.define :notes_label_link,
      :face => 'verdana', :size => "-1",
      :fg => "66f",
      :bold => true, :underline => true

    Styles.define :notes_g, :fg=>"6cf", :face=>'arial black', :size=>"0", :bold=>true
    Styles.define :notes_blue, :fg=>"69f", :face=>'arial black', :size=>"0", :bold=>true
    Styles.define :notes_red, :fg=>"c55", :face=>'arial black', :size=>"0", :bold=>true
    Styles.define :notes_yellow, :fg=>"CC0", :face=>'arial black', :size=>"0", :bold=>true
    Styles.define :notes_green, :fg=>"3C3", :face=>'arial black', :size=>"0", :bold=>true


    if Styles.dark_bg?   # If black bg
      # >>...
      Styles.define :notes_h2, :face=>'arial', :size=>"-1", :fg=>'fff', :bg=>"333", :bold=>true
      Styles.define :notes_h2_pipe, :face=>'arial', :size=>"-1", :fg=>'555555', :bg=>"333333", :bold=> true
    else
      Styles.define :notes_h2, :face=>'arial', :size=>"-1", :fg=>'fff', :bg=>"999", :bold=>true
      Styles.define :notes_h2_pipe, :face=>'arial', :size=>"-1", :fg=>'bbb', :bg=>"999", :bold=>true
    end

    if Styles.dark_bg?   # If black bg
      Styles.dotted :bg=>'080808', :fg=>'111', :strike=>nil, :underline=>nil, :border=>['111', -1]
    else
      Styles.dotted :bg=>'eee', :fg=>'ddd', :strike=>nil, :underline=>nil, :border=>['ddd', -1]
    end

    notes_exclamation_color = Styles.dark_bg? ? "7c4" : "5a0"

    Styles.define :notes_exclamation,  # Green bold text
      :face=>'arial black', :size=>"0",
      :fg=>notes_exclamation_color, :bold=>true

    Styles.notes_link :fg=>(Styles.dark_bg? ? "9ce" : "08f")

    Styles.shell_prompt :fg=>'#888', :bold=>1

  end

  def self.apply_styles
    # Don't format quotes (it overrides the following formatting)
    Styles.clear

    # >... lines (headings)
    Styles.apply("^\\(>\\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)

    Styles.apply("^\\(> \\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
    Styles.apply("^\\(>> \\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)
    Styles.apply("^\\(= \\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
    Styles.apply("^\\(== \\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)

    Styles.apply("^\\(>\\)\\( .+?: \\)\\(.+\n\\)", nil, :notes_h1_pipe, :notes_h1_label, :notes_h1)

    Styles.apply("^\\(> 20[0-9][0-9]-[0-9][0-9]-[0-9][0-9].*:\\)\\(.*\n\\)", nil, :notes_h1_agenda_pipe, :notes_h1_agenda)

    @@h1_styles.each do |k, v|
      l = k.to_s[/_..(.)$/, 1]
      next unless l
      Styles.apply("^\\(> #{l}\\)\\(\n\\| .*\n\\)", nil, "#{k}_pipe".to_sym, k)
      Styles.apply("^\\(>\\)\\( #{l} .+: \\)\\(.*\n\\)", nil, "#{k}_pipe".to_sym, "#{k}_label".to_sym, k)
    end

    # >>... lines
    #     Styles.apply("^\\(>>\\)\\(.*\\)", nil, :notes_h2_pipe, :notes_h2)
    Styles.apply("^\\(>>\\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)

    # Commented
    Styles.apply("^\\(>> .+?: \\)\\(.+\n\\)", nil, :notes_h2_pipe, :notes_h2)

    # >>>... lines
    Styles.apply("^\\(>>>\\)\\(.*\n\\)", nil, :notes_h3_pipe, :notes_h3)

    # >>>... lines
    Styles.apply("^\\(>>>>\\)\\(.*\n\\)", nil, :notes_h4_pipe, :notes_h4)

    # - bullets with labels and comments
    Styles.apply("^[ \t]*\\([<+-]\\) \\([^/:\n]+:\\) ", nil, :ls_bullet, :notes_label)   # - hey: you
    Styles.apply("^[ \t]*\\([<+-]<*\\) \\([^(\n]+?)\\) ", nil, :ls_bullet, :notes_label)   # - hey) you

    Styles.apply("^[ \t]*\\([<+=-]<*\\) \\([^(\n]+)\\)$", nil, :ls_bullet, :notes_label)   # - hey)
    Styles.apply("^[ \t]*\\([<+=-]<*\\) \\(.+:\\)$", nil, :ls_bullet, :notes_label)   # - hey)

    #     Styles.apply("^[ \t]*\\(x\\)\\( \\)\\(.+\\)", nil, :notes_label, :variable, :strike)

    Styles.apply("^\\([ \t]*\\)\\([<+-]\\) \\(.+?:\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)
    Styles.apply("^\\([ \t]*\\)\\([<+-]\\) \\([^(\n]+?)\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)

    Styles.apply("^ +\\(!.*\n\\)", nil, :ls_quote)   # ^!... for commands

    # exclamation! / todo
    Styles.apply("^[ \t]*\\([<+-]\\) \\(.*!\\)$", nil, :ls_bullet, :notes_exclamation)
    Styles.apply("^ +\\(!\\+.*\n\\)", nil, :diff_green)   # Whole lines
    Styles.apply("^ +\\(!-.*\n\\)", nil, :diff_red)

    Styles.apply("\\(\(-\\)\\(.+?\\)\\(-\)\\)", nil, :diff_small, :diff_red, :diff_small)
    Styles.apply("\\(\(\\+\\)\\(.+?\\)\\(\\+\)\\)", nil, :diff_small, :diff_green, :diff_small)

    # google/
    Styles.apply "^ *\\(-?\\) ?\\(@?\\)\\(g\\)\\(o\\)\\(o\\)\\(g\\)\\(l\\)\\(e\\)\\(/\\)", nil, :ls_bullet, :ls_dir,
      :notes_blue, :notes_red, :notes_yellow, :notes_blue, :notes_green, :notes_red,
      :ls_dir

    Styles.apply "^hint/.+", :fade6

    Styles.apply "^[< ]*@? ?\\([%$&]\\) ", nil, :shell_prompt   # Colorize shell prompts

    Styles.apply("^ *\\(|`\\)\\(.*\n\\)", nil, :quote_heading_pipe, :dotsies_experimental)
    Styles.apply("^ *\\(|~\\)\\([^\n~]+\\)\\(~?\\)", nil, :quote_heading_pipe, :dotsies, :quote_heading_pipe)

  end

  # Startup
  def self.init

    return if ! $el

    $el.defun(:notes_mouse_meta_click, :interactive => "e") do |e|
      $el.mouse_set_point(e)
      View.insert "hey"
    end

    $el.defun(:notes_mouse_double_click, :interactive => "e") do |e|
      next Launcher.insert "h" if Line =~ /^$/   # If blank line, launch history
      Launcher.launch_or_hide(:blink=>true)
    end

    $el.defun(:notes_mouse_toggle, :interactive => "e") do |e|
      $el.mouse_set_point(e)
      Notes.mouse_toggle
    end

    $el.defun(:notes_mode, :interactive => "", :docstring => "Apply notes styles, etc") {# |point|
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"

      FileTree.apply_styles
      Notes.apply_styles
      FileTree.apply_styles_at_end
      $el.use_local_map $el.elvar.notes_mode_map
    }
    $el.el4r_lisp_eval %q<
      (progn
        (add-to-list 'auto-mode-alist '("\\\\.notes\\\\'" . notes-mode))
        (add-to-list 'auto-mode-alist '("\\\\.xik\\\\'" . notes-mode))
        (add-to-list 'auto-mode-alist '("\\\\.wik\\\\'" . notes-mode)))
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

    Move.to_end if Line.before_cursor =~ /^ +$/   # If at awkward position, move

    cursor = View.cursor
    line = Line.value
    indent = Line.indent line
    pipe = line =~ /^ *([|#])/ ? $1 : ""
    if Line.left == cursor || Line.right == cursor   # If beginning or end, leave current line alone
      Move.to_end
    else   # In middle of line
      Deletes.delete_whitespace
    end

    return View.<< "\n#{line[/^[ |#]*/]}  " if pipe
    View << "\n#{indent}#{pipe}  "
  end

  def self.bullet bullet_text="- "
    prefix = Keys.prefix :clear=>true

    if prefix == :u
      Move.forward if Line.at_right?
      return Tree.collapse
    end

    if prefix == :uu
      return Tree.collapse :replace_parent=>1
    end

    line = Line.value
    indent = Line.indent indent

    if line.present?   # If non-blank line
      Move.to_end if line =~ /^ / && View.column <= indent.length   # If just entered a bullet, go to end first

      Move.to_end if line =~ /^[+-] / && View.column <= 2   # If just entered a bullet, go to end first

      # If at beginning of line, just insert bullet
      return View.insert "- " if View.column == 0 && bullet_text == "- " && line !~ /^ /

      if View.cursor != Line.right
        Deletes.delete_whitespace
      end
      View.insert "\n"

      # Do simple case if quoted
      return View.<<("#{line[/^[ |#]*/]}  - ") if line =~ /^ *[|#]/

      # Do simple case if on heading
      return View.<<("- ") if line =~ /^>/
    end

    if prefix.is_a? Fixnum   # If numeric prefix, indent by n
      View.insert((" " * prefix) + bullet_text)
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

  def self.mouse_toggle
    #Launcher.launch_or_hide(:blink=>true)

    # If next line is indented more, kill children
    # If starts with plus or minus, and on plus or minus, launch
    if Line.matches(/^\s*[+-]/) and View.char =~ /[+-]/
      plus_or_minus = Tree.toggle_plus_and_minus
      if ! Tree.children?
        #plus_or_minus == '+'   # If +, expand (launch

        if FileTree.dir? or ! FileTree.handles?   # If on a dir or code_tree
          Launcher.launch
        else   # If on a file in a FileTree
          FileTree.enter_lines
        end

      else   # If -, kill under
        Tree.kill_under
        Line.to_beginning
      end
    end
  end

  # Returns an instance of BlockNotes representing the block the point is currently in
  #   def self.get_block regex="^[|>]\\( \\|$\\)"
  def self.get_block regex=nil
    regex ||= self.heading_regex

    regex = self.heading_regex if regex == 1
    regex = self.heading_regex(:u) if regex == 2 || regex == :u

    left, after_header, right = View.block_positions regex
    NotesBlock.new(left, after_header, right)
  end

  def self.to_html txt
    txt = txt.
      gsub(/^> (.+)/, "<h1>\\1</h1>").
      gsub(/(^|[^\n>])$/, "\\0<br>")
  end

  def self.as_nav

    prefix = Keys.prefix :clear=>true
    txt = ""
    if prefix == :u || prefix == :uu
      txt = Code.grab_containing_method
    end

    label = nil
    if prefix == 9
      label = Keys.input :prompt=>"label: ", :timed=>1
      label = "do" if label.blank?
      label = Notes.expand_if_action_abbrev(label) || label

      prefix = nil
    end

    if prefix == :uu   # up+up means add function and line
      txt << "\n#{Line.value}"
    elsif prefix != :u
      txt = View.txt_per_prefix prefix, :selection=>1, :default_is_line=>1, :just_txt=>1
    end

    # If file has bullet or ends with slash, grab path

    keep_tweeking = true
    if ! prefix && FileTree.handles?   # Grab tree
      txt = Tree.ancestors_indented :just_sub_tree=>1
      txt.sub! /^  /, '  - '
      keep_tweeking = false
    end

    file = View.file
    orig = Location.new

    if keep_tweeking
      if Search.fit_in_snippet(txt)   # Insert it in existing tree if there
        View << "    - #{label}:\n" if label
        return orig.go
      end
    else
      View.layout_files :no_blink=>1
    end

    # Make it quoted, unless already a quote
    if keep_tweeking && (txt !~ /\A([+-] |\/)/ || txt !~ /^ +/)   # If txt isn't already a tree, make it one
      txt = FileTree.snippet :txt=>txt, :file=>file
      txt.sub! /^    /, "    - #{label}:\n    " if label
    end

    # Else, add it to top...

    View.to_highest

    if prefix == 8
      if Line =~ /^>/
        Line.next
      end
      result = "#{txt}\n"
    else
      result = ">\n#{txt}\n"
    end

    View.<< result, :dont_move=>1

    orig.go
  end


  def self.as_todo
    prefix = Keys.prefix :clear=>1

    txt = nil

    # If method, make it Foo.bar method call
    line = Line.value

    if View.file =~ /_spec.rb/ && line =~ /^ *(it|describe) /
      return Specs.enter_as_rspec
    end

    buffer_name = $el.buffer_name
    file_name = View.file_name
    path = Xiki.path rescue nil

    if prefix.nil?   # So 1+ or numeric prefix just grab normally
      if buffer_name == "*ol"   # Make it into "foo = bar" format
        txt = line[/\) (.+)/, 1]
        txt.sub!(": ", " = ") if txt
        txt ||= line[/ *- (.+?) /, 1]

      elsif path && path.last =~ /(\w+)\.rb\/\| *def ([\w\.?]+)/
        clazz = $1
        method = $2
        clazz = TextUtil.camel_case clazz if method.slice! /^self\./
        txt = "#{clazz}.#{method}"

      elsif line =~ /^ +def (.+)/   # Make it into Foo.bar format
        method = $1
        clazz = file_name[/\w+/]
        clazz = TextUtil.camel_case clazz if method.slice! /^self\./

        txt = "#{clazz}.#{method}"

      elsif line =~ /^ *\|/   # Make it into Foo.bar format
        txt = line.sub /^ *\| ?/, ''
      elsif FileTree.handles?
        txt = Tree.dir
      elsif line =~ /(^ *[+-] |\/$)/   # Make it into Foo.bar format
        txt = Xiki.path.last
      end
    end

    txt ||= View.txt_per_prefix(prefix, :selection=>1, :just_txt=>1, :default_is_line=>1)
    txt.strip! if txt =~ /\A.+\n\z/   # Strip when only 1 linebreak

    options = prefix == :uu ? {:append=>1} : {}
    Search.move_to "$t", txt, options
  end


  class NotesBlock

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

    def delete_content
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
      delete_content
      filename = 'archive.' + $el.file_name_nondirectory(buffer_file_name)
      timestamp = "--- archived on #{Time.now.strftime('%Y-%m-%d at %H:%M')} --- \n"
      $el.append_to_file timestamp, nil, filename
      $el.append_to_file content, nil, filename
    end
  end

  def self.enter_note
    # If on blank line, just insert it
    indent = ""
    if ! Line.blank?
      line = Line.value
      indent, first_char = line.match(/^( *)(.)/)[1..2]

      Move.to_axis
      $el.open_line 1
    end

    Line << "#{indent}- !"
    Move.backward

    txt = Keys.input :timed=>1, :prompt=>'Enter a character: '

    expanded = Notes.expand_if_action_abbrev txt

    View << (expanded || txt)

    # If wasn't expanded prepare to edit
    ControlLock.disable if expanded == txt

    if expanded
      # Do nothing
    else
      View << "#{indent}- !"
      Move.backward
      View << txt
    end

    nil
  end

  def self.drill file, heading=nil, *content

    prefix = Keys.prefix :clear=>true
    content = content.any? ? content.join('/') : nil

    file_orig = file.dup
    file = Bookmarks[file]

    # If bookmark wasn't found, complain
    if file =~ /^\$(\w+)/
      bm = $1
      return "| Set the following bookmark first. Then you'll be able to use this menu to\n| browse the file. The file should have '> ...' headings.\n\n@ $#{bm}\n"
    end

    # If docs/, output docs string...

    if heading == "docs"
      message = "
        > Summary
        | Convenient way for browsing the headings in this file:
        |
        - @ #{file}
        ".unindent
      return message
    end

    if ! File.exists? file
      return "
        | File doesn't exist yet, do as+update to create it:
        @ #{file}
          | > Heading
          | Stuff
        "
    end

    txt = File.open(file, 'rb') {|f| f.read}

    # If just file passed, headings...

    if ! heading

      return View.open file if prefix == "open"   # If as+open, just jump there

      txt = txt.split("\n")
      txt = txt.grep /^\>( .+)/
      return "| This file has no '>...' headings:\n@ #{file}" if txt.empty?
      return txt.join("\n")  #.gsub /^> /, '| '
    end

    # If just heading passed, show text under heading...
    heading.sub!(/^\| /, '> ')
    escaped_heading = Regexp.escape heading
    if ! content
      if prefix == :u || prefix == "open"   # If C-u on a heading, just jump there
        View.open file
        View.to_highest
        Search.forward "^#{$el.regexp_quote heading}$", :beginning=>1
        View.recenter_top
        return
      end

      txt = self.extract_block txt, heading
      ENV['no_slash'] = "1"
      return txt.gsub(/^/, '| ').gsub(/^\| $/, '|')
    end

    # If content passed

    # If C-4, grab text and save it to file / update
    if prefix == "update"
      # Update difflog

      # Grab before and after
      index = txt.index /^#{escaped_heading}$/
      index += heading.length

      before = txt[0..index]

      after = txt[index..-1].sub(/.*?^>( |$)/m, ">\\1")

      content = Tree.siblings :string=>1

      txt = "#{before}#{content}#{after}"

      DiffLog.save_diffs :patha=>file, :textb=>txt

      # return
      File.open(file, "w") { |f| f << txt }

      # Revert file if it's open?

      View.flash "- Saved!"
      return
    end

    # Navigate to heading, then content

    View.open file
    View.to_highest
    Search.forward "^#{$el.regexp_quote heading}$"
    View.recenter_top
    Search.forward "^#{$el.regexp_quote content.sub(/^\| /, '')}"
    Move.to_axis
    nil
  end

  def self.extract_block txt, heading
    txt = txt.sub /.*^#{Regexp.escape heading}\n/m, ''   # Delete before block
    txt.sub! /^>( |$).*/m, ''   # Delete after block
    txt  # = txt.gsub(/^.?/, "|\\1")   # Escape with pipes
  end

  def self.read_block file, heading
    self.extract_block File.read(file), heading
  end

  def self.tab_key
    indent = Line.indent(Line.value 0)
    Line.sub! /^ */, indent
    Line.to_beginning
  end

  @@single_letter_abbrev = {
    "f"=>"fix",
    "b"=>"borrow",
    "i"=>"implement",
    "d"=>"do",
    "de"=>"delete",
    "r"=>"rename",
    "t"=>"todo",
    "e"=>"extract",
    "er"=>"error",
    }

  # If the string is "t" or "i", or a few others, return "todo" or "imprement" etc. respectively.
  def self.expand_if_action_abbrev txt
    @@single_letter_abbrev[txt] || txt
  end

  def self.do_as_quote
    # Make this add or remove quotes
  end

  def self.from_markdown_format txt
    txt = txt.gsub(/^#+/){"#{'>' * $&.length}"}
  end

end

Notes.define_styles
Notes.init
Notes.keys  # Define local keys

# TODO - how to turn these on conditionally?
  # What's the best approach for presentations?
    # Probably make .deck files use notes mode
      # Why wasn't working before?
# require 'deck'
# Deck.keys :notes_mode_map   # Temporary - only when doing presentations
