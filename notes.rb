require 'styles'
require 'line'
require 'effects'
require 'view'
require 'keys'
require 'clipboard'

class Notes
  extend ElMixin

  def self.menu
    puts "- .help_wiki_format"
  end

  def self.block regex="^| "
    left, after_header, right = View.block_positions regex
    buffer_substring(after_header, right)
  end


  def self.expand_block up=false
    # If nothing hidden, hide all but current
    if point_min == 1 && (buffer_size + 1 == point_max)
      left, after_header, right = View.block_positions "^| "
      narrow_to_region left, right
      return
    end
    # Otherwise, expand all, go to next heading, hide all but current
    widen
    Notes.to_block
    left, after_header, right = View.block_positions "^| "
    narrow_to_region left, right
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
    if up
      (Keys.prefix || 1).times do
        Line.to_left
        re_search_backward "^| "
      end
    else
      (Keys.prefix || 1).times do
        Line.next if Line.matches(/^\| /)
        re_search_forward "^| "
        Line.to_left
      end
    end
  end

  def self.move_block up=false
    block = get_block
    block.blink
    block.delete_content

    if up
      (Keys.prefix || 1).times do
        re_search_backward "^| ", nil, 1
      end
      insert block.content
      search_backward_regexp "^| "
    else
      re_search_forward "^| "
      (Keys.prefix || 1).times do
        re_search_forward "^| ", nil, 1
      end
      beginning_of_line
      insert block.content
      search_backward_regexp "^| "
    end
    moved_block = get_block
    moved_block.blink
  end

  def self.insert_heading
    Line.start

    times = Keys.prefix_u? ? 1 : (Keys.prefix || 1)
    times.times { insert "|" }
    insert " "

    open_line(4) if Keys.prefix_u?   # If U create blank lines.

    #PauseMeansSpace.go

    # Exit control lock mode if on
    #ControlLock.disable
  end

  def self.cut_block no_clipboard=false
    block = get_block
    block.blink
    unless no_clipboard
      Clipboard.set("0", block.content)
    end
    block.delete_content
  end

  def self.move_block_to_top no_clipboard=false
    block = get_block
    block.blink
    block.delete_content

    beginning_of_buffer
    insert block.content

    goto_line 2
    moved_block = get_block
    moved_block.blink
  end

  def self.keys
    # Get reference to map if already there (don't mess with existing buffers)
    elvar.notes_mode_map = make_sparse_keymap unless boundp :notes_mode_map

    Keys.CA(:notes_mode_map) { Notes.archive }

    Keys.CO(:notes_mode_map) { Notes.show_text }
    Keys.CM(:notes_mode_map) { Notes.hide_text }

    Keys.CT(:notes_mode_map) { Notes.move_block_to_top }

    Keys.CD(:notes_mode_map) { Notes.cut_block true }
    Keys.CK(:notes_mode_map) { Notes.cut_block }

    Keys.CH(:notes_mode_map) { Notes.insert_heading }  # Insert ||... etc. heading
    Keys.CN(:notes_mode_map) { Notes.to_block }  # Go to block after next block
    Keys.CP(:notes_mode_map) { Notes.to_block true }  # Go to block before next block
    Keys.CE(:notes_mode_map) { Notes.expand_block }  # Show just block

    Keys.CF(:notes_mode_map) { Notes.move_block }  # Move block down to after next block
    Keys.CB(:notes_mode_map) { Notes.move_block true }  # Move block up to before next block

    define_key :notes_mode_map, kbd("C-\\") do

      widen; Hide.show
      Hide.hide_unless /^\| /
      recenter -2
      Hide.search
    end

    # Make right-click launch a line
    define_key(:notes_mode_map, kbd("<mouse-3>"), :notes_mouse_launch)
    define_key(:notes_mode_map, kbd("<double-mouse-1>"), :notes_mouse_double_click)
    define_key(:notes_mode_map, kbd("<mouse-1>"), :notes_mouse_toggle)

  end

  def self.define_styles

    # - foo (r): <here>
    Styles.define :notes_light_gray,
      :fg => "bbb"

    # - foo (r): <here>
    Styles.define :variable,
      :face => 'verdana' #, :size => "+2"

    # - foo (r): <here>
    Styles.define :notes_label_parens,
      :fg => "bbb",
      :size => "-2",
      :face => 'arial'

    # |...
    #h1_size = "+2"

    Styles.define :notes_h1,
      :face => 'arial', :size => "+2",
      :fg => 'ffffff', :bg => "666699",
      :bold =>  true

    Styles.define :notes_h1_pipe,
      :face => 'arial', :size => "+2",
      :fg => '9999cc', :bg => "666699",
      :bold => true



    Styles.define :notes_h1i,
      :face => 'arial', :size => "+2",
      :fg => 'ffffff', :bg => "66aa66",
      :bold =>  true
    Styles.define :notes_h1i_pipe,
      :face => 'arial', :size => "+2",
      :fg => 'aad2aa', :bg => "66aa66",
      :bold =>  true

    Styles.define :notes_h1e,
      :face => 'arial', :size => "+2",
      :fg => 'ffffff', :bg => "cccc66",
      :bold =>  true
    Styles.define :notes_h1e_pipe,
      :face => 'arial', :size => "+2",
      :fg => 'f3f399', :bg => "cccc66",
      :bold =>  true

    Styles.define :notes_h1c,
      :face => 'arial', :size => "+2",
      :fg => 'ffffff', :bg => "996699",
      :bold =>  true
    Styles.define :notes_h1c_pipe,
      :face => 'arial', :size => "+2",
      :fg => 'bb99bb', :bg => "996699",
      :bold =>  true

    Styles.define :notes_h1s,
      :face => 'arial', :size => "+2",
      :fg => 'ffffff', :bg => "449688",
      :bold =>  true
    Styles.define :notes_h1s_pipe,
      :face => 'arial', :size => "+2",
      :fg => '77c6aa', :bg => "449688",
      :bold =>  true

    Styles.define :notes_h1n,
      :face => 'arial', :size => "+2",
      :fg => 'ffffff', :bg => "eeaa33",
      :bold =>  true
    Styles.define :notes_h1n_pipe,
      :face => 'arial', :size => "+2",
      :fg => 'ffdd88', :bg => "eeaa33",
      :bold =>  true

    # ||...
    Styles.define :notes_h2,
      :face => 'arial', :size => "0",
      :fg => '8888bb', :bg => "e0e0f2",
#      :fg => 'ffffff', :bg => "ddddee",
#      :fg => 'ffffff', :bg => "aaaacc",
      :bold =>  true
    Styles.define :notes_h2_pipe,
      :face => 'arial', :size => "0",
      :fg => 'bbbbdd', :bg => "e0e0f2",
#      :fg => 'ddddf0', :bg => "ddddee",
#      :fg => 'ddddf0', :bg => "aaaacc",
      :bold =>  true


    # |||...
    Styles.define :notes_h3,
      :face => 'arial', :size => "-1",
      :fg => '9999ee',#, :bg => "9999cc",
      :bold =>  true
    Styles.define :notes_h3_pipe,
      :face => 'arial', :size => "-1",
      :fg => 'ddddf0'

    # ||||...
    Styles.define :notes_h4,
      :face => 'arial', :size => "-2",
      :fg => 'bbbbee',
      :bold =>  true
    Styles.define :notes_h4_pipe,
      :face => 'arial', :size => "-2",
      :fg => 'ddddf0'

    # Labels, emphasis
    Styles.define :notes_label,
      :face => 'arial black', :size => "0",  # Mac
      #:face => 'courier', :size => "0",  # Mac
      :fg => "dd7700", :bold => true

    Styles.define :notes_bullet_parens,
      :face => 'arial', :size => "-2",
      :fg => "ee7700", :bold => true

    Styles.define :notes_exclamation,  # Green bold text
      :face => 'arial black', :size => "0",
      :fg => "55aa22", :bold => true

    # Strikethrough
    Styles.define(:strike, :strike => true)

    # - <here> (r): foo
    Styles.define :notes_label_link,
      :face => 'verdana', :size => "-1",
      :fg => "66f",
      :bold => true, :underline => true

    Styles.define :notes_g, :fg => "00B"
    Styles.define :notes_blue, :fg => "46f"
    Styles.define :notes_red, :fg => "c00"
    Styles.define :notes_yellow, :fg => "CC0"
    Styles.define :notes_green, :fg => "0C0"

    if Styles.inverse

      Styles.define :notes_h1,
        :fg => 'ffffff', :bg => "333366"
      Styles.define :notes_h1_pipe,
        :fg => '7777aa', :bg => "333366"

      Styles.define :notes_h2,
        :bg => "181833"
      Styles.define :notes_h2_pipe,
        :fg => '333366', :bg => "181833"

      Styles.define :notes_h1e,
        :bg => "666633"
      Styles.define :notes_h1e_pipe,
        :fg => 'aaaa77', :bg => "666633"
    end
  end

  def self.apply_styles
    # Don't format quotes (it overrides the following formatting)
    Styles.clear

    # |... lines
    Styles.apply("^\\(| \\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
    Styles.apply("^\\(| .+?: \\)\\(.+\n\\)", nil, :notes_h1_pipe, :notes_h1)

    # | i ... lines
    Styles.apply("^\\(| i \\)\\(.+\n\\)", nil, :notes_h1i_pipe, :notes_h1i)
    Styles.apply("^\\(| i .+?: \\)\\(.+\n\\)", nil, :notes_h1i_pipe, :notes_h1i)

    # | e ... lines
    Styles.apply("^\\(| e \\)\\(.+\n\\)", nil, :notes_h1e_pipe, :notes_h1e)
    Styles.apply("^\\(| e .+?: \\)\\(.+\n\\)", nil, :notes_h1e_pipe, :notes_h1e)

    # | c ... lines
    Styles.apply("^\\(| c \\)\\(.+\n\\)", nil, :notes_h1c_pipe, :notes_h1c)
    Styles.apply("^\\(| c .+?: \\)\\(.+\n\\)", nil, :notes_h1c_pipe, :notes_h1c)

    # | s ... lines
    Styles.apply("^\\(| s \\)\\(.+\n\\)", nil, :notes_h1s_pipe, :notes_h1s)
    Styles.apply("^\\(| s .+?: \\)\\(.+\n\\)", nil, :notes_h1s_pipe, :notes_h1s)

    # | n ... lines
    Styles.apply("^\\(| n \\)\\(.+\n\\)", nil, :notes_h1n_pipe, :notes_h1n)
    Styles.apply("^\\(| n .+?: \\)\\(.+\n\\)", nil, :notes_h1n_pipe, :notes_h1n)

    # ||... lines
    Styles.apply("^\\(|| \\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)
    Styles.apply("^\\(|| .+?: \\)\\(.+\n\\)", nil, :notes_h2_pipe, :notes_h2)

    # |||... lines
    Styles.apply("^\\(||| \\)\\(.+\n\\)", nil, :notes_h3_pipe, :notes_h3)

    # ||||... lines
    Styles.apply("^\\(|||| ?\\)\\(.+\n\\)", nil, :notes_h4_pipe, :notes_h4)

    #     # ~emphasis~ strings
    #     Styles.apply("\\(~\\)\\(.+?\\)\\(~\\)", :notes_label)

    # - bullets with labels
    Styles.apply("^[ \t]*\\([+-]\\) \\([!#-~ ]+?:\\) ", nil, :ls_bullet, :notes_label)
    Styles.apply("^[ \t]*\\([+-]\\) \\([!#-~ ]+?:\\)$", nil, :ls_bullet, :notes_label)

    #Styles.apply("^[ \t]*\\(\\+\\)\\( \\)", nil, :ls_bullet, :variable)

    Styles.apply("^[ \t]*\\(x\\)\\( \\)\\(.+\\)", nil, :ls_bullet, :variable, :strike)

    Styles.apply("^\\([ \t]*\\)\\([+-]\\) \\(.+?:\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)

    Styles.apply("^ +\\(!.*\n\\)", nil, :ls_quote)   # ^!... for commands

    # - item exclamation! / todo
    Styles.apply("^[ \t]*\\(-\\) \\(.+!\\)$", nil, :notes_exclamation, :notes_exclamation)

    # - google:
    Styles.apply "^ *\\(-\\) \\(g\\)\\(o\\)\\(o\\)\\(g\\)\\(l\\)\\(e:\\) .*", nil, :ls_bullet,
      :notes_blue,
      :notes_red,
      :notes_yellow,
      :notes_blue,
      :notes_green,
      :notes_red
  end

  # Startup
  def self.init
    defun(:notes_mouse_launch, :interactive => "e") do |e|
      mouse_set_point(e)

      # If search in progress
      if TreeLs.search_going_or_interrupted and ! Line.blank?
        TreeLs.search_going_or_interrupted = false
        TreeLs.kill_siblings
      end

      LineLauncher.launch# :no_search => true
    end


    defun(:notes_mouse_double_click, :interactive => "e") do |e|
      if Line.matches(/\/$/)   # If dir, kill siblings first
        TreeLs.kill_siblings
      end

      LineLauncher.launch

    end

    defun(:notes_mouse_toggle, :interactive => "e") do |e|
      mouse_set_point(e)
      Notes.mouse_toggle
    end

    defun(:notes_mode, :interactive => "", :docstring => "Apply notes styles, etc") {# |point|
      el4r_lisp_eval "(setq font-lock-defaults '(nil t))"

      TreeLs.apply_styles
      Notes.apply_styles
      use_local_map elvar.notes_mode_map
    }
    el4r_lisp_eval %q<
      (progn
        (add-to-list 'auto-mode-alist '("\\\\.notes\\\\'" . notes-mode))
        (add-to-list 'auto-mode-alist '("\\\\.xik\\\\'" . notes-mode)))
      >

  #    el4r_lisp_eval %q[(add-to-list 'auto-mode-alist '("\\\\.notes\\\\'" . notes-mode))]
  #    el4r_lisp_eval %q[(add-to-list 'auto-mode-alist '("\\\\.\\\\'" . notes-mode))]
  end

  def self.mode
    notes_mode
  end

  def self.enter_label_bullet
    Line.to_left
    View.insert "- : "
    Move.backward 2
  end

  def self.bullet bullet_text="- "

    prefix = Keys.prefix

    # If non-blank line
    if ! Line.blank?   # Line
      if Line.matches(/^ *[|+-]/)   # If bullet already, just make new line after
        # Continue on below
      else   # If not bullet, make it a bullet
        # Get line minus indent, and indent one deeper than previous
        line = Line.value(1, :delete=>true).sub(/^ +/, '')

        if prefix.is_a? Fixnum   # If numeric prefix, indent by n
          View.insert((" " * prefix) + "- #{line}")
        else
          prev_indent = Line.value(0)[/^ */]
          View.insert "#{prev_indent}  - #{line}"
        end
        return
      end

    # Make extra line if none there yet

      Line.to_right
      View.insert "\n"
    end
    if prefix.is_a? Fixnum   # If numeric prefix, indent by n
      View.insert (" " * prefix) + bullet_text
    else   # Get bullet indent of previous line
      prev = Line.value(0)[/^( *)[+-]/, 1]
      prev = prev ? "  #{prev}#{bullet_text}" : bullet_text
      prev.sub!(/^  /, '') if Keys.prefix_u?   # Don't indent if U
      View.insert prev
    end

    #ControlLock.disable
  end

  def self.help_wiki_format
    View.to_buffer("*help wiki format*")
    View.clear;  Notes.mode

    View.unindent %q<
      | Headings
      - syntax: put "| " at beginning
        |  | Foo

      | Bullets
      - syntax: put "- " at beginning:
        |  - foo
      - looks like!
        - foo

      | Bullets with labels
      - syntax: put "- " at beginning and colon after label:
        |  - foo: bar
      - looks like!
        - foo: bar

      | Todo item
      - syntax: put "- " at beginning and "!" at end:
        | - foo!
      - looks like!
        - foo!
    >

    View.to_top

  end

  def self.mouse_toggle
    # If next line is indented more, kill children
    # If starts with plus or minus, and on plus or minus, launch
    if Line.matches(/^\s*[+-]/) and View.char =~ /[+-]/
      plus_or_minus = TreeLs.toggle_plus_and_minus
      if plus_or_minus == '+'   # If +, expand (launch

        if TreeLs.dir? or ! TreeLs.is_tree_ls_path   # If on a dir or code_tree
          LineLauncher.launch
        else   # If on a file in a FileTree
          TreeLs.enter_lines
        end

      else   # If -, kill under
        TreeLs.kill_under
        Move.to_line_text_beginning
      end
    end
  end

  # returns an instance of BlockNotes representing the block the point is currently in
  def self.get_block
    left, after_header, right = View.block_positions "^| "
    NotesBlock.new(left, after_header, right)
  end

  private
    class NotesBlock
      include ElMixin

      attr_accessor :left, :after_header, :right
      attr_accessor :header, :text

      def initialize(left, after_header, right)
        @left, @after_header, @right = left, after_header, right
        @header = buffer_substring left, after_header
        @text = buffer_substring after_header, right
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

      def delete_content
        delete_region left, right
      end

      # initialize an overlay for this notes block
      # it has a special hook that updates name to be header always
      # this way we can always find the overlay corresponding to header

      def show_text
        @header_overlay ||= Overlay.find_or_make(left, after_header - 1)
        @header_overlay.before_string = ''
        @header_overlay.after_string = ''

        @body_overlay ||= Overlay.find_or_make(after_header, right)
        @body_overlay.invisible = false
      end

      def hide_text
        @header_overlay ||= Overlay.find_or_make(left, after_header - 1)

        @header_overlay.before_string = ''
        @header_overlay.after_string = ' (more...)'

        @body_overlay ||= Overlay.find_or_make(after_header, right)
        @body_overlay.invisible = true
      end

      # cuts the block, and stores it in archive.file.notes
      # example: ruby.notes -> archive.ruby.notes
      def archive
        delete_content
        filename = 'archive.' + $el.file_name_nondirectory(buffer_file_name)
        timestamp = "--- archived on #{Time.now.strftime('%Y-%m-%d at %H:%M')} --- \n"
        append_to_file timestamp, nil, filename
        append_to_file content, nil, filename 
      end
    end

end
Notes.define_styles
#Notes.keys  # Define local keys
Notes.init

Notes.keys  # Define local keys
