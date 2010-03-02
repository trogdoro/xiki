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
    heading_regex = "^|\\( \\|$\\)"
    if up
      (Keys.prefix || 1).times do
        Line.to_left
        Search.backward heading_regex
      end
    else
      (Keys.prefix || 1).times do
        Line.next if Line[/^\|( |$)/]

        Search.forward heading_regex

        Line.to_left
      end
    end
  end

  def self.move_block up=false

    times = Keys.prefix_times

    orig = Location.new

    block = get_block
    block.blink
    block.delete_content

    if up
      times.times do
        re_search_backward "^| ", nil, 1
      end
      insert block.content
      search_backward_regexp "^| "
    else
      re_search_forward "^| "
      times.times do
        re_search_forward "^| ", nil, 1
      end
      beginning_of_line
      insert block.content
      search_backward_regexp "^| "
    end
    moved_block = get_block

    times == 1 ?
      moved_block.blink :
      orig.go

  end

  def self.insert_heading
    Line.start

    times = Keys.prefix_u? ? 1 : (Keys.prefix || 1)
    times.times { insert "|" }
    View.insert " "

    open_line(4) if Keys.prefix_u?   # If U create blank lines.


    # If U, get letter from next bullet
    if Keys.prefix_u?
      orig = Location.new
      Search.forward "^| "   # Find next heading
      char = Line.value[/^\| (\w) /, 1]   # Pull char off and insert, if there is one
      orig.go
      View.insert "#{char} " if char
    end

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

  def self.copy_block
    block = get_block
    block.blink
    Clipboard.set("0", Keys.prefix_u ? block.text : block.content)
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

    Keys.custom_archive(:notes_mode_map) { Notes.archive }
    Keys.custom_back(:notes_mode_map) { Notes.move_block true }   # Move block up to before next block
    Keys.custom_clipboard(:notes_mode_map) { Notes.copy_block }   # block -> clipboard
    Keys.custom_delete(:notes_mode_map) { Notes.cut_block(true) }   # block -> clear
    Keys.custom_expand(:notes_mode_map) { Notes.expand_block }   # Show just block
    Keys.custom_forward(:notes_mode_map) { Notes.move_block }   # Move block down to after next block
    Keys.custom_heading(:notes_mode_map) { Notes.insert_heading }   # Insert ||... etc. heading
    Keys.custom_item(:notes_mode_map) { Agenda.quick_add_line }
    # j
    Keys.custom_kill(:notes_mode_map) { Notes.cut_block }   # block -> cut
    # l
    Keys.custom_mask(:notes_mode_map) { Notes.hide_text }   # block -> hide
    Keys.custom_next(:notes_mode_map) { Notes.to_block }   # Go to block after next block
    Keys.custom_open(:notes_mode_map) { Notes.show_text }   # block -> reveal
    Keys.custom_previous(:notes_mode_map) { Notes.to_block(true) }   # Go to block before next block
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

    define_key :notes_mode_map, kbd("C-\\") do

      widen; Hide.show
      Hide.hide_unless /^\| /
      recenter -2
      Hide.search
    end

    define_key(:notes_mode_map, kbd("M-<mouse-1>"), :notes_mouse_meta_click)
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
    h1_size = "+2"

    # Colors of "| ..." headings
    if Styles.inverse   # If black bg
      @@h1_styles = {
        :notes_h1  => "666699",
        :notes_h1r => "661111",   # | r This will be red
        :notes_h1o => "884411",   # | o This will be orange
        :notes_h1y => "887711",
        :notes_h1e => "336633",
        :notes_h1g => "336633",
        :notes_h1b => "666699",
        :notes_h1p => "663366",
        :notes_h1m => "662222",
        :notes_h1x => "333333",
        :notes_h1t => "005555",
        }
    else
      # Colors of headings
      @@h1_styles = {
        :notes_h1  => "9999bb",
        :notes_h1r => "bb6666",   # | r This will be red
        :notes_h1o => "bb8833",   # | o This will be orange
        :notes_h1y => "bbbb33",
        :notes_h1e => "336633",
        :notes_h1g => "77bb66",
        :notes_h1b => "9999bb",
        :notes_h1p => "bb88bb",
        :notes_h1m => "994444",
        :notes_h1x => "999999",
        :notes_h1t => "005555",
        }
    end


    @@h1_styles.each do |k, v|
      lighter = v.gsub(/../) {|c| (c.to_i(16) + "44".to_i(16)).to_s(16)}
      Styles.define k,                  :face => 'arial', :size => h1_size, :fg => 'ffffff', :bg => v, :bold =>  true
      Styles.define "#{k}_pipe".to_sym, :face => 'arial', :size => h1_size, :fg => lighter,   :bg => v, :bold =>  true
    end

    # ||...
    Styles.define :notes_h2,
      :face => 'arial', :size => "0",
      :fg => '8888bb', :bg => "e0e0f2",
      :bold =>  true
    Styles.define :notes_h2_pipe,
      :face => 'arial', :size => "0",
      :fg => 'bbbbdd', :bg => "e0e0f2",
      :bold =>  true

    # |||...
    Styles.define :notes_h3,
      :face => 'arial', :size => "-1",
      :fg => '9999ee',#, :bg => "9999cc",
      :bold =>  true
    Styles.define :notes_h3_pipe,
      :face => 'arial', :size => "-1",
      :fg => '222244'

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
      :fg => "ee7700", :bold => true

    Styles.define :notes_bullet_parens,
      :face => 'arial', :size => "-2",
      :fg => "ee7700", :bold => true

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

      Styles.define :notes_h2, :fg => '555588', :bg => "222233"
      Styles.define :notes_h2_pipe, :fg => '3b3b5e', :bg => "222233"

      Styles.define :notes_exclamation,  # Green bold text
        :face => 'arial black', :size => "0",
        :fg => "66bb22", :bold => true
      #         :fg => "449911", :bold => true

      #       Styles.define :notes_h1,
      #         :fg => 'ffffff', :bg => "3b3b5e"
      #       Styles.define :notes_h1_pipe, :fg => '9191b3', :bg => "3b3b5e"
    else

      Styles.define :notes_exclamation,  # Green bold text
        :face => 'arial black', :size => "0",
        :fg => "77cc44", :bold => true
      #         :fg => "66bb33", :bold => true

      Styles.define :notes_h1_pipe, :fg => 'bbbbee', :bg => "9999bb"
    end
  end

  def self.apply_styles
    # Don't format quotes (it overrides the following formatting)
    Styles.clear

    # |... lines (headings)
    #     Styles.apply("^\\(|\\)\\( \n\\|.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
    Styles.apply("^\\(|\\)\\(.*\n\\)", nil, :notes_h1_pipe, :notes_h1)
    Styles.apply("^\\(| .+?: \\)\\(.+\n\\)", nil, :notes_h1_pipe, :notes_h1)

    @@h1_styles.each do |k, v|
      l = k.to_s[/_..(.)$/, 1]
      next unless l
      Styles.apply("^\\(| #{l}\\)\\(\n\\| .*\n\\)", nil, "#{k}_pipe".to_sym, k)
      Styles.apply("^\\(| #{l} .+: \\)\\(.*\n\\)", nil, "#{k}_pipe".to_sym, k)
    end

    # ||... lines
    Styles.apply("^\\(||\\)\\(.*\n\\)", nil, :notes_h2_pipe, :notes_h2)
    Styles.apply("^\\(|| .+?: \\)\\(.+\n\\)", nil, :notes_h2_pipe, :notes_h2)

    # |||... lines
    Styles.apply("^\\(|||\\)\\(.*\n\\)", nil, :notes_h3_pipe, :notes_h3)

    # ||||... lines
    Styles.apply("^\\(||||\\)\\(.*\n\\)", nil, :notes_h4_pipe, :notes_h4)

    #     # ~emphasis~ strings
    #     Styles.apply("\\(~\\)\\(.+?\\)\\(~\\)", :notes_label)

    # - bullets with labels
    Styles.apply("^[ \t]*\\([+-]\\) \\([!#-~ ]+?:\\) ", nil, :ls_bullet, :notes_label)
    Styles.apply("^[ \t]*\\([+-]\\) \\([!#-~ ]+?:\\)$", nil, :ls_bullet, :notes_label)

    #Styles.apply("^[ \t]*\\(\\+\\)\\( \\)", nil, :ls_bullet, :variable)

    Styles.apply("^[ \t]*\\(x\\)\\( \\)\\(.+\\)", nil, :notes_label, :variable, :strike)

    Styles.apply("^\\([ \t]*\\)\\([+-]\\) \\(.+?:\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :notes_label, :ls_quote)

    Styles.apply("^ +\\(!.*\n\\)", nil, :ls_quote)   # ^!... for commands

    # exclamation! / todo
    Styles.apply("^[ \t]*\\([+-]\\) \\(.+!\\)$", nil, :ls_bullet, :notes_exclamation)
    Styles.apply("^ +\\(!\\+.*\n\\)", nil, :diff_green)   # Whole lines
    Styles.apply("^ +\\(!-.*\n\\)", nil, :diff_red)

    Styles.apply("\\(\(-\\)\\(.+?\\)\\(-\)\\)", nil, :diff_small, :diff_red, :diff_small)
    Styles.apply("\\(\(\\+\\)\\(.+?\\)\\(\\+\)\\)", nil, :diff_small, :diff_green, :diff_small)

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
    defun(:notes_mouse_meta_click, :interactive => "e") do |e|
      mouse_set_point(e)

      View.insert "hey"
    end

    defun(:notes_mouse_double_click, :interactive => "e") do |e|
      #       if Line.matches(/\/$/)   # If dir, toggle
      LineLauncher.launch(:blink=>true)
    end

    defun(:notes_mouse_toggle, :interactive => "e") do |e|
      #LineLauncher.launch_or_hide(:blink=>true)
      mouse_set_point(e)
      Notes.mouse_toggle
    end

    defun(:notes_mode, :interactive => "", :docstring => "Apply notes styles, etc") {# |point|
      el4r_lisp_eval "(setq font-lock-defaults '(nil t))"

      FileTree.apply_styles
      Notes.apply_styles
      use_local_map elvar.notes_mode_map
    }
    el4r_lisp_eval %q<
      (progn
        (add-to-list 'auto-mode-alist '("\\\\.notes\\\\'" . notes-mode))
        (add-to-list 'auto-mode-alist '("\\\\.xik\\\\'" . notes-mode))
        (add-to-list 'auto-mode-alist '("\\\\.wik\\\\'" . notes-mode)))
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
    prefix = Keys.prefix :clear=>true

    Move.to_end if prefix == :u   # If U prefix, go to the end

    if ! Line.blank?   # If non-blank line

      if Line.matches(/^\|/) || Line.matches(/^ *[+-]/) || bullet_text != "- "    # If bullet already, or not adding bullets

        if Line.point == Line.left || Line.point == Line.right   # If cursor at beginning of line
          Line.to_right
          # Will move down one line
        else   # Else, leave rest of line to be text of bullet
          Deletes.delete_whitespace
        end
        View.insert "\n"
      else   # If not bullet, make it a bullet
        # Get line minus indent, and indent one deeper than previous
        line = Line.value(1, :delete=>true).sub(/^ +/, '')

        if prefix.is_a? Fixnum   # If numeric prefix, indent by n
          View.insert((" " * prefix) + "- #{line}")
        else
          prev_indent = Line.value(0)[/^ */]
          prev_indent << "  " unless prev_indent == ""
          View.insert "#{prev_indent}- #{line}"
        end
        Move.to_line_text_beginning
        return
      end

    end
    if prefix.is_a? Fixnum   # If numeric prefix, indent by n
      View.insert((" " * prefix) + bullet_text)
    else   # Get bullet indent of previous line
      prev = Line.value(0)[/^( *)/]
      # Indent further, unless it we're doing bullets and not following bullet
      prev << "  " unless bullet_text == "- " && ! Line.value(0)[/^ *[+-]/]
      prev = "#{prev}#{bullet_text}"
      View.insert prev

      if prefix == :uu
        View.insert "(): "
        Move.backward 3
      end
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
    #LineLauncher.launch_or_hide(:blink=>true)

    # If next line is indented more, kill children
    # If starts with plus or minus, and on plus or minus, launch
    if Line.matches(/^\s*[+-]/) and View.char =~ /[+-]/
      plus_or_minus = FileTree.toggle_plus_and_minus
      if ! CodeTree.children?
        #plus_or_minus == '+'   # If +, expand (launch

        if FileTree.dir? or ! FileTree.handles?   # If on a dir or code_tree
          LineLauncher.launch
        else   # If on a file in a FileTree
          FileTree.enter_lines
        end

      else   # If -, kill under
        FileTree.kill_under
        Move.to_line_text_beginning
      end
    end
  end

  # Returns an instance of BlockNotes representing the block the point is currently in
  def self.get_block regex="^|\\( \\|$\\)"
    left, after_header, right = View.block_positions regex
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

  def self.enter_do_bullet
    line = Line.value
    indent, first_char = line.match(/^( *)(.)/)[1..2]

    if first_char == "|"   # If quote, insert before
      Move.to_axis
      $el.open_line(1)
      View.insert "#{indent}- do!"
      return
    end

    # If bullet, insert under
    Notes.bullet
    View.insert "do!"

  end

end
Notes.define_styles
#Notes.keys  # Define local keys
Notes.init

Notes.keys  # Define local keys
