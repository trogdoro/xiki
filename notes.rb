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
    left, after_header, right = View.block_positions "^| "
    Effects.blink :left => after_header, :right => right
    txt = buffer_substring left, right
    delete_region left, right
    if up
      (Keys.prefix || 1).times do
        re_search_backward "^| ", nil, 1
      end
      insert txt
      search_backward_regexp "^| "
    else
      re_search_forward "^| "
      (Keys.prefix || 1).times do
        re_search_forward "^| ", nil, 1
      end
      beginning_of_line
      insert txt
      search_backward_regexp "^| "
    end
    left, after_header, right = View.block_positions "^| "
    Effects.blink :left => after_header, :right => right
  end

  def self.insert_heading
    Line.start
    (Keys.prefix || 1).times do
      insert "|"
    end
    insert " "
    #open_line 1

    # Exit control lock mode if on
    ControlLock.disable
#     # Read in one key, then upcase it
#     a = Keys.input(:one_char => true)
#     insert a.upcase
  end

  def self.cut_block no_clipboard=false
    left, after_header, right = View.block_positions "^| "
    Effects.blink :left => after_header, :right => right
    unless no_clipboard
      Clipboard.set("0", buffer_substring(left, right))
    end
    delete_region left, right
  end

  def self.move_block_to_top no_clipboard=false
    left, after_header, right = View.block_positions "^| "
    Effects.blink :left => after_header, :right => right
    txt = buffer_substring(left, right)
    delete_region left, right
    beginning_of_buffer
    insert txt
    goto_line 2
    left, after_header, right = View.block_positions "^| "
    Effects.blink :left => left, :right => right

  end

  def self.keys
    # Get reference to map if already there (don't mess with existing buffers)
    elvar.notes_mode_map = make_sparse_keymap unless boundp :notes_mode_map
    Keys.CT(:notes_mode_map) { Notes.move_block_to_top }

    Keys.CD(:notes_mode_map) { Notes.cut_block true }
    Keys.CX(:notes_mode_map) { Notes.cut_block }

    Keys.CH(:notes_mode_map) { Notes.insert_heading }  # Insert ||... etc. heading
    Keys.CN(:notes_mode_map) { Notes.to_block }  # Go to block after next block
    Keys.CP(:notes_mode_map) { Notes.to_block true }  # Go to block before next block
    Keys.CE(:notes_mode_map) { Notes.expand_block }  # Show just block

    Keys.CF(:notes_mode_map) { Notes.move_block }  # Move block down to after next block
    Keys.CB(:notes_mode_map) { Notes.move_block true }  # Move block up to before next block

    define_key :notes_mode_map, kbd("C-\\") do
      Hide.show
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
    Styles.apply("^\\(| \\)\\(.+\n\\)", nil, :notes_h1_pipe, :notes_h1)
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
    Styles.apply("^\\(|| \\)\\(.+\n\\)", nil, :notes_h2_pipe, :notes_h2)
    Styles.apply("^\\(|| .+?: \\)\\(.+\n\\)", nil, :notes_h2_pipe, :notes_h2)

    # |||... lines
    Styles.apply("^\\(||| \\)\\(.+\n\\)", nil, :notes_h3_pipe, :notes_h3)

    # ||||... lines
    Styles.apply("^\\(|||| ?\\)\\(.+\n\\)", nil, :notes_h4_pipe, :notes_h4)

    # ~emphasis~ strings
    Styles.apply("\\(~\\)\\(.+?\\)\\(~\\)", :notes_label)

    # - bullets with labels
    #Styles.apply("^[ \t]*\\([+-]\\)\\( \\)", nil, :ls_bullet, :variable)
    Styles.apply("^[ \t]*\\([+-]\\) \\(.+?:\\) ", nil, :ls_bullet, :notes_label)
    Styles.apply("^[ \t]*\\([+-]\\) \\([^:\n]+?:\\)$", nil, :ls_bullet, :notes_label)

    #Styles.apply("^[ \t]*\\(\\+\\)\\( \\)", nil, :ls_bullet, :variable)

    Styles.apply("^[ \t]*\\(x\\)\\( \\)\\(.+\\)", nil, :ls_bullet, :variable, :strike)

    Styles.apply("^\\([ \t]*\\)\\([+-]\\) \\(.+?:\\) +\\(|.*\n\\)", nil, :default, :ls_bullet, :ls_bullet, :ls_quote)

    # - item exclamation! / todo
    Styles.apply("^[ \t]*\\(-\\) \\(.+!\\)$", nil, :notes_exclamation, :notes_exclamation)

    # - (r): code
    Styles.apply("^ *- \\(.*\\)\\( ([a-z]+):\\) ", nil, :notes_label_link, :notes_label_parens)

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

  def self.bullet bullet_text="- "
    # Make extra line if none there yet
    if Line.matches(/./)
      Line.to_right
      View.insert "\n"
    end

    prefix = Keys.prefix

    if prefix.is_a? Fixnum   # If numeric prefix, indent by n
      insert (" " * prefix) + bullet_text
    else   # Get bullet indent of previous line
      prev = Line.value(0)[/^( *)#{bullet_text}/, 1]
      prev = prev ? "  #{prev}#{bullet_text}" : bullet_text
      prev.sub!(/^  /, '') if Keys.prefix_u   # Don't indent if U
      insert prev
    end

    ControlLock.disable
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
        if Line.matches(/\/$/)   # If on a dir
          LineLauncher.launch
        else   # If on a file, enter outline
          TreeLs.enter_lines
        end
      else   # If -, kill under
        TreeLs.kill_under
        Move.to_line_text_beginning
      end
    end
  end

end
Notes.define_styles
#Notes.keys  # Define local keys
Notes.init

Notes.keys  # Define local keys
