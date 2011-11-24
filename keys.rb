require 'pause_means_space'
require 'line'
require 'text_util'

# Methods for defining keyboard shortcuts
class Keys

  @@key_queue =[]  # For defining menus (must be done in reverse)

  CODE_SAMPLES = %q[
    # Map C-c C-a
    Keys.CA { View.insert "foo" }

    # Map M-x (in shell mode)
    Keys._X(:shell_mode_map) { View.insert "foooo" }

    # Get user input
    - A String
      - puts Keys.input(:prompt => "Name: ")
    - Just one char
      - puts Keys.input(:one_char => true)
  ]

  def self.menu
    %`
    - .history/
    - docs/
      > Summary
      | Xiki has keyboard shortcuts predefined for doing all kinds of things.
      | Each keyboard shortcut has a mnemonic. Check out the "Keys" menu bar
      | menu for a quick look at them.
      |
      | And Xiki lets you define your own keyboard shortcuts.  This line makes
      | Control-e Control-n insert "Steve".
      |
      |   Keys.EN { View << "Steve" }
      |
      | For more about defining your own keyboard shortcuts see:
      - @keys/api/
      |
      > Xiki's "type the acronym" approach
      | Each xiki keyboard shortcut has a mnemonic that helps you
      | simultaneously remember what it does and how to type it.
      |
      | For example, given this mnemonic:
      |
      |   layout_create
      |
      | you type:
      |
      |   Control-l Control-c  (l for "layout" and c for "create")
      |
      > Reasons for this appoarch
      - More possible shortcuts/
        | The approach of having single character key shortcuts (e.g. Control-a)
        | works nicely for apps that have a small number of shortcuts. But it
        | becomes less elegant when more shortcuts are used (Ctrl-a, Alt-a,
        | Ctrl-Shift-a).
        |
        | The "type the acronym" approach with just the Control
        | key allows for very a large number of key shortcuts that are less
        | prone to get confused with one another.
        |
      - Less to remember/
        | A mnemonic clues you into what the keyboard shortcut does and how to type it,
        | so it's all you need to remember. There's no need to separately remember a keyboard shortcut and what it does (a
        | challenging part of most keyboard shortcut schemes having a large number of
        | shortcuts, which xiki attempts to avoid).
        |
        | Doesn't sound like standard emacs shortcuts?  Here's an explanation about
        | how xiki deals with existing emacs shortcuts.
        |
      - emacs_shortcuts/
        | TODO add stuff about how C-a turns into C-a C-a, etc.
        | Mention how this lets a large number of key shortcuts without interfering
        | with emacs shortcuts.
        | But an admitted downside is it affects 6 existing emacs shortcuts
        | and makes you type them twice.
        | In practice the annoyance caused by this isn't as bad as it initially may seem
        | Consider using to_axis instead of C-a C-a and to_end instead of C-e C-e.
      |
      > Six categories
      | As you can see by looking at the "Keys" menu in the menu bar, there are
      | six main categories of key shortcuts.
      |
      - Descriptions of each category/
        |   to: jumping to specific points
        |   open: open things
        |   layout: subdivisions of windows
        |   as: remembering things
        |   enter: inserting things
        |   do: executing things
      |
      > Examples
      | Here are some of the most commonly used shortcuts in each category.
      | (Double-click a category to see them.)
      |
      - to/
        | to_highest: Jump to top of file
        | to_lowest: Jump to bottom of file
        | to_axis: Jump to beginning of line
        | to_end: Jump to end of line
      - open/
        | open_bookmark: view a bookmark
        | open_tree: view a tree of a directory
        | open_current: shows currently open files
        | open_edited: shows recently edited files
        | open_open_history: shows recently viewed files
        | open_menu: opens view that lets you type a menu (type "-" to see all)
      - layout/
        | layout_create: Create a new view
        | layout_hide: Hide this view
        | layout_next: Go to next view
        | layout_previous: Go to previous view
        | layout_kill: Close the current file
      - as/
        | as_clipboard: Copy (after doing Control-space on the other side)
        | as_kill: Cut (after doing Control-space on the other side)
        | as_bookmark: remember this file as a bookmark
      - enter/
        | enter_clipboard: Paste
      - do/
        | do_tree: view an expanded tree of a directory
      |
      - miscellaneous/
        | Control-tab: cycles through files
      |
      | For all keyboard shortcuts, see where they're key_bindings.rb, where they're
      | defined:
      - @$xiki/key_bindings.rb
      |
      |
      > Keyboard shortcuts while searching
      | The seventh category, "search" has special behavior.  See:
      - @search/docs/
      |
    - .api/
    `
  end

  def self.api
    '
    > Summary
    | Ways to use the Keys class, to define keyboard shortcuts etc.  Remember
    | that with xiki shortcuts you hold down the control key and "type the
    | acronym" (the first letter in each word) while holding down the control
    | key.
    |
    > Define keyboard shortcuts
    | Defines the key Control-e Control-y
    |
    |   Keys.enter_name { View << "Steve" }
    |
    | Defines the key Control-e Control-y (with no mnemonic)
    |
    |   Keys.EN { View << "Steve again" }
    |
    > Where to put them
    | You can put keyboard shortcuts into any file that gets required by xiki.
    | For example, you could create a file like this:
    |
    - @~/xiki/lib/
      - keys.rb
        | # My shortcuts
        | Keys.enter_name { View << "Steve" }
        | Keys.enter_yay { View << "Yay" }
    |
    | Then you could require this file when xiki loads by adding this line:
    |
    - @~/.el4r/
      - init.rb
        | require "~/xiki/lib/keys"
    '
  end


  # Handles Keys.to_foo etc.

  def self.method_missing(meth, *args, &block)

    # Accept it if block but no args
    meth = meth.to_s

    meth_orig = meth.dup


    # Delegate to super unless arg is single symbol
    unless args.size == 0 or (args.size == 1 and args[0].is_a? Symbol)
      return super(meth, *args, &block)
    end

    meth_title = meth.gsub('_', ' ').gsub(/\b\w/) {|s| s.upcase}
    menu, item = meth_title.match(/(.+?) (.+)/)[1..2] if meth_title =~ /. ./

    # If 1st word is 'isearch', use it as map
    if meth =~ /^search_/
      @@key_queue << ["Search", item]
      meth.sub! /^search_/, ''
      meth = self.words_to_letters meth
      args = [:isearch_mode_map]
    elsif meth =~ /[A-Z]/   # If capital letters
      # Don't convert
    elsif meth =~ /_/   # Add menu item, if more than one word

      if args.size == 0   # If global keymap
        # Make lisp function
        $el.defun(meth.to_sym, :interactive=>true) do
          block.call
        end
        @@key_queue << [menu, item]
      end

      # Change 'to_foo' to 'TF' etc
      meth = self.words_to_letters meth
    end

    # Translate to 'C-t C-f' etc
    keys_raw = self.translate_keys meth

    # Translate to actual control keys
    keys = $el.kbd keys_raw
    # Default to global keymap
    map = :global_map

    # Use keymap if symbol passed as 1st arg
    map = args.shift if args[0] and args[0].class == Symbol

    # If they just passed a string, use it as code
    if args and (args.size == 1) and !block
      self.map_to_eval keys_raw, args[0]
      return
    end

    # Define key
    begin
      $el.define_key map, keys, &block
      "- key was defined: #{keys_raw}"
    rescue Exception => e

      return if map != :global_map || meth !~ /([A-Z])([A-Z]?)./   # Only global map and 2 control keys
      message = e.message
      prefix = message[/"Key sequence .+? starts with non-prefix key (.+?)"/, 1]
      return if prefix.nil?

      prefix = $el.kbd(prefix)

      begin   # If it appears to be a prefix key (already defined)

        $el.global_unset_key(prefix)
        $el.define_key map, keys, &block

      rescue Exception => e
        Ol << "e (inner): #{e.inspect}"
      end

    end
  end

  def self.map_to_eval keys_raw, code
    $el.el4r_lisp_eval"
      (global-set-key (kbd \"#{keys_raw}\")  (lambda () (interactive)
        (el4r-ruby-eval \"#{code}\" )
      ))
      "
  end

  def self.translate_keys txt
    l = txt.scan(/_?\w/)
    l.collect! { |b|
      case b
      when /^_([A-Z])/
        "M-" + $1.downcase
      when /^([a-z])$/
        $1
      else
        "C-" + b.downcase
      end
    }
    l.join " "
  end

  def self.set *args, &block
    # Keys is always first arg
    keys = args.shift
    if args.size > 0   # If 2nd arg, use it
      self.map_to_eval keys, args[0]
    else   # Otherwise, use block
      $el.define_key :global_map, $el.kbd(keys), &block
    end
  end

  # Gets input from user.
  # Sample usages:
  #   - Terminated by enter:  Keys.input
  #   - Just one char:  Keys.input(:one_char => true)
  #   - One char if control:  Keys.input(:control => true)
  #   - Terminated by pause:  Keys.input(:timed => true)
  #   - Terminated by pause:  Keys.input(:optional => true)
  #     - A pause at the beginning will result in no input (nil)
  def self.input options={}

    return self.input_with_choices(options) if options[:choices]

    Cursor.remember :before_input
    Cursor.green
    Cursor.hollow

    prompt = options[:prompt] || "Input: "

    # Not completely implemented
    if options[:control]

      prompt = "todo - implement this: "

      elvar.inhibit_quit = true
      c = read_char(prompt)
      elvar.inhibit_quit = nil
      if c == 7
        Cursor.restore :before_input
        keyboard_quit
      end

      Cursor.restore :before_input
      return c
    end
    if options[:one_char]
      char = $el.char_to_string(
        self.remove_control($el.read_char(prompt))).to_s
      Cursor.restore :before_input
      return char
    end

    # If simple un-timed input, just get string and return it
    unless options[:timed] || options[:optional]
      Cursor.restore :before_input
      c = $el.read_string(prompt, options[:initial_input])
      return c
    end

    keys = ""

    $el.elvar.inhibit_quit = true
    c = nil

    # If not optional, wait for input initially
    unless options[:optional]
      c = $el.read_char(prompt)
      keys = self.to_letter(c)
    end

    if c == 7
      Cursor.restore :before_input
      $el.elvar.inhibit_quit = nil
      $el.keyboard_quit
    end

    while(c = $el.read_char("#{prompt}#{keys}", nil, 0.35))
      keys += self.to_letter(c)
      if c == 7
        Cursor.restore :before_input
        $el.elvar.inhibit_quit = nil
        $el.keyboard_quit
      end
    end
    $el.elvar.inhibit_quit = nil
    Cursor.restore :before_input

    $el.message ""

    # If nothing, return nil
    keys == "" ? nil : keys
  end

  # TODO: finish - look at spec
  def self.input_with_choices options
    prompt = options[:prompt] ? "#{options[:prompt]} " : ""
    prompt << options[:choices].map{|i|
      "[#{i.first[/./]}]#{i.first[/.(.+)/,1]}"}.
      join(', ')
    c = Keys.input :one_char=>true, :prompt=>prompt
    options[:choices].find{|i| i.first =~ /^#{c}/}[1]
  end

  def self.to_letter ch
    return nil if ch.nil?
    if ch < 27
      ch += 96

    # Now it includes C-.  Do we want that?!
    elsif 67108910 <= ch and ch <= 67108921
      #     elsif 67108912 <= ch and ch <= 67108921
      ch -= 67108864
    end
    ch.chr
  end

  # Converts any control keys in input to normal keys.
  # Example: "\C-x" => "x"
  def self.remove_control ch
    ch += 96 if ch < 27
    ch
  end

  def self.read_char_maybe
    loc = $el.read_char("Optionally type a char:", nil, 0.35)
    $el.message ""
    return if loc.nil?

    # Convert control chars to the corresponding letters
    loc += 96 if(1 <= loc and loc <= 26)
    loc = self.remove_control loc
    loc
  end

  #   def self.prefix
  #     elvar.current_prefix_arg || 1
  #   end

  def self.insert_code
    keys = $el.read_key_sequence("Type some keys, to insert the corresponding code: ")

    # If C-n or C-p, pretend like they were mapped to xiki functions

    if keys == "\cn"
      return View << 'Line.next'
    elsif keys == "\cp"
      return View << 'Line.previous'
    end

    proc = self.proc_from_key keys

    # If lisp, enter lisp?
    if proc.nil?
      keys = $el.key_binding(keys)
      if keys
        return View.insert($el.prin1_to_string(keys))
      else
        $el.beep
        return View.message("Key is unmapped")
      end
    end

    code = Code.to_ruby(proc)
    code.gsub! 'proc { ', ''
    code.gsub! ' }', ''

    code.gsub! '(:blink => (true))', ''

    View << code
  end

  def self.jump_to_code
    keys = $el.read_key_sequence("Type some keys, to jump to the corresponding code: ")
    proc = self.proc_from_key keys
    if proc.nil?
      $el.beep
      return View.message("Key wasn't mapped")
    end

    file, line = Code.location_from_proc proc
    file = "#{XIKI_ROOT}/#{file}" unless file =~ /^\//
    Location.go(file)
    View.to_line line.to_i
    Effects.blink(:what=>:line)
  end

  def self.proc_from_key keys
    code = $el.prin1_to_string($el.key_binding(keys))
    # If it is a call to elisp
    id = code[/el4r-ruby-call-proc-by-id.+?([_0-9]+)/, 1]
    return nil if id.nil?

    ObjectSpace._id2ref(id.to_i)
  end

  def self.timed_insert options={}
    prefix = Keys.prefix
    # If prefix of 0, insert in a way that works with macros
    case prefix
    when nil   # Do nothing
    when :u   # Do pause for space
      PauseMeansSpace.go
      return
    when 0
      View.insert Keys.input(:prompt => "Insert text to insert: ")
      return
    else   # If other prefix, insert single char n times
      c = View.read_char("Insert single char to insert #{prefix} times: ").chr
      prefix.times do
        View.insert c
      end
      return
    end

    Cursor.remember :before_q
    Cursor.green

    # Get first char and insert
    c = $el.read_char("insert text (pause to exit): ").chr
    inserted = "#{c}"
    #     c = c.upcase if prefix == :u

    View.insert c
    #     o = $el.make_overlay $el.point, $el.point - 1
    #     $el.overlay_put o, :face, :control_lock_found
    # While no pause, insert more chars
    while(c = $el.read_char("insert text (pause to exit): ", nil, 0.36))
      #       $el.delete_overlay o
      inserted += c.chr
      View.insert c.chr
      #       o = $el.make_overlay $el.point, $el.point - inserted.size
      #       $el.overlay_put o, :face, :control_lock_found
    end
    #     $el.delete_overlay o
    $el.elvar.qinserted = inserted
    $el.message "input ended"

    Cursor.restore :before_q

    # Store in hash by first letter for use by enter_yank

    Clipboard.save_by_first_letter inserted   # Store for retrieval with enter_yank

  end

  def self.as name
    Clipboard.copy("#{name}")
    Bookmarks.save("$#{name}")
    Bookmarks.save("$_#{name}")
    View.save("#{name}")
  end

  def self.insert_from_q
    ($el.elvar.current_prefix_arg || 1).times do
      View.insert($el.elvar.qinserted)
    end
  end

  def self.prefix_or_0 options={}
    pre = Keys.prefix
    pre.is_a?(Fixnum) ? pre : 0
  end

  # Returns nil, numeric prefix if C-1 etc, or :u if C-u
  def self.prefix options={}
    pre = $el.elvar.current_prefix_arg
    return nil unless pre

    # Clear prefix if :clear
    $el.elvar.current_prefix_arg = nil if options[:clear]
    str = pre.to_s

    if str =~ /^\(/
      return :uu if str == "(16)"
      return :uuu if str == "(64)"
      return :u
    end

    return :- if "#{pre}" == "-"
    return pre
  end

  def self.prefix= to
    $el.elvar.current_prefix_arg = to
  end

  # Whether C-u was held down before this
  def self.prefix_u?
    self.prefix == :u
  end

  # Whether C-u was held down before this
  # Deprecated
  def self.prefix_u options={}
    result = self.prefix == :u
    self.clear_prefix if options[:clear]
    result
  end

  def self.prefix_n options={}
    pre = self.prefix(options)
    pre.is_a?(Fixnum) ? pre : nil
  end

  def self.prefix_uu
    self.prefix == :uu
  end

  def self.clear_prefix
    $el.elvar.current_prefix_arg = nil
  end

  def self.bookmark_as_path options={}
    bm = Keys.input(:timed=>true, :prompt=>options[:prompt]||"Enter a bookmark: ")
    if bm == " "   # If space, return special token
      return :space
    elsif bm == "/"   # If slash, return special token
      return :slash
    elsif bm == ","   # If slash, return special token
      return :comma
    elsif bm =~ /^\.+$/   # If .+ do tree in current dir
      dir = View.dir :force_slash
      (bm.size - 1).times do
        dir.sub! /\/$/, ''   # Remove / on end if there
        dir.sub! /[^\/]+$/, ''   # Remove dir
      end
      dir.sub! /^$/, '/'   # If nothing left, use root (/)
      return dir
    end

    dir = Bookmarks.expand bm, :just_bookmark=>true
    if dir.nil?   # If no dir, return nil
      View.message "Bookmark '#{bm}' doesn't exist."
      return nil
    end

    unless options[:include_file]
      dir = Bookmarks.dir_only dir
      dir << "/" unless dir =~ /\/$/
    end
    dir

  end

  def self.prefix_times
    prefix = self.prefix
    case prefix
    when nil, :u, :uu, :uuu
      1
    else
      prefix
    end
  end

  def self.add_menu_items
    @@key_queue.reverse.each do |i|
      Menu.add_item [Menu::ROOT_MENU, i[0]], i[1], "#{i[0].downcase}-#{i[1].downcase.gsub(' ', '-')}"
    end
    @@key_queue = []
  end

  def self.char

    $el.elvar.inhibit_quit = true
    ch_initial = $el.read_event.to_s
    $el.elvar.inhibit_quit = nil

    if ch_initial =~ /^\d+$/   # If a number, assign it to raw
      ch_raw = ch_initial.to_i
      if 134217825 <= ch_raw and ch_raw <= 134217850  # If meta (out of elisp range)
        return ["meta_#{(ch_raw - 134217728).chr}".to_sym, nil]
      end

      # If char is over the elisp max, try to interpret it as Meta
      ch = $el.char_to_string(ch_raw)
      # Special check for C-. and other sequences
      ch = :control_period if ch_raw == 67108910
      ch = :control_slash if ch_raw == 67108911
      return [ch, ch_raw]

    elsif ['left', 'right', 'up', 'down', ].member?(ch_initial)
      return [ch_initial.to_sym, 0]   # Arbitrary indicator for arrow keys

    elsif ch_initial == "C-return"
      return [:control_return, 13]

    elsif ch_initial == "return"
      return [:return, 13]

    elsif ch_initial == "backspace"
      return [:backspace, 127]

    elsif ch_initial == "tab"
      return ["\t", 9]

    else   # Probably a mouse event
      return [nil, nil]
    end

  end

  def self.words_to_letters txt
    TextUtil.camel_case(txt).gsub(/[a-z]/, '')
  end

  def self.last
    $el.el4r_lisp_eval("(elt (recent-keys) (- (length (recent-keys)) 1))").to_s
  end

  def self.before_last
    $el.el4r_lisp_eval("(elt (recent-keys) (- (length (recent-keys)) 2))").to_s
  end

  def self.history
    $el.view_lossage
    View.success "- Showed recently-typed keys in other view!", :times=>4
  end
end
