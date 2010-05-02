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

  # Handles Keys.to_foo etc.

  def self.method_missing(meth, *args, &block)
    # Accept it if block but no args
    meth = meth.to_s

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
      if map == :global_map && meth =~ /([A-Z])([A-Z]?)./
        prefix = $2.to_s.empty? ? $el.kbd("C-#{$1.downcase}") : $el.kbd("C-#{$1.downcase} C-#{$2.downcase}")

        begin   # If it appears to be a prefix key (already defined)
          $el.global_unset_key(prefix)
          #$el.define map, prefix, nil
          $el.define_key map, keys, &block
          "- key #{keys_raw} was defined"
        rescue Exception => e
        end
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
    unless options[:timed] or options[:optional]
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
    elsif 67108912 <= ch and ch <= 67108921
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
      return View.insert('Line.next')
    elsif keys == "\cp"
      return View.insert('Line.previous')
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

    View.insert code
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

    Clipboard.save_for_yank inserted   # Store for retrieval with enter_yank

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
      dir = $el.elvar.default_directory
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
      Menu.add_item ['Xiki', i[0]], i[1], "#{i[0].downcase}-#{i[1].downcase.gsub(' ', '-')}"
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

  def self.before_last
    $el.el4r_lisp_eval("(elt (recent-keys) (- (length (recent-keys)) 2))").to_s
  end

end
