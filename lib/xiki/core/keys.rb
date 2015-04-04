require 'xiki/core/pause_means_space'
require 'xiki/core/line'
require 'xiki/core/xi'
require 'xiki/core/text_util'

module Xiki
  # Methods for defining keyboard shortcuts
  class Keys

    @@key_queue = []   # For defining menus (must be done in reverse)
    @@source ||= {}   # Stores source for new unified key defs

    # Stores definition of new expanding keys. Structure:
    #   "view"     => {
    #     "create"   => #<Proc>,
    #     "hide"     => #<Proc>,
    #   },
    #   "quit"     => #<Proc>,

    # Default key menu order...

    def self.map_default
      {
        "window"=>nil,
        "open"=>nil,

        "hop"=>nil,
        "jump"=>nil,
        "do"=>nil,

        "as"=>nil,
        "enter"=>nil,
        "run"=>nil,
        "search"=>nil,
        "custom"=>nil,
      }
    end

    def self.map_default_noob
      {
        "window"=>nil,
        "open"=>nil,
        "hop"=>nil,

        "jump"=>nil,
      }
    end

    @@map ||= Keys.map_default
    @@map_noob ||= Keys.map_default_noob
    @@noob_mode = nil

    # Saves by into ~/xiki/commands/conf/ by replacing
    # the line (copying the default conf over first, if
    # it's not there yet.
    # Keys.write_to_conf "return warning", " 4"
    def self.write_to_conf key, value

      # Read in file...

      user_conf = Bookmarks[":xh/commands/conf/xsh.conf"]
      FileUtils.mkdir_p File.dirname user_conf   # In case it doesn't exist yet

      txt = File.read(user_conf) rescue nil

      # If not there, read from default...

      if ! txt
        txt = File.read(Bookmarks[":xiki/commands/xsh/default.conf"]) rescue nil
      end

      # Update file accordingly

      result = txt.sub! /^(#{key}:).*/, "\\1 #{value}"

      # No replacement means it somehow wasn't in the file, so append to the end...

      txt << "\n#{key}: #{value}\n" if ! result

      # Write file...

      File.open(user_conf, "w") { |f| f << txt }

      nil
    end


    # Reads from ~/xiki/commands/conf/
    # Keys.read_from_conf "return warning"
    def self.read_from_conf key
      txt = File.read Bookmarks[":xh/commands/conf/xsh.conf"] rescue nil
      return nil if ! txt
      txt[/^#{key}: (.*)/, 1]   # => noob
    end

    def self.noob_mode value=nil

      # No value, so return the result
      if value == nil

        # Memo-ize it, so we don't look it up every time
        if @@noob_mode == nil
          value = self.read_from_conf "key shortcuts"
          @@noob_mode = ! value || value == "noob"
        end
        return @@noob_mode
      end

      # Value passed so set it in the cache and on the disk
      @@noob_mode = value
      self.write_to_conf 'key shortcuts', (value ? 'noob' : 'advanced')
    end

    # Called when expanding key shortcuts are pressed.
    def self.expand path

      prefix = Keys.prefix

      # In noob mode, and ^A, ^E, ^R, or ^D, so just run simplified version...

      if Keys.noob_mode
        case path
        when ["as"]
          return Move.hop_left_key
        when ["enter"]
          return Move.hop_right_key
        when ["run"]
          return Shell.recent_history_external nil, :from_key_shortcut=>1
        when ["do"]
          return Deletes.forward
        end
      end

      # See if user types anything right after
      key = Keys.input(:optional=>true, :chars=>1, :prompt=>"")

      # [] and key was "k", so do keys+kill...
      if path == [] and key == "k"
        Keys.remember_key_for_repeat(proc {Clipboard.kill})
        return Clipboard.kill
      end
      # [] and key was "m", so do keys+more...
      if path == [] and key == "m"
        return Launcher.open "keys/\n  + more/", :hotkey=>1
      end

      # Key pressed quickly, so recurse...

      if key && key != ","
        hash = Xi.hget(@@map, *path)

        name = hash.keys.find{|o| o =~ /^#{key}/}
        path << name

        # If Proc, run it...

        if hash[name].is_a? Proc
          self.remember_key_for_repeat path # , :prefix=>prefix   # Remember key pressed > so C-. can repeat it...

          # Eventually handle also
          #   Keys.prefix arg
          #   :task arg
          return hash[name].call
        end

        # Else, recurse...

        return self.expand path
      end

      # No shortcut in time, so display menu...

      # If nothing, use "keys" menu
      path = ["keys"] if path == []

      options = {:bar_is_fine=>1}
      if key != ","
        options.merge! :hotkey=>1, :letter_when_not_found=>1
      end

      # First arg is apparently the command and what the buffer will be named
      command = path.join("/")+"/"
      buffer_name = path.join("+")+"/"
      Launcher.open command, options.merge(:buffer_name=>buffer_name)

    end

    def self.kill # args, options
      View.kill if View.name == "keys/"
      Clipboard.kill
    end

    def self.more args, options

      txt = %`
        + arrow keys/
          | You can type the arrow keys to move the cursor around.
          |
          | Often much of what you'll do in Xiki is use the arrow keys and
          | type Ctrl+X to expand and collapse things.
          |
          | Also, the arrow keys can help you escape out of odd situations,
          | like when you start typing a key shortcut and become confused.
          | (Note that the escape key doesn't escape out of things, due to
          | the limitations of shell consoles.)
        + copying and pasting, etc/
          | You can use these keys to copy, paste, cut, and undo:
          |
          |   Ctrl+C, Ctrl+V, Ctrl+X, Ctrl+Z
          |
          | To select some text before copying or cutting, type Ctrl+Space
          | and then move the arrow keys to select.
        + mouse/
          | If you're using a terminal that has mouse support (like iTerm),
          | you can you the mouse to...
          |
          | - click to reposition the cursor
          | - double-click to expand
          | - right-click to see a tasks menu
      `.unindent

      txt << "\n"
      txt <<
        if Keys.noob_mode
          %`
          + enable advanced key shortcuts mode
            ! Keys.noob_mode false
            ! options[:no_slash] = 1
            ! "
            ! | Advanced mode enabled. Many useful shortcuts are now set:
            ! |
            ! |   ^A  as+... shortcuts
            ! |   ^E  enter+... shortcuts
            ! |   ^R  run+... shortcuts
            ! |   ^D  do+... shortcuts
            ! |
            ! | Type ^A, ^E, ^R and ^D twice to get the original behavior:
            ! |
            ! |   ^A ^A  beginning of line
            ! |   ^E ^E  end of line
            ! |   ^R ^R  recent commands
            ! |   ^D ^D  delete character
            ! |
            ! | Type ^K to see the current shortcuts, or to switch back to noob mode.
            ! "
          `.unindent

        else
          %`
          + enable noob mode
            ! Keys.noob_mode true
            ! options[:no_slash] = 1
            ! "
            ! | Noob mode enabled. These key shortcuts are now set:
            ! |
            ! |   ^A  beginning of line
            ! |   ^E  end of line
            ! |   ^R  recent commands
            ! |   ^D  delete character
            ! |
            ! | These hide many advanced keys, but are simpler for new users.
            ! |
            ! | Type ^K to see the current shortcuts, or to switch back to advanced mode.
            ! "
          `.unindent
        end

      options_in = options.merge(:eval=>1)
      txt = Xik.new(txt).expand args, options_in

      # Propagate some options back
      Options.propagate_some options_in, options

      txt

    end

    # Implements menu for each key, (=view, =open, etc).
    def self.menu_expander path, options={}

      in_own_view = View.name =~ /\/$/
      options[:hotkey] = 1 if options[:was_letter]

      # keys/copying and pasting/, so do special handling of this item...

      return self.kill if path[0] == "k"
      return self.more(path[1..-1], options) if path[0] == "more"

      return Launcher.open("help") if path[0] == "help"

      # Special args of keys menu

      map = self.noob_mode ? @@map_noob : @@map

      item = Xi.hget(map, *path)

      # Key combo wasn't initially found in noob map, so look in full map via the single letter...

      if ! item && self.noob_mode
        siblings = Xi.hget(@@map, *path[0..-2])
        item = siblings.find{|k, v| k =~ /^#{path[-1]}/}[1]
      end

      if ! item
        options[:no_search] = 1
        return "<! Key not defined"
      end

      # More items, so show next keys...

      if item.is_a?(Hash)

        txt = ""
        item.each do |k, v|
          next if ! v
          next if k !~ /^[a-z]/i   # Remove items that don't start with words
          next if path == [] && k == "keys"

          txt << "+ #{k}"
          txt << "/" if v.is_a? Hash
          txt << "\n"
        end

        # Add blank lines if "view" menu
        txt = self.decorate_items path, txt

        return txt
      end

      # Leaf key (proc), so run it...

      # Task, so show options or navigate...

      if task = options[:task]
        return "~ source\n~ run" if task == []
        if task == ["source"]
          file, line = item.source_location   # => ["/projects/xiki/lib/xiki/core/key_shortcuts.rb", 755]
          View.open file
          View.line = line
          return ""
        end
      end

      # Kill view if name is =...

      View.kill if in_own_view

      # Remember key pressed > so C-. can repeat it...

      self.remember_key_for_repeat path
      item.call

      nil

    end

    def self.decorate_with_spacing path, txt

      # Noob user, so add spacing to simplified item subset...

      if self.noob_mode
        if path == []
          txt.gsub!(/^\+ (quit|jump|as|enter|do|run)\/?\n/, "")   # Remove quit and xpand
          return
        elsif path == ["open"]
          txt.gsub!(/\+ [fp]/, "\n\\0")
          return
        elsif path == ["window"]
          txt.gsub!(/\+ [c]/, "\n\\0")
          return
        elsif path == ["hop"]
          txt.gsub!(/\+ [su]/, "\n\\0")
          return
        elsif path == ["as"]
          txt.gsub!(/\+ [u]/, "\n\\0")
          return
        elsif path == ["enter"]
          txt.gsub!(/\+ [t]/, "\n\\0")
          return
        end
      end

      # All keys shown, so add spaces...

      if path == []
        txt.gsub!(/^\+ (xpand|quit|grab|tasks)\n/, "")   # Remove quit and xpand
        txt.gsub!(/(^\+ (backward|forward|previous|next)\n)+/, "")   # Remove backward, forward, previous, next
        txt.gsub!(/\+ [hal]/, "\n\\0")

      elsif path == ["as"]
        txt.gsub!(/\+ [clut]/, "\n\\0")
      elsif path == ["enter"]
        txt.gsub!(/\+ [bjhqi]/, "\n\\0")

      elsif path == ["hop"]
        txt.gsub!(/\+ [stuoc]/, "\n\\0")
      elsif path == ["jump"]
        txt.gsub!(/\+ [oqlryc]/, "\n\\0")
      elsif path == ["open"]
        txt.gsub!(/\+ [tpfuel]/, "\n\\0")

      elsif path == ["window"]
        txt.gsub!(/\+ [hndeatwq]/, "\n\\0")
      elsif path == ["run"]
        txt.gsub!(/\+ [miehv]/, "\n\\0")
      elsif path == ["run", "version"]
        txt.gsub!(/\+ [rb]/, "\n\\0")
      elsif path == ["run", "delete"]
        txt.gsub!(/\+ [icm]/, "\n\\0")
      elsif path == ["enter", "in"]
        txt.gsub!(/\+ [rhl]/, "\n\\0")
      elsif path == ["do"]
        txt.gsub!(/\+ [ntq]/, "\n\\0")

      elsif path == ["search"]
        txt.gsub!(/\+ [vtbeh]/, "\n\\0")
      elsif path == ["custom"]
        txt.gsub!(/\+ [bch]/, "\n\\0")
      end

    end

    @@descriptions = {
      []=>"
        Keyboard shortcuts. Right now, and anywhere in xsh, you
        can type the first letter of one of the below words while
        holding Ctrl (^W for window, ^O for open, or ^H for hop).

        The arrow keys always move around. ESC moves between views.
        Try using the arrow keys then Ctrl+X to explore these items:
        ".unindent+"\n",
      ["as"]=>"Saving and remembering",
      ["enter"]=>"Inserting stuff",

      ["hop"]=>"Cursor movement",

      ["jump"]=>"Going to specific places",

      ["open"]=>"Opening files, views, and lists",

      ["window"]=>"Splitting and navigating",
      ["do"]=>"Various actions",

      ["run"]=>"Running and processing things",
      ["search"]=>"Shortcuts you can use during a Ctrl+S search",
      ["custom"]=>"Related to the kind of file you're viewing",

      ["run", "version"]=>"Diffing and listing versions",

    }

    def self.decorate_items path, txt

      self.decorate_with_spacing path, txt

      # Maybe don't show > require right-click?
      description = @@descriptions[path]

      # keys/ in advanced mode, so don't show the long decription
      description = nil if path == [] && ! self.noob_mode

      if description
        description = Tree.pipe(description).strip
        description.sub! /\|\z/, ""   # Remove "|" from last blank line in description
        txt = "#{description}\n#{txt}"
      end

      # Special extra text for root "keys"...

      if path == []
        if self.noob_mode

          txt << "\n"+"
            + more/
            + help

            | ^K ^K to kill line
            | Also see bottom bar below (press an arrow key if it says to)
            ".unindent
        else
          txt << "\n+ more/\n+ help" if path == []
        end
      end

      txt
    end


    # Sets key shortcuts in @@map, and defines root shortcuts
    # that read from them.
    def self.map_reset
      @@map = self.map_default
    end

    def self.map_noob
      @@map_noob
    end

    def self.map keys=nil, options={}
      return @@map if keys.blank?   # Just return map if no keys

      name = keys[0]
      initial = name[0]
      if keys[1].is_a? Proc

        # Single key combination (a proc), so define it directly...

        map = options[:map] || :global_map
        $el.define_key map, "\\C-#{initial}", &keys[1]

      elsif ! @@map[name]

        # Multiple key combo, so define root key as expander, only if not defined yet...

        if ! ["s", "c"].member? initial
          map = options[:map] || :global_map
          $el.define_key map, "\\C-#{initial}" do
            Keys.expand [name]
          end
        end

        # Define menu, that will optionally pop up if they're slow...

        Xiki.def(name) do |path, options|
          Keys.menu_expander [name]+path, options
        end

      end

      # Keys, so set in map...

      Xi.hset @@map, *keys

      # Also save to @@map_noob if option

      if options[:noob]
        Xi.hset @@map_noob, *keys
      end

    end

    # Have user type in key.  Returns...
    #   ["as", "enter"], <Proc...>
    def self.prompt_for_key

      combo, found = [], nil

      while ! found
        val = Xi.hget(@@map, *combo)

        if ! val
          View.flash "- Key not defined: #{combo}!"
          raise "key not defined"
        end

        # Hash, so ask for another key and narrow down...

        if val.is_a? Hash
          key = Keys.input :chars=>1
          word = val.keys.find{|o| o =~ /^#{key}/}
          if ! word
            View.flash "- Key not defined: #{combo+[key]}!"
            return
            # raise "key not defined"
          end
          combo << word
        else
          found = val
        end
      end

      [combo, found]

    end



    def self.source
      @@source
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
      - @~/my_xiki_stuff/
        - keys.rb
          | # My shortcuts
          | Keys.enter_name { View << "Steve" }
          | Keys.enter_yay { View << "Yay" }
      |
      | Then you could require this file when xiki loads by adding this line:
      |
      - @~/.el4r/
        - init.rb
          | require "~/my_xiki_stuff/keys"
      '
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
    #
    # Sample usages:
    # Keys.input   # Terminated by enter
    # Keys.input "Type something: "
    # Keys.input :chars=>1   # Just one char
    # Keys.input :timed=>1   # Terminated by pause (and convert control to alpha)
    # Keys.input :optional=>1   # Terminated by pause
    #   - A pause at the beginning will result in no input (nil)
    def self.input *args

      prompt = args.shift if args[0].is_a?(String)

      options = args[0] || {}

      return self.input_with_choices(options) if options[:choices]

      # Cursor.remember :before_input

      prompt ||= options[:prompt] || "Input: "

      if options[:chars] && ! options[:optional]
        char = $el.char_to_string(
          self.remove_control($el.read_char(prompt))).to_s
        return char
      end

      # If simple un-timed input, just get string and return it...

      unless options[:timed] || options[:optional]
        c = $el.read_string(prompt, options[:initial_input])
        return c
      end

      # :timed or :optional option...

      keys = ""

      # $el.elvar.inhibit_quit = true
      c = nil

      # If not optional, wait for input initially
      unless options[:optional]
        c = $el.el4r_lisp_eval("(let ((inhibit-quit t)) (read-char #{prompt.inspect}))")#.to_s
        keys = self.to_letter(c)
      end

      if c == 7 || c == 27
        $el.keyboard_quit
      end

      # No, leave it alone, since it's timed (mouse won't interfere)
      while(c = $el.el4r_lisp_eval("(let ((inhibit-quit t)) (read-char \"#{prompt}#{keys}\" nil 0.35))"))
        keys += self.to_letter(c)
        break if options[:optional] && options[:chars] == 1
      end

      $el.message ""
      # If nothing, return nil
      keys == "" ? nil : keys
    end

    def self.input_with_choices options
      prompt = options[:prompt] ? "#{options[:prompt]} " : ""
      prompt << options[:choices].map{|i|
        "[#{i.first[/./]}]#{i.first[/.(.+)/,1]}"}.
        join(', ')
      c = Keys.input :chars=>1, :prompt=>prompt
      options[:choices].find{|i| i.first =~ /^#{c}/}[1]
    end

    # Converts character number to char or "C-..." string.
    # p Keys.to_letter 32
    def self.to_letter ch, options=nil
      verbose = options && options[:verbose]
      return nil if ch.nil?
      if ch == 0
        verbose = nil if verbose
        ch = 32
      elsif ch < 27
        verbose = "C-" if verbose
        ch += 96
      elsif 67108896 <= ch and ch <= 67108925
        verbose = "C-" if verbose
        ch -= 67108864
      elsif 134217825 <= ch and ch <= 134217850
        verbose = "M-" if verbose
        ch -= 134217728
      else
        verbose = nil if verbose
      end
      return "#{verbose}#{ch.chr}"
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
      code.sub! /.*{(.+)}.*/, "\\1"
      code.gsub! ' }', ''
      code.gsub! '(:blink => (true))', ''
      code.strip!

      View << code
    end

    def self.jump_to_code

      # Defined in Keys.map, so jump to proc...

      combo, prok = Keys.prompt_for_key
      if prok
        file, line = prok.source_location
        View.open file
        View.to_line line

        # Jump to source of method inside the proc...

        if Keys.prefix != :u && line = Line.value[/{ ?(\w+\.\w+?) ?}/, 1]
          Search.open_file_and_method line
        end

      end

    end

    def self.proc_from_key keys

      code = $el.prin1_to_string($el.key_binding(keys))
      # If it is a call to elisp
      id = code[/el4r-ruby-call-proc-by-id.+?([_0-9]+)/, 1]
      return nil if id.nil?

      ObjectSpace._id2ref(id.to_i)
    end

    def self.timed_insert_handle_char char
      if char == "\x7F"   # If delete, just do a delete
        return $el.delete_backward_char 1
      end
      View.insert char
    end

    def self.timed_insert options={}

      prefix = Keys.prefix

      # 0 prefix, so insert in a way that works with macros...

      if prefix == 0
        View.insert Keys.input(:prompt => "Insert text to insert: ")
        return
      end

      Cursor.remember :before_q
      Cursor.box

      beginning = View.cursor

      prompt = options[:prompt] || "insert text (pause to exit): "

      # Get first char and insert
      c = $el.read_char(prompt).chr

      # Do nothing if it was escape

      return if c == "\e"

      inserted = "#{c}"
      self.timed_insert_handle_char c

      # While no pause, insert more chars
      while(c = $el.read_char(prompt, nil, 0.36))
        inserted += c.chr
        self.timed_insert_handle_char c.chr
      end

      $el.elvar.qinserted = inserted
      $el.message ""

      txt = View.txt beginning, View.cursor

      # Save txt as string to > .remember_key_for_repeat...

      if prefix == :u
        View.cursor = beginning
        self.remember_key_for_repeat(proc {View >> txt})
      else
        self.remember_key_for_repeat(proc {View << txt})
      end

      Cursor.restore :before_q

      # Store in hash by first letter for use by enter_yank

      # Numeric prefix, so repeat that many times...

      if prefix.is_a? Fixnum

        (prefix-1).times do
          View.<< txt
        end

      end

      inserted

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

    #
    # Set prefix, or append it (space-delimited) if one already there.
    #
    def self.add_prefix new_prefix
      prefix = self.prefix

      return self.prefix = new_prefix if ! prefix   # If none there already, just set it

      self.prefix = "#{self.prefix} #{new_prefix}"
    end

    def self.prefix options={}
      return nil if ! $el
      pre = $el.elvar.current_prefix_arg
      return nil unless pre

      # Clear prefix if :clear
      $el.elvar.current_prefix_arg = nil if options[:clear]
      str = pre.to_s

      return :u if str == "u"

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

    def self.up? options={}
      self.prefix_u options
    end

    def self.update? options={}
      # TODO update so prefix can have multiple values, like C-u and "update"
      #   for when C-u as+update
      #   (space-separated list)?
      #     "u update"
      self.prefix == "update"
    end

    def self.delete? options={}
      self.prefix == "delete"
    end

    def self.open? options={}
      self.prefix == "open"
    end

    def self.prefix_n options={}
      pre = self.prefix(options)
      return pre if pre.is_a?(Fixnum)
      return $&.to_i if pre.is_a?(String) && pre =~ /\d+/
      nil
    end

    def self.prefix_uu
      self.prefix == :uu
    end

    def self.clear_prefix
      return if ! $el
      $el.elvar.current_prefix_arg = nil
    end

    # Prompts for input of bookmark name, then returns its file path.
    # If bookmark is to a file, it returns the enclosing dir.
    # Keys.bookmark_as_path   # If user typed "ru"...
    #   /notes/ruby/
    # Keys.bookmark_as_path :include_file=>1   # If user typed "ru"...
    #   /notes/ruby/index.notes
    # Keys.bookmark_as_path :prompt=>"Enter something"   # Show message
    # Keys.bookmark_as_path :bm=>"ru"   # Don't prompt
    # Keys.bookmark_as_path :bm=>"."   # Current file
    # Keys.bookmark_as_path :bm=>".."   # Current dir
    # Keys.bookmark_as_path :bm=>"..."   # Parent dir
    #   /projects/xiki/lib/xiki/core/
    def self.bookmark_as_path options={}

      bm = options[:bm] || Keys.input(:timed=>true, :prompt=>options[:prompt]||"Enter a bookmark: ")

      return if bm == "\e"

      if bm == " "   # If space, return special token
        return :space
      elsif bm == "/"   # If slash, return special token
        return :slash
      elsif bm == "."   # Dash means the current file
        return View.file
      elsif bm =~ /^\.+$/   # If .+ do tree in current dir
        dir = View.dir :force_slash=>1
        (bm.size - 2).times do
          dir.sub! /\/$/, ''   # Remove / on end if there
          dir.sub! /[^\/]+$/, ''   # Remove dir
        end
        dir.sub! /^$/, '/'   # If nothing left, use root (/)
        return dir
      end

      dir = Bookmarks.expand ":#{bm}"

      if dir.nil?   # If no dir, return nil
        View.beep "- Bookmark '#{bm}' doesn't exist."
        return :bookmark_doesnt_exist
      end

      unless options[:include_file]
        dir = Bookmarks.dir_only dir
        dir << "/" unless dir =~ /\/$/
      end
      dir

    end

    def self.prefix_times prefix=self.prefix, &block
      result = case prefix
        when nil, :u, :uu, :uuu
          1
        else
          prefix
        end
      result.times{ block.call } if block
      result
    end

    def self.add_menubar_items
      @@key_queue.reverse.each do |i|
        next if ! i[0]   # Just get this to continue on
        Menu.add_menubar_item [Menu::ROOT_MENU, i[0]], i[1], "#{i[0].downcase}-#{i[1].downcase.gsub(' ', '-')}"
      end
      @@key_queue = []
    end

    def self.char

      # Is it an issue to comment this out?

      # Why doesn't work?
      $el.elvar.control_lock_disable_once = true   # Because we need to distinguish between C-n and n, etc.
      $el.elvar.overriding_local_map = $el.elvar.xiki_az_keymap   # So C-x and C-c will be single chars

      ch_initial = $el.read_key_sequence "", nil, nil, true

      $el.elvar.overriding_local_map, $el.elvar.control_lock_disable_once = nil, nil

      # Char was a vector (or more than 1 char), so just execute corresponding command (probably a click)...
      if $el.vectorp(ch_initial) || ch_initial.length > 1
        $el.command_execute ch_initial
        return
      end

      ch_initial = "#{$el.string_to_char ch_initial}"   # Get it into weird format we used before switching to read_key_sequence

      if ch_initial =~ /^\d+$/   # If a number, assign it to raw

        ch_raw = ch_initial.to_i

        if 134217825 <= ch_raw and ch_raw <= 134217850  # If meta (out of elisp range)
          return ["meta_#{(ch_raw - 134217728).chr}".to_sym, nil]
        end

        # Special check for C-. and other sequences
        ch =
          if ch_raw == 67108910
            :control_period
          elsif ch_raw >= 67108912 && ch_raw <= 67108921   # If between C-0 and C-9
            (ch_raw - 67108864).chr
          elsif ch_raw == 67108911
            :control_slash

          elsif ch_raw == 28
            :control_backslash

          else
            # If char is over the elisp max, try to interpret it as Meta
            $el.char_to_string(ch_raw)
          end

        return [ch, ch_raw]

      elsif ['left', 'right', 'up', 'down', ].member?(ch_initial)
        return [ch_initial.to_sym, 0]   # Arbitrary indicator for arrow keys

      elsif ch_initial == "A-return"
        return [:meta_return, 13]

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

    # Keys.words_to_letters "hey_you"
    #   HY
    def self.words_to_letters txt
      TextUtil.camel_case(txt).gsub(/[a-z]/, '')
    end

    def self.last nth=1
      $el.el4r_lisp_eval("(elt (recent-keys) (- (length (recent-keys)) #{nth}))").to_s
    end

    # Returns codes for ast few keys pressed, in the order:
    #   [recent, one ago, two ago]
    def self.recent_few

      return $el.recent_keys.to_a.reverse

    end

    def self.isearch_prefix shortcut_length=2

      # Return it if character before key shortcut was C-u or C-0 - C-9...
      char = Keys.last(shortcut_length+1).to_i
      return :u if char == 21
      return :- if char == 67108909
      return (char - 67108912) if char >= 67108912 && char <= 67108921

      nil
    end

    def self.human_readable txt
      txt.split(/[^a-z]/i).map{|o| "Control-#{o[/./].upcase}"}.join(" ")
    end

    private

    def self.define_key map, keys_raw, keys, &block
      map ||= :global_map
      $el.define_key map, keys, &block
      self.define_key_extra map, keys_raw, &block
    end

    # Maybe define key again, if it starts with C-i for compatibility with control-lock.
    def self.define_key_extra map, keys_raw, &block
      return if keys_raw !~ /\bC-i\b/

      keys_raw = keys_raw.gsub /\bC-i\b/, "C-<tab>"
      wrapper = lambda { block.call }

      $el.define_key map, $el.kbd(keys_raw), &wrapper
    end

    def self.log options={}
      codes = $el.recent_keys.to_a.reverse

      if ! options[:raw]   # Unless they wanted it raw
        codes = codes[0..30]   # Only show a few
        # Turn into letters
        codes = codes.map{|o| Keys.to_letter o, :verbose=>1 }

        codes = codes.map{|o| "| #{o}\n" }.join("")
        codes.gsub!(/  $/, " space")
      end

      codes
    end

    # Keys.sequence_to_string "\f\x03"
    def self.sequence_to_string keys
      keys.split('').map{|o| Keys.to_letter(o.sum).upcase}.join('')
    end

    # Filters a list if items based on one or a few keys.
    # To match, it finds the note file starting
    # with the first char, and containing the second.  So, "r" or
    # "rb" would match "ruby".
    def self.filter list, keys

      regex = Regexp.new "^#{keys}"   # First, try exact match, starting at the beginning
      keys = keys.split(//)
      first = keys.shift
      found = list.find{|o| o =~ regex}

      return found if found

      regex = "^#{first}"   # Then, try starts with 1st letter, and any combination in order
      keys.each{|o| regex << ".*#{o}"}
      regex = Regexp.new regex
      found ||= list.find{|o| o =~ regex}
      found
    end

    def self.load_keys_dir
      dir = File.expand_path "~/xiki/keys/"

      return if ! File.directory? dir   # If dir no there, just don't load it

      Dir.new(dir).entries.grep(/^[^.]/).each do |shortcut_name|
        next if shortcut_name == "reload.rb" || shortcut_name == "keys_index.rb"

        pluses = shortcut_name[/\w+/].gsub("_", "+")
        Xiki.def(pluses) do
          file = "#{dir}/#{shortcut_name}"
          txt, out, exception = Code.eval File.read(file), file, 1, :pretty_exception=>1, :target_module=>::Xiki
          View.open("exception", :txt=>"> Error while running #{pluses.gsub("_", "+")}\n#{exception}") if exception
        end
      end
      nil
    end

    # Expands lines like these.  The 1st runs the key shortcut,
    # the 2nd edits it (assumes it's in the keys/ dir, not defined
    # the old way).
    #
    # do+something
    # do+something.
    def self.expand_plus_syntax path, options

      key, txt = path
      key_orig = key.dup
      key.gsub! "+", "_"

      extension = key.slice!(/\.\w*$/)

      file = File.expand_path "~/xiki/keys/#{key}.rb"

      file_exists = File.exists? file

      if options[:prefix] == "open" && file_exists   # as+open, so just jump to it
        View.open file
        return
      end

      # foo+bar.*, so insert show or save the source...

      if extension

        # foo+bar., so redirect to with rb

        if extension == "."

          Line.sub! /$/, "rb"
          Launcher.launch
          return
        end

        if ! txt

          # foo+bar., so insert text...

          if ! file_exists

            # Check for whether it's defined the old way. Borrow > open+key."]

            proc = self.proc_from_acronym key

            # If mapped the old way, just jump to it...

            if proc
              Code.jump_to_proc_source proc
              return
            end

            return "| # Doesn't exist yet.\n| # Add some code here to define this shortcut.\n| View >> 'hi'"
          end

          # Insert source...

          options[:no_slash] = 1
          txt = Tree.quote(File.read(file), :char=>"|")
          return txt

        else

          # foo+bar./| code, so save...

          File.open(file, "w") { |f| f << txt }

          self.load_keys_dir   # Reload in case it's new

          return "<! saved!"

        end
      end

      # foo+bar, so run it...

      if ! file_exists   # If defined the old way, run it
        proc = self.proc_from_acronym key

        if ! proc
          View.flash "- Not mapped yet.  Expand again to create!", :times=>4
          return "=replace/line/\n  #{key_orig}."
        end

        if options[:prefix] == "open"
          Code.jump_to_proc_source(proc)
          return
        end

        proc.call
      end

      txt, out, exception = Code.eval File.read(file), file, 1, :pretty_exception=>1, :target_module=>::Xiki

      View.open("exception", :txt=>"> Error while running #{key_orig}\n#{exception}") if exception
    end

    # Returns the proc that is mapped to an acronym.
    # Keys.proc_from_acronym "layout create"
    def self.proc_from_acronym acronym
      meth = Keys.words_to_letters acronym
      keys_raw = Keys.translate_keys meth   # Translate to 'C-t C-f' etc
      keys = $el.kbd keys_raw   # Translate to actual control keys
      proc = Keys.proc_from_key keys
      proc
    rescue
      nil
    end

    # Mapped to C-k. Usually people will pause after, and it'll just
    # show =keys/.
    def self.k_key

      # Just open the menu
      Launcher.open "keys/", :bar_is_fine=>1, :hotkey=>1

      # # If we want a delay when C-k (probably not the best tradeoff)
      # Keys.expand []
    end

    @@last_key, @@last_prefix = nil, nil
    @@last_key_movement, @@last_prefix_movement = nil, nil

    def self.repeat options={}

      prefix = Keys.prefix :clear=>1

      # Keys.clear_prefix if prefix.is_a? Fixnum

      if options[:movement]
        Keys.prefix = @@last_prefix_movement
        prock = @@last_key_movement.is_a?(Proc) ?
          @@last_key_movement :
          Xi.hget(@@map, *@@last_key_movement)

        # In the future, this could contain strings
      else
        Keys.prefix = @@last_prefix
        prock = @@last_key.is_a?(Proc) ?
          @@last_key :
          Xi.hget(@@map, *@@last_key)
      end

      times = prefix.is_a?(Fixnum) ? prefix : 1

      times.times{ prock.call }

    end

    # Remembers this key to repeat it when C-., except for trivial keys
    def self.remember_key_for_repeat path, options={}

      # Set default flags for what it should do...

      prefix = Keys.prefix
      kind = :action   # Assume C-. to repeat. Set to :movement later if appropriate

      # Some keys are :movement's (C-, to repeat), or not recorded at all...

      if path.is_a? Array
        case path[0]
        when "next", "previous", "forward", "backward"
          return if ! prefix   # No point remembering if no prefix
          kind = :movement
        when "as"
          return if ["save"].member? path[1]   # Don't remember
        when "window"
          return if ["middle"].member? path[1]   # Don't remember
        when "run"
          return if ["save"].member? path[1]   # Don't remember
        when "hop"
          return kind = :action if ["hack"].member? path[1]   # Certain ones are actions
          return if ["start", "end"].member?(path[1]) && ! prefix   # Don't remember
          return if ["top", "bottom"].member?(path[1])   # Don't remember
          return if path[1] =~ /^[0-9]$/
          kind = :movement
        when "jump", "next", "previous"
          kind = :movement
        when "tile"
          return if ["upper", "todo", "files"].member? path[1]   # Don't remember
          kind = :movement if ! ["delete", "hide", "create"].member? path[1]   # Certain ones are actions
          # The rest are movements
        when "custom"
          kind = :movement if ["previous", "next"].member?(path[1])
        end
      end

      kind = :movement if options[:movement]

      # Store for later C-. or C-,...

      if kind == :movement
        @@last_key_movement = path
        @@last_prefix_movement = prefix == :uu ? 16 : prefix
        return
      end

      # kind == :action...

      @@last_key = path
      @@last_prefix = prefix
    end

    def self.el4r_init
      $el.el4r_lisp_eval "
        (progn
          ; Just defined, so we can use it as a local map
          (setq xiki-az-keymap
            '(keymap
              (3 . next-line)
              (24 . previous-line)
              (27 . keyboard-quit)   ; Makes escape work
            )
          )

          ; Does nothing
          (defun xiki-noop () (interactive)
          )

          ; Temporarily map Ctrl+A (etc) as single char commands...

          ; This map can be temporarily enabled to make Ctrl+A (etc) be
          ; part of a single command. The keys are mapped to xiki-noop
          ; since, when the map is used, the pre-command-hook will change
          ; the command to something else anyway.

          (setq xiki-az-control-keymap
            '(keymap
              (1 . xiki-noop)
              (2 . xiki-noop)
              (3 . xiki-noop)
              (4 . xiki-noop)
              (5 . xiki-noop)
              (6 . xiki-noop)
              (7 . xiki-noop)
              (8 . xiki-noop)
              (9 . xiki-noop)
              (10 . xiki-noop)
              (11 . xiki-noop)
              (12 . xiki-noop)
              (13 . xiki-noop)
              (14 . xiki-noop)
              (15 . xiki-noop)
              (16 . xiki-noop)
              (17 . xiki-noop)
              (18 . xiki-noop)
              (19 . xiki-noop)
              (20 . xiki-noop)
              (21 . xiki-noop)
              (22 . xiki-noop)
              (23 . xiki-noop)
              (24 . xiki-noop)
              (25 . xiki-noop)
              (26 . xiki-noop)
            )
          )
          ; Wraps and toggles
          (setq xiki-az-control-keymap-toggler
            `((xiki-filter-hotkey . ,xiki-az-control-keymap))
          )
          (add-to-ordered-list 'emulation-mode-map-alists 'xiki-az-control-keymap-toggler 200)
        )
      "
    end

    def self.accumulate

      @@accumulate = true   # Make key definitions build up elisp instead of run it as it comes
      @@accumulated = ""

      yield

      @@accumulate, @@accumulated = nil, nil

    end

    def self.open_project_menu

      # Move up until no dirs left

      dir = "#{View.dir}/tmp"   # Add on 'tmp' so first pass checks the actual dir

      found = nil

      while dir != "/"
        dir = File.dirname dir
        file = "#{dir.sub(/\/$/, '')}/menu.xiki"
        break found = file if File.exists? file
      end

      return View.flash("- No 'menu.xiki' file found in this dir or any ancestor dirs!", :times=>3) if ! found

      View.open file

    end

  end
end
