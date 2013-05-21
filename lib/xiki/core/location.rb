module Xiki
  # Saves and restores locations.  A location can include multiple of
  # these: a file, a line number, a column number
  class Location

    def self.menu
      %`
      - examples/
        | orig = Location.new   # Save where we are
        | Line.next 4;  View.open '/tmp'   # Go Somewhere else
        | orig.go   # Go back to where we were
      `
    end

    attr_accessor :line
    attr_accessor :file
    attr_accessor :column
    attr_accessor :buffer

    @@spots = {}

    def file_or_buffer
      @file || @buffer
    end

    # Save file and location
    def initialize *args
      # Just use file if it's passed
      if args[0] && args[0].class == String
        @file = File.expand_path(args.shift)
        return
      end

      if args[0] && args[0].class == Hash
        options = args.shift
        if options[:save_scroll_position]
          @scroll_position = ($el.line_number_at_pos $el.point) - ($el.line_number_at_pos $el.window_start)
        end
      end

      @file = $el.buffer_file_name
      @buffer = View.name
      @line = Line.number
      @column = $el.point - $el.point_at_bol

      # Get buffer if no file
      @buffer = $el.buffer_name unless @file

    end

    # Use View.open instead of this, where possible
    # Opens path, in other window if already open.
    # If nothing passed, prompt user.  If string assume it's a path.
    # If symbol, assume it's a bookmark.
    def self.go path=nil, options={}
      # If no param passed, get key from user and go to corresponding location
      if path.nil?
        loc = Keys.input(:optional => true)
        loc ||= "0"
        loc = "_#{loc}"
        View.open("$#{loc}")
        # Optionally go to point
        $el.bookmark_jump(loc) unless $el.elvar.current_prefix_arg
        return

      # If symbol, look up location in map and go to it
      elsif path.class == Symbol
        # @@hash[path.to_s].go
        View.open("$#{path.to_s}")
        $el.bookmark_jump(path.to_s)
        return

      # If string, look up location in map and go to it
      elsif path.class == String and path[/^\$./]
        View.open(path, :goto_point => true)

        # Jump to specific boomkark point - redundant??
        # Is this even a bookmark?
        $el.bookmark_jump(path.sub(/^\$/, ""))
        return
      end

      # Otherwise, go to path passed in
      self.new(path).go(options)
    end

    # Goes to location, with whatever information we have.  Note that if
    # file is already shown, we just move to its window.
    def go options={}
      if ! options[:assume_file]
        if @file
          View.open(@file, options)
        else
          View.to_buffer(@buffer)
        end
      end

      $el.goto_line @line if @line

      # Exit if no column is set
      return unless @column

      # If enough space, just move to column
      if $el.point + @column <= $el.point_at_eol
        $el.forward_char @column
      # Otherwise, move to end
      else
        $el.end_of_line
      end

      $el.recenter @scroll_position if @scroll_position
    end

    # Saves a generic location based on user input
    def self.save name=nil
      # Use string if user types it quickly (or arg, if passed)
      name ||= Keys.input(:prompt => "Save this spot as (pause when done): ", :optional => true)
      name ||= "0"
      #path = path.to_s unless path.class == String
      name = "_#{name}"
      # Remove beginning $ (it might have been passed in with the arg)
      name.sub!(/^\$/, "")

      # Save location in corresponding register
      # @@hash[name] = Location.new
      $el.bookmark_set(name)
    end

    def self.jump path=nil
      path ||= "0"
      path = "_#{path}"
      #path = path.to_s unless path.class == String
      View.open("$#{path}")
      # Optionally go to point
      $el.bookmark_jump(path) #unless $el.elvar.current_prefix_arg
    end

    # Enter selected text at spot
    def self.enter_at_spot
      txt = View.selection
      txt = FileTree.snippet if Keys.prefix_u?
      Location.jump("0")
      View.set_mark
      $el.insert txt
    end

    def self.as_spot key='0'

      # Remember window (in case buffer in 2 windows)
      @@spot_index = View.index if key == '0'

      if View.file   # If buffer has a file, just save in location
        Location.save(key)
        return @@spots[key] = nil
      end

      # Must be a buffer
      @@spots[key] = [$el.buffer_name(View.buffer), $el.point]
    end

    def self.to_spot key='0'
      if @@spots[key]
        View.to_buffer @@spots[key][0]
        View.to @@spots[key][1]
      else    # If file, just jump
        # If original window/buffer still there, go back
        if key == '0'
          if $el.buffer_file_name($el.window_buffer(View.nth(@@spot_index))) == Bookmarks["$_0"]
            View.to_nth @@spot_index
          end
        end
        Location.jump(key)
      end
    end

  end
end
