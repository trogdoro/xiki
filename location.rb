require 'view'

# Saves and restores locations.  A location can include multiple of
# these: a file, a line number, a column number
class Location
  extend ElMixin
  include ElMixin

  #@@hash = {}

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
        @scroll_position = (line_number_at_pos point) - (line_number_at_pos window_start)
      end
    end

    @file = buffer_file_name
    @line = line_number_at_pos
    @column = point - point_at_bol

    # Get buffer if no file
    @buffer = buffer_name unless @file

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
      bookmark_jump(loc) unless elvar.current_prefix_arg
      return

    # If symbol, look up location in map and go to it
    elsif path.class == Symbol
      # @@hash[path.to_s].go
      View.open("$#{path.to_s}")
      bookmark_jump(path.to_s)
      return

    # If string, look up location in map and go to it
    elsif path.class == String and path[/^\$./]
      View.open(path, :goto_point => true)
      bookmark_jump(path.sub(/^\$/, ""))
      return
    end

    # Otherwise, go to path passed in
    self.new(path).go(options)
  end

  # Goes to location, with whatever information we have.  Note that if
  # file is already shown, we just move to its window.
  def go options={}
    if @file
      View.open(@file, options)
    else
      View.to_buffer(@buffer)
    end

    goto_line @line if @line

    # Exit if no column is set
    return unless @column

    # If enough space, just move to column
    if point + @column <= point_at_eol
      forward_char @column
    # Otherwise, move to end
    else
      end_of_line
    end

    recenter @scroll_position if @scroll_position
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
    bookmark_set(name)
  end

  def self.jump path=nil
    path ||= "0"
    path = "_#{path}"
    #path = path.to_s unless path.class == String
    View.open("$#{path}")
    # Optionally go to point
    bookmark_jump(path) #unless elvar.current_prefix_arg
  end

  # Enter selected text at spot
  def self.enter_at_spot
    txt = View.selection
    txt = TreeLs.snippet if Keys.prefix_u
    Location.jump("0")
    View.set_mark
    insert txt
  end
end
