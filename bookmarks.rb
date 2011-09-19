require 'location'
require 'keys'
require 'yaml'

class Bookmarks
  extend ElMixin

  CODE_SAMPLES = %q<
    # Get path for bookmark
    p Bookmarks["$t"]  # Bookmark name is 't'

    # Expand bookmark shortcut in a path
    p Bookmarks["$tm/hi.txt"]
  >

  def self.save arg=nil
    in_a_file_tree = FileTree.handles? rescue nil

    # If U and we're in a file tree, bookmark it
    if Keys.prefix_u? && in_a_file_tree && ! Line[/^ *\|/]
      path = Tree.construct_path
      keys = Keys.input(:timed=>true, :prompt=>"Name of bookmark for #{path.sub(/.+\/(.)/, "\\1")}: ") || "0"
      with(:save_window_excursion) do
        $el.find_file path
        self.set keys
      end
      return
    end

    # If arg is a symbol, use it as the prefix
    prefix = ""
    if arg && arg.type == Symbol
      prefix = arg.to_s
    elsif arg && arg.type == String
      self.set(arg.sub(/^\$/, ""))
      return
    end
    # Use input from user or "default"
    keys = Keys.input(:timed=>true, :prompt=>"Name of bookmark for #{View.file_name}: ") || "0"
    self.set "#{prefix}#{keys}"

    # Append to bookmark file
    log_path = "/temp/bookmark_log.notes"
    if File.exists?(log_path)
      File.open(log_path, "a") { |f| f << "| $#{prefix}#{keys}\n#{View.file}\n\n" }
    end
  end

  def self.[]= bookmark, path
    with(:save_window_excursion) do
      View.open path
      Bookmarks.set bookmark
    end
  end

  # Like bookmark-set, but accepts buffers
  def self.set name
    # Just create normal bookmark if file
    return bookmark_set(name) if View.file || $el.elvar.dired_directory

    # Must be buffer

    $el.bookmark_delete name   # Delete real bookmark

    # Load bookmarks.yml
    bookmarks_yml = File.expand_path("~/bookmarks.yml")
    if File.exists?(bookmarks_yml)
      bookmarks = YAML::load IO.read(bookmarks_yml)
    end
    bookmarks = [] unless bookmarks

    # Delete if there
    already_here = bookmarks.find {|bm| bm[0] == name}
    bookmarks.delete(already_here) if already_here

    # Add to end
    bookmarks << [name, View.name]

    # Save file
    File.open(bookmarks_yml, "w") { |f| f << bookmarks.to_yaml }

  end

  # Like bookmark-jump, but accepts buffers
  def self.jump name

    # If normal bookmark found, use it
    return bookmark_jump(name) if bookmark_get_filename(name)

    buffer = self.buffer_bookmark name

    if buffer == :buffer_not_open
      return View.message "Buffer '#{buffer}' not currently open."
    end
    return nil if buffer.nil?

    View.to_buffer buffer
    return true
  end

  def self.buffer_bookmark name
    # Load bookmarks.yml
    bookmarks_yml = File.expand_path("~/bookmarks.yml")
    return nil unless File.exists?(bookmarks_yml)
    bookmarks = YAML::load IO.read(bookmarks_yml)
    bookmarks = [] unless bookmarks

    # Get buffer name
    found = bookmarks.find {|bm| bm[0] == name}
    return nil unless found

    unless View.buffer_open?(found[1])
      return :buffer_not_open
    end
    # Do nothing if already open

    found[1]
  end



  # If string passed, go to file+point of bookmark.
  # If nothing passed, go to file of user-prompted text
  # If no args passed, get bookmark from user and jump there
  def self.go bookmark=nil, options={}
    #el4r_lisp_eval "(require 'bookmark)(bookmark-maybe-load-default-file)"
    # If arg is a symbol, use it as the prefix
    prefix_to_bm = ""
    if bookmark && bookmark.type == Symbol
      prefix_to_bm = bookmark.to_s
    elsif bookmark && bookmark.type == String
      keys = bookmark
      #       return self.jump(bookmark.sub(/^\$/, ""))
    end

    # Use input from user or "default"
    keys ||= Keys.input(:timed => true, :prompt => "Enter bookmark to jump to: ") || "0"

    # Open file or jump to if already open
    path = bookmark_get_filename( "#{prefix_to_bm}#{keys}" )

    if path.nil?   # If not found, try buffer in bookmarks.yml

      return true if self.jump( "#{prefix_to_bm}#{keys}" )
      View.beep
      message("Bookmark not found!")
      return :not_found
    end

    prefix = Keys.prefix
    if prefix==9  # If 9, open in bar
      View.bar
      self.jump "#{prefix_to_bm}#{keys}"
    else
      Location.go path, :stay_in_bar => true
    end

    if options[:point] or prefix == :u   # Go to actual point
      return self.jump("#{prefix_to_bm}#{keys}")
    end
  end

  def self.keys
    # TODO: put newest keys here
  end

  # Expand $foo paths in strings.  Expects strings to bee bookmark names,
  # and replaces with corresponding paths.
  def self.[] path
    self.expand(path)
  end

  def self.expand path, options={}
    if options[:just_bookmark]   # If only a bookmark, just expand it
      return bookmark_get_filename(path)
    end

    # If $xxx found
    if path =~ /\$([._a-zA-Z0-9-]+)([\\\/]?)(.*)/
      bm, slash, rest = $1, $2, $3

      bm_orig = bm
      # Expand bookmark
      bm = bookmark_get_filename(bm)

      # If a slash, cut off filename if there is one (only dir is wanted)
      if slash != ""
        bm.sub!(/[^\\\/]+$/, "")
      end

      path = "#{bm}#{rest}"
      # Expand ~/ if it has it
      path = View.expand_path(path)  if path =~ /^~/

      path

    elsif path =~ /^~\//  # If home dir, expand
      # Expand ~/ if it has it
      View.expand_path(path)

    elsif options[:absolute] || path =~ /^\.\//  # If relative path, expand
      View.expand_path(path)
    elsif path !~ /\//   # If path doesn't contain a slash, make relative to current dir
      "#{View.dir}/#{path}"
    else
      path
    end
  end

  # Insert $bookmark into to path if it contains a bookmarked path
  def self.collapse path
    if ! @bookmarks_cache
      @bookmarks_cache = []
      # TODO: pull this list out and make configurable
      %w[a tr p n x 18].each do |name|

        bmpath = bookmark_get_filename(name)
        next unless bmpath
        bmpath.sub!(/[^\/]+$/, "")

        @bookmarks_cache << [name, bmpath]
      end
    end

    @bookmarks_cache.each do |name, bmpath|
      next unless path =~ /^#{bmpath}/
      return path.sub(bmpath, "$#{name}/")
    end
    return path
  end

  # Remove file from the end (if not dir)
  def self.dir_only path
    path.sub(/\/[^\/]+\.[^\/]+$/, '')
  end

  # Prompt user for bookmark name, and return bookmark
  def self.input options={}
    self.expand(
      Keys.input(options.merge(:timed => true)),
      :just_bookmark => true
      )
  end

  # Read a file
  def self.read name
    File.read(self.expand(name))
  end

  def self.menu
    puts "
      + .tree/
      + .list/
      "
  end

  def self.list path=nil

    if ! path   # Print all bookmarks
      all = elvar.bookmark_alist.collect { |bm|
        item = bm.to_a
        [item[0], item[1].to_a[0][1]]
      }
      all.sort.each do |l|
        n, p = l
        puts "- #{n}: #{p.sub(/\/$/,'')}"
        #         puts "- #{n.ljust(7)} #{p.sub(/\/$/,'')}"
        #puts "- #{n}: #{p}"
      end
      return
    end

    # Open single path
    View.open path[/ ([~\/].+)/, 1]
  end

  def self.tree
    #     paths = elvar.bookmark_alist.collect {|bm|
    #       bm.to_a[1].to_a[0][1]
    #     }.select{|path| path =~ /^\/.+\w$/}

    paths = elvar.bookmark_alist.collect {|bm|
      ary = bm.to_a
      key = ary[0]
      path = ary[1].to_a[0][1]
      [key, path]
    }
    paths = paths.select{|a| a[1] =~ /^\/.+\w$/}   # Remove bm's to dirs
    paths.collect! {|bm|
      path = bm[1]
      path.sub! /(.+\/)/, "\\1#{bm[0]}: "
      path
    }

    puts CodeTree.tree_search_option + FileTree.paths_to_tree(paths)
  end
end

$el.el4r_lisp_eval("(require 'bookmark)")
$el.bookmark_maybe_load_default_file
