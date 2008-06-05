require 'location'
require 'keys'

class Bookmarks
  extend ElMixin

  CODE_SAMPLES = %q<
    # Get path for bookmark
    p Bookmarks["$t"]  # Bookmark name is 't'

    # Expand bookmark shortcut in a path
    p Bookmarks["$tm/hi.txt"]
  >

  def self.save arg=nil
    # If arg is a symbol, use it as the prefix
    prefix = ""
    if arg && arg.type == Symbol
      prefix = arg.to_s
    elsif arg && arg.type == String
      bookmark_set(arg.sub(/^\$/, ""))
      return
    end
    # Use input from user or "default"
    keys = Keys.input(:timed => true) || "0"
    bookmark_set "#{prefix}#{keys}"

    # Append to bookmark file
    log_path = "/tmp/bookmark_log.notes"
    if File.exists?(log_path)
      File.open(log_path, "a") { |f| f << "| $#{prefix}#{keys}\n#{View.path}\n\n" }
    end
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
      bookmark_jump(bookmark.sub(/^\$/, ""))
      return
    end

    # Use input from user or "default"
    keys = Keys.input(:timed => true, :prompt => "Enter bookmark to jump to: ") || "0"

    # Open file or jump to if already open
    path = bookmark_get_filename( "#{prefix_to_bm}#{keys}" )
    if path.nil?
      message("no path found")
      return
    end

    case Keys.prefix
    when :u  # If u, go to point
      bookmark_jump "#{prefix_to_bm}#{keys}"
    when 9  # If 9, open in bar
      View.bar
      bookmark_jump "#{prefix_to_bm}#{keys}"
    else
      Location.go path, :stay_in_bar => true
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
    if options[:just_bookmark]  # If only a bookmark, just expand it
      return bookmark_get_filename(path)
    end

    # If $xxx found
    if path =~ /\$([._a-zA-Z0-9-]+)([\\\/]?)(.*)/
      bm, slash, rest = $1, $2, $3

      # Expand bookmark
      bm = bookmark_get_filename(bm)

      # If a slash, cut off filename if there is one (only dir is wanted)
      if slash != ""
        bm.sub!(/[^\\\/]+$/, "")
      end

      path = "#{bm}#{rest}"

      # Expand ~/ if it has it
      path = File.expand_path(path) if path =~ /^~/

      path

    elsif path =~ /^~/  # If home dir, expand
      # Expand ~/ if it has it
      File.expand_path(path)

    elsif options[:absolute] || path =~ /^\.\//  # If relative path, expand
      File.expand_path(path)
    else
      path
    end
  end

  # Insert $bookmark into to path if it contains a bookmarked path
  def self.collapse path

    %w[a tr p n].each do |bm|
#    %w[mo vi co ap tr p].each do |bm|
      find = bookmark_get_filename(bm)
      next unless find
      find.sub!(/[^\/]+$/, "")
      next unless path =~ /^#{find}/
      return path.sub(find, "$#{bm}/")
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
      - .tree/
      - .list/
      "
  end

  def self.list bookmark=nil
    unless bookmark   # Print all bookmarks
      all = elvar.bookmark_alist.collect { |bm|
        item = bm.to_a
        [item[0], item[1].to_a[0][1]]
      }
      all.sort.each do |l|
        n, p = l
        puts "- #{n}: #{p}"
      end
      return
    end

    # Open single bookmark
    View.open bookmark
  end

  def self.tree
    paths = elvar.bookmark_alist.collect {|bm|
      bm.to_a[1].to_a[0][1]
    }.select{|path| path =~ /^\/.+\w$/}

    puts CodeTree.tree_search + TreeLs.paths_to_tree(paths)
  end
end

$el.el4r_lisp_eval("(require 'bookmark)")
$el.bookmark_maybe_load_default_file
