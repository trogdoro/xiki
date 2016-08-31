require 'xiki/core/location'
require 'xiki/core/keys'

require 'xiki/core/files'

module Xiki
  class Bookmarks

    def self.menu
      %`
      - .list/
      - .tree/
      - .options/
        - .persist/
      - .api/
        | > Summary
        | Some of the common ways to use the Bookmarks class programatically.
        |
        | > Get path for a bookmark
        =p Bookmarks["^n"]
        |
        | > Expand bookmark in a path
        =p Bookmarks["^tm/hi.txt"]
        |
        > Where Emacs stores bookmarks
        =~/.emacs.bmk
        |
        - .elisp/
          | > Save unsaved bookmarks to ~/.emacs.bmk
          | (bookmark-save)
          |
          | > Delete a bookmark
          | (bookmark-delete "foo")
      - docs/
        > Summary
        | Xiki uses standard emacs bookmarks, while adding several keyboard shortcuts
        | and the ability to bookmark buffers in addition to just files.
        |
        > Keys
        | All of these prompt for a bookmark name:
        |
        | as+bookmark: bookmark the current file
        | open+bookmark: jumps to bookmark file
        | open+point: jumps to bookmark file and cursor position
        |
        > See Also
        =files/docs/
      `
    end

    def self.save arg=nil
      in_a_file_tree = FileTree.handles? rescue nil

      # Cursor is on a file in a tree, so use it (unless C-u over-rides it)...

      if ! Keys.prefix_u? && in_a_file_tree && ! Line[/^ *\|/]

        path = Tree.construct_path

        keys = Keys.input(:timed=>true, :prompt=>"Name of bookmark for #{path.sub(/.+\/(.)/, "\\1")}: ") || "0"

        self.set keys, :file=>path
        View.flash "- bookmarked as: ^#{keys}", :times=>3
        return
      end

      # arg is a symbol, so use it as the prefix...

      prefix = ""
      if arg && arg.class == Symbol
        prefix = arg.to_s
      elsif arg && arg.class == String
        self.set(arg.sub(/^\:/, ""))
        return
      end

      # Append to bookmark file...

      keys = Keys.input(:prompt=>"Name of bookmark for #{View.file_name}: ") || "0"
      self.set "#{prefix}#{keys}"

    end

    def self.[]= bookmark, path
      $el.with(:save_window_excursion) do
        View.open path
        Bookmarks.set bookmark
      end
    end

    # Lower level method > saves actual bookmark
    def self.set name, options={}

      file = options[:file] || View.file

      dir = File.expand_path "~/.xiki/bookmarks/"

      FileUtils.mkdir_p dir

      File.open("#{dir}/#{name}.xiki", "w") { |f| f << "#{file}\n" }

      # Handle bookmarks for buffers?
      # Format of file?
      #   | buffers/foo

    end

    # If string passed, go to file+point of bookmark.
    # If nothing passed, go to file of user-prompted text
    # If no args passed, get bookmark from user and jump there
    def self.go bookmark=nil, options={}

      # If arg is a symbol, use it as the prefix
      prefix_to_bm = ""
      if bookmark && bookmark.class == Symbol
        prefix_to_bm = bookmark.to_s
      elsif bookmark && bookmark.class == String
        keys = bookmark
      end

      # Use input from user or "default"
      keys ||= Keys.input(:timed=>true, :prompt=>"Type ., /, ~, or a bookmark: ") || "0"

      # User typed "-" or ",", so jump to "continue here" or "- implement!" link
      if keys == "," || keys == "-"
        View.open "^links"
        Move.top

        target = keys == "-" ? "- implement!" : "continue here"
        Search.forward target, :beginning=>true

        Line.next if Line !~ /^ *:/
        Launcher.launch
        return ""
      elsif keys == "\e"   # They typed escape
         return

      # Non-word, so delegate to FileTree.tree, since it knows how to hilight the current file

      elsif keys =~ /^[.\/]+$/
        keys.sub! /^\./, ".."   # Add another dot, so it shows the dir, not the file
        return FileTree.tree :bm=>keys
      end

      # User typed "foo/", so treat as command

      # User input included punctuation (eg "foo/" or ":foo"), so expand it unaltered...

      if keys =~ /[^a-z0-9]/i
        View.to_buffer View.unique_name("untitled.xiki")
        Notes.mode
        View >> "#{keys}\n\n\n"

        Launcher.launch
        return ""
      end

      path = keys =~ /^\w/ ?
        Bookmarks["^#{prefix_to_bm}#{keys}", :raw_path=>1] :
        keys

      return View.open "> Error:\n:-Bookmark #{path} doesn't exist" if path =~ /^\^/

      prefix = Keys.prefix

      # If up+, open bookmark as menu
      if prefix == :u
        Launcher.open "^#{keys}//" #, :unified=>1
        return
      elsif prefix == :uu   # Jump up one level if up+up+
        path = File.dirname path
      end

      View.open path, options.merge(:stay_in_bar=>1)
    end

    # Expand $foo paths in strings.  Expects strings to bee bookmark names,
    # and replaces with corresponding paths.
    def self.[] path, options={}
      self.expand(path, options)
    end

    def self.bookmarks_required bm=nil
      return @@bookmarks_required if ! bm
      @@bookmarks_required[bm]
    end

    @@bookmarks_required = {
      "n"=>"~/xiki/notes.xiki",
      "x"=>"~/xiki/",
      "home"=>"~/",
      "xiki"=>Xiki.dir,
      "links"=>"~/xiki/links.xiki",
      "source"=>Xiki.dir,
      "xs"=>Xiki.dir,
      "xn"=>Xiki.dir,
    }

    # Bookmarks.bookmarks_optional "t"
    # Bookmarks.bookmarks_optional "herring"
    def self.bookmarks_optional bm=nil
      return @@bookmarks_optional if ! bm
      @@bookmarks_optional[bm]
    end

    @@bookmarks_optional = {
      "l"=>"~/xiki/links.xiki",
      "t"=>"~/xiki/",

      "s"=>Files.tilde_for_home(Xiki.dir),
      "h"=>"~/",

      "d"=>"~/Desktop/",
      "r"=>"/",
      "tm"=>"/tmp/",
      "et"=>"/etc/",
      "dx"=>"~/.xiki/",
      "b"=>"~/.xiki/bookmarks/",
      "dl"=>"~/Downloads/",

      "br"=>"~/.bashrc",
      "de"=>"~/.emacs",

      "fa"=>"~/.xiki/misc/favorite/topics.xiki",

      "i"=>"~/.xiki/interactions/",
      "links"=>"~/xiki/links.xiki",

      "ro"=>"~/.xiki/roots/",
      "w"=>"~/xiki/",

      "co"=>"~/.xiki/roots/conf/",
      "co2"=>"#{Xiki.dir}roots/conf/",

      "q"=>"~/xiki/quick.xiki",
      "qq"=>"~/xiki/quick2.xiki",
      "ti"=>"~/xiki/timer.xiki",

      "xc"=>"#{Xiki.dir}commands",
      "c2"=>"#{Xiki.dir}commands",

      "cp"=>"#{Xiki.dir}misc/core_patterns.rb",

      "ir"=>"#{Xiki.dir}misc/emacs/el4r/init.rb",
      "th"=>"#{Xiki.dir}misc/themes/",
      "li"=>"#{Xiki.dir}lib/xiki/",
      "xs"=>"#{Xiki.dir}spec/",

      "m"=>"~/.xiki/misc/",
      "v"=>"~/.xiki/misc/versions/",
      "lo"=>"~/.xiki/misc/logs/",
      "diff"=>"~/.xiki/misc/logs/difflog.xiki",
      "yo"=>"~/.xiki/misc/yours.rb",
      "cl"=>"~/.xiki/misc/logs/xiki_commands_log.xiki",
      "tt"=>"~/.xiki/misc/topic_top/",
      "pi"=>"~/Pictures/",
      "k"=>"#{Xiki.dir}lib/xiki/core/key_shortcuts.rb",
      "us"=>"/usr/",
      "lo"=>"/usr/local/",
      "ap"=>"/Applications/",
      "vo"=>"/Volumes/",
    }

    # Looks up bookmark in ~/.xiki/bookmarks
    # Bookmarks.lookup "p"
    def self.lookup path
      file = File.read(File.expand_path("~/.xiki/bookmarks/#{path}.xiki")).strip
      file
    rescue Exception=>e
      # It looks for a file and falls back to optional bookmarks if not found
      nil
    end

    # Expands bookmark to file
    def self.expand path, options={}
      # If ~/..., /..., or ./..., no need to look up bookmark...

      if path =~ /^~\// || options[:absolute] || path =~ /^\.+(\/|$)/
        # Expand ~/ if it has it
        return View.expand_path(path, options)
      end

      # Just bookmark string passed, so just expand it...

      if options[:just_bookmark]
        bm, slash, rest = path, nil, nil
      elsif path =~ /^\^([._a-zA-Z0-9-]+)([\\\/]?)(.*)/   # Path starts with $xxx, so pull it out and look it up...
        bm, slash, rest = $1, $2, $3

      # Remove this
      # Just leave in for a bit, to catch deprecated $foo... paths
      elsif path =~ /^\$[._a-zA-Z0-9-]+(\/|$)/   # Path starts with $xxx, so pull it out and look it up...
        Ol.stack 7
        raise "Don't use $..., use ^... > #{path}!"

      else   # If no $..., just return the literal path...
        return path
      end

      found = self.bookmarks_required bm
      found ||= self.lookup bm
      found ||= self.bookmarks_optional bm

      if found.nil?
        return nil if options[:just_bookmark]
        return path
      end
      # Recursively run if still has $ after lookup...

      found = self.expand found if found =~ /^\^/

      if ! options[:raw_path] && found =~ /^~/
        had_slash = found =~ /\/$/
        found = File.expand_path found
        found = "#{found}/" if had_slash
      end


      if slash.any? || rest.any?
        found.sub! /\/$/, ""
        found << "/"
      end
      "#{found}#{rest}"

    end

    # Remove file from the end (if not dir).
    # Bookmarks.dir_only "/tmp/foo.txt"
    #   /tmp
    # Bookmarks.dir_only "/tmp/"
    #   /tmp/
    #
    # Similar to ruby's File.dirname, but which doesn't recognize dir paths:
    # File.dirname "/tmp/a/"
    #   /tmp
    def self.dir_only path, options={}
      path = path.sub(/\/[^\/]+\.[^\/]+$/, '')
      # return path if options[:no_slash]

      if options[:with_slash]
        path << "/" unless path =~ /\/$/
      end

      path

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

    def self.tree

      raise "This is cool, so maybe do this with new :b dir"

      paths = $el.elvar.bookmark_alist.collect {|bm|
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

      Tree.paths_to_tree(paths)
    end

    def self.open_quick
      bookmark = Keys.input :timed=>1, :prompt=>"Enter a quick bookmark to jump to:"
      self.go("q#{bookmark}")
    end

    # Bookmarks.bookmarkify_path "/Users/craig/.xiki/misc/logs/"
    def self.bookmarkify_path path

      # Expand out tilde in path if not there, since paths in bookmarks have tildes

      path = Files.tilde_for_home path
      bookmarks = self.bookmarks_required

      self.bookmarks_required.each do |bookmark, dir|
        return path.sub(/^#{dir}/, "^#{bookmark}/") if path =~ /^#{dir}/
      end

      path

    end

  end

end

