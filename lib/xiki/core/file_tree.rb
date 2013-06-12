# -*- coding: utf-8 -*-

require 'xiki/core/styles'
require 'xiki/core/line'
require 'xiki/core/view'
require 'net/http'
require 'uri'
require 'xiki/core/cursor'

module Xiki
  # Draws a tree from a dir structure and lets you incrementally search in the tree.
  # Usage (user):
  #   - Type C-x C-t
  #     - A tree will be drawn in a new buffer
  #       - Representing the directory structure in the current dir
  #   - Type Enter to open a file
  #     - Navigate within the buffer
  #     - Then press Enter when you're on the file you wish to open
  # Usage (api):
  #  - Call this to draw a tree from the current directory
  #    - FileTree.ls
  class FileTree

    @@one_view_in_bar_by_default = false

    def self.menu
      %`
      - docs/
        | > Summary
        | Lets you navigate your filesystem as a tree.
        |
        | Type a file path on a line and double-click on it. Here's an example
        | (the "@" isn't required when there are no spaces at the beginning of the
        | line).
        |
        @/
        |
      - overview of keys/
        | When you launch a path, the cursor turns blue and you go into a temporary
        | search.  Here are some keys you can type:
        |
        | - Letters: search and filter lines
        |
        | - Return: stops searching and launches (expands file or dir)
        | - Tab: like return but hides others
        | - ;: like return but collapses path
        |
        | - C-g: stops searching
        |
      - api/
        | Turn paths into a tree
        @ puts Tree.paths_to_tree $el.elvar.recentf_list.to_a[0..2]
      `
    end

    # TODO
    # - Make search handle trees with multiple roots

    # Call this method from your init.rb to use the default key shortcuts.
    def self.keys
      Keys.XT { FileTree.ls }
    end

    def initialize
      @res = ""
      @list = []
      @file_regex = nil
    end
    attr :res
    attr :list
    attr_accessor :file_regex

    # Change dirs into spaces, etc
    def clean path, indent=0
      path = path.gsub(/.+?\//, "  ")
      indent == 0 ?
        path :
        path.sub(/^#{indent}/, '')
    end

    # Recursively draws out tree
    def traverse path
      entries = Dir.glob("#{View.expand_path(path)}/*", File::FNM_DOTMATCH).
        select {|i| i !~ /\/\.(\.*|svn|git)$/}.   # Exclude some dirs (exclude entensions here too?)
        sort

      # Process dirs
      entries.each{ |f|
        next unless File.directory?(f)
        cleaned = clean f
        @res += "#{cleaned.sub(/(^ *)/, "\\1- ")}/\n"
        traverse f
      }

      # Process files
      entries.each{ |f|
        next unless File.file?(f)
        cleaned = clean f
        @res += "#{cleaned.sub(/(^ *)/, "\\1+ ")}\n"
      }

    end

    def self.grep dir, regex, options={}
      # args when ## => ["/projects/foo/", "hey", {}]
      # args when ** => ["/projects/foo/", nil, {:files=>"index"}]

      # Expand out bookmark (if there is one)
      dir = Bookmarks.expand(dir)
      dir = Bookmarks.dir_only dir  # Cut off file (if there is one)
      dir.sub!(/\/$/, '')  # Remove slash from end

      # Turn regex into a regex, if a string
      regex = Regexp.new(regex, Regexp::IGNORECASE) if regex.is_a? String

      @@indent_count = dir.count('/') - 1
      @@indent = "  " * @@indent_count
      t = self.new

      files = options[:files]
      if files
        files = Regexp.new(files, Regexp::IGNORECASE) if files.is_a? String
        t.file_regex = files
      end
      t.list << "- #{dir}/"
      t.grep_inner dir, regex, :first_time

      list = t.list
      Tree.clear_empty_dirs! list

      list

    end

    def self.grep_with_hashes path, regex, prepend='##'
      Search.append_log path, "- #{prepend}#{regex}/"

      View.to_buffer "*tree grep"
      View.dir = Files.dir_of(path)
      View.clear; Notes.mode

      if File.directory?(path)
        View << "- #{File.expand_path path}/\n  - #{prepend}#{regex}/\n"
      else
        dir, name = path.match(/(.+\/)(.+)/)[1..2]
        View << "- #{File.expand_path dir}/\n  - #{name}\n    - #{prepend}#{regex}/\n"
      end

      View.to_bottom; Line.previous
      self.launch
    end

    def self.grep_one_file(f, regex, indent)
      result = []

      return result if f =~ /\.(ai|icns|png|gif|jpg|gem|ttf)$/

      IO.foreach(f, *Files.encoding_binary) do |line|
        #       line.gsub!(/[\r\n\c@]+/, '')
        line.chomp!
        if regex
          next unless line =~ regex
        end
        result.<< line == "" ?
          "#{indent}|" :
          "#{indent}| #{line}"
      end
      result
    end


    # Used by new Unified code.
    # Searches through a single file for a pattern.  If no pattern, defaults
    # to the pattern appropriate for the file extension.
    #
    # FileTree.filter_one_file "/tmp/foo.txt", regex, indent, options={}


    def self.filter_one_file file, regex=nil, options={}

      # Default pattern based on file extension
      regex ||= /#{self.outline_pattern File.extname(file).sub(".", '')}/

      indent = options[:indent] || ""

      result = []

      # If search_outline, we want to put cursor on that line when done
      # Remove .outline_goto_once part of line after Unified refactor is done

      current_line = options[:current_line] || Search.outline_goto_once

      line_found, matches_count, i = nil, 0, 0
      IO.foreach(file, *Files.encoding_binary) do |line|
        i+=1
        line.gsub!(/[\r\n\c@]+/, '')

        if current_line && line_found.nil?
          line_found = matches_count if i == current_line
        end

        if regex
          next unless line =~ regex
        end

        # Don't show space after quote for blank lines.
        line.sub! /^ > $/, ' >'
        line = line == "" ? "" : " #{line}"

        result << "#{indent}|#{line}"
        matches_count+=1
      end

      if line_found   # If line we were on was found, remember it so we can hililght it when we show the outline
        options[:line_found] = line_found
        Search.outline_goto_once = line_found   # Remove after Unified refactor
      end
      result
    end

    def self.skip
      @skip || {}
    end

    def grep_inner path, regex, first_time=nil

      path.sub!(/\/$/, '')
      entries = Dir["#{path}/*"].entries.sort

      entries = entries.select{|o| o !~ /\/(vendor|log)$/}   # Exclude some dirs (why doing it here?

      # Exclude some file extensions and dirs
      # Make these be in config!  pass along as instance var
      # TMP:::: Hard-code skipping image/ dirs (for now) !!!!!!!!!!!!!
      entries = entries.select{|o| o !~ /\/(images|twilio_sound)$/}

      if first_time and skip = FileTree.skip[path]
        entries = entries.select{|o| ! skip.member? o[/.+\/(.+)/, 1]}
      end

      # Process dirs
      entries.each{ |f|
        next unless File.directory?(f)
        cleaned = clean(f, @@indent)
        @list << "#{cleaned.sub(/(^ *)/, "\\1- ")}/"
        grep_inner f, regex
      }

      indent = nil

      # Process files
      entries.each do |f|
        next unless File.file?(f)

        # If matching filename, skip if no match
        if file_regex
          stem = f[/[^\/]+$/]
          next unless stem =~ file_regex
        end

        indent = "  " * (f.count('/') - @@indent_count) unless indent

        if regex
          result = FileTree.grep_one_file(f, regex, indent)   # Search in file contents

          if result.size > 0   # Add if any files were found
            @list << clean(f, @@indent).sub(/(^ *)/, "\\1- ")
            @list += result
          end
        else
          @list << clean(f, @@indent).sub(/(^ *)/, "\\1+ ")
        end
      end

    end

    # Does ls in current buffer, without making any modifications to the environment
    def self.ls_here dir
      dir ||= View.dir
      t = self.new
      dir.sub!(/\/$/, '')
      t.traverse dir
      View.insert "#{dir}/\n"

      result_indent = t.res[/^ */]   # Get indent of first line
      View.insert t.res.gsub(/^#{result_indent}/, "  ")
    end

    def self.define_styles

      return if ! $el

      if Styles.dark_bg?   # Bullets
        Styles.define :ls_bullet,
          :face => 'courier', :size => "+2",  # Mac
          :fg => "dd7700", :bold => true
      else
        Styles.define :ls_bullet,
          :face => 'courier', :size => "+2",  # Mac
          :fg => "ff7700", :bold => true
      end


      if Styles.dark_bg?
        Styles.define :quote_heading_h0, :fg=>"fff", :size=>"+8", :face=>"arial", :bold=>true
        Styles.define :quote_heading_h1, :fg=>"fff", :size=>"2", :face=>"arial", :bold=>true
        Styles.define :quote_heading_h2, :fg=>"fff", :size=>"-2", :face=>"arial", :bold=>true
        Styles.define :quote_heading_pipe, :fg=>"444", :size=>"0", :face => "xiki", :bold=>true
        Styles.define :quote_heading_h1_green, :fg=>"8f4", :size=>"2", :face=>"arial", :bold=>true

        Styles.define :quote_heading_bracket, :fg=>"4c4c4c", :size=>"-2", :face=>"arial black", :bold=>true
        Styles.define :quote_heading_small, :fg=>"fff", :size=>"-2", :face => "arial black", :bold=>true

        Styles.define :diff_line_number, :bold => true, :size => "-2", :fg => "444444"
        Styles.define :diff_red, :bg=>"400", :fg=>"ee3333", :size=>"-1"
        Styles.define :diff_red_pipe, :bg=>"400", :fg=>"771111", :size=>"0", :face=>"xiki", :bold=>true

        Styles.define :diff_green, :bg=>"130", :fg=>"44dd33", :size=>"-1"
        Styles.define :diff_green_pipe, :bg=>"130", :fg=>"117719", :size=>"0", :face=>"xiki", :bold=>true

        Styles.define :diff_small, :fg => "222", :size => "-11"

        Styles.tree_keys :fg=>"#fff", :underline=>nil

        # dir/
        Styles.define :ls_dir, :fg => "888", :face => "verdana", :size => "-1", :bold => true

      else
        Styles.define :quote_heading_h0, :fg=>"444", :size=>"+8", :face=>"arial", :bold=>true
        Styles.define :quote_heading_h1, :fg=>"444", :size=>"2", :face=>"arial", :bold=>true
        Styles.define :quote_heading_h2, :fg=>"aaa", :size=>"-2", :face=>"arial", :bold=>true
        Styles.define :quote_heading_pipe, :fg=>"bbb", :size=>"0", :face => "xiki", :bold=>true
        Styles.define :quote_heading_h1_green, :fg=>"8f4", :size=>"2", :face=>"arial", :bold=>true

        Styles.define :quote_heading_bracket, :fg=>"bbb", :size=>"-2", :face=>"arial black", :bold=>true
        Styles.define :quote_heading_small, :fg=>"fff", :size=>"-2", :face => "arial black", :bold=>true

        Styles.define :diff_line_number, :bold=>true, :size=>"-2", :fg=>"ccc"
        Styles.define :diff_red, :bg=>"ffdddd", :fg=>"cc4444", :size=>"-1"
        Styles.define :diff_red_pipe, :bg=>"ffdddd", :fg=>"cc4444", :size=>"0", :face=>"xiki", :bold=>true
        Styles.define :diff_green, :bg=>"ddffcc", :fg=>"337744", :size=>"-1"
        Styles.define :diff_green_pipe, :bg=>"ddffcc", :fg=>"337744", :size=>"0", :face=>"xiki", :bold=>true
        Styles.define :diff_small, :fg=>"ddd", :size=>"-11"

        Styles.tree_keys :fg=>"#000", :underline=>1

        # dir/
        Styles.define :ls_dir, :fg => "777", :face => "verdana", :size => "-1", :bold => true

      end

      Styles.define :quote_medium, :size=>140, :fg=>"666", :bold=>1

      # ##search/
      Styles.define :ls_search,
        :fg => "ff7700",
        :face => "verdana",
        :size => "-2",
        :bold => true

      if Styles.dark_bg?   #   | Quoted text
        Styles.define :ls_quote,
          :size => "-1",
          :fg => "aaa"
      else
        Styles.define :ls_quote,
          :size => "-1",
          :fg => "777"
      end

      #   001| Quoted text lines
      Styles.define :ls_quote_line_number,
        :size => "-4",
        :fg => "eee"

      # Highlight in search
      Styles.define :ls_quote_highlight,
        :size => "-1",
        :bg => "ffff44",
        :fg => "666666"
    end

    def self.apply_styles
      $el.el4r_lisp_eval "(setq font-lock-defaults '(nil t))"

      # Must go before quotes - if it goes after, it supercedes them
      Styles.apply("\\(~\\)\\(.+?\\)\\(~\\)", nil, :quote_heading_bracket, :notes_label, :quote_heading_bracket)

      #     Styles.apply("\\(https?\\|file\\)://[a-zA-Z0-9\/.~_:;,-]+", :notes_link)   # blue-ify url's

      # - bullets
      Styles.apply("^[ \t]*\\([+=-]\\)\\( \\)", nil, :ls_bullet, :variable)

      # With numbers
      Styles.apply("^ +\\(:[0-9]+\\)\\(|.*\n\\)", nil, :ls_quote_line_number, :ls_quote)

      # Path-like lines and parts of lines (make gray)

      # Single "@" or bullets at beginning
      Styles.apply("^[ <+=@-]*\\(@\\)", nil, :ls_dir)   # @, <= @, etc.

      Styles.apply("^[ \t]*\\(<+=?@?\\)\\( \\)", nil, :ls_bullet, :variable)

      # Remove later?
      #     Styles.apply("^[ <+=@-]*\\([^|\n]+/\\)$", nil, :ls_dir)  # slash at end

      # Slash after almost anything

      # foo/ lines
      Styles.apply("^[ <=+-]*@?\\([~$&#a-zA-Z0-9_,?* ().:;@'-]*[^ \n]\/\\)", nil, :ls_dir)

      # Covers paths in files by themselves
      Styles.apply("^[ <=+-]*\\([@~$&a-zA-Z0-9_,*? ().:;<-]*\/[@\#'$a-zA-Z0-9_,*? ().:;\/<-]+\/\\)", nil, :ls_dir)   # Paths with multiple slashes




      # < next) menus/
      Styles.apply("^[ \t]*[<+-][<+=-]* [a-zA-Z0-9_,? ().:;-]+?[:)] \\(\[.@a-zA-Z0-9 ]+\/\\)", nil, :ls_dir)   # label, one word, slash
      Styles.apply("^[ \t]*[<+-][<+=-]* [a-zA-Z0-9_,? ().:;-]+?[:)] \\([.@a-zA-Z0-9 ]+\/[.@a-zA-Z0-9 \/]+\/\\)", nil, :ls_dir)   # label, one word, path, slash



      # Bullets
      Styles.apply("^[ \t]*[+-] [^(\n]+?) \\(.+/\\)$", nil, :ls_dir)   # - hey) /what/
      Styles.apply("^[ \t]*[+-] [a-zA-Z0-9_,? ().:;-]+?: \\(.+/\\)$", nil, :ls_dir)   # - hey: /what/

      # Put this one back?
      #     Styles.apply("^[ +-]*\\([^|\n]+/\\)$", nil, :ls_dir)   # Dirs with bullets

      Styles.apply('\\(https?\\|file\\):/[a-zA-Z0-9\/.~_:;,?%&=|+!-#-]+', :notes_link)   # Url


      # :... lines (quotes)
      Styles.apply("^ *\\(:\\)\\($\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote)


      # |... lines (quotes)

      # Is this one adding anything?
      #     Styles.apply("^ *\\(|\\)\\( *\\)", nil, :quote_heading_pipe, :ls_quote)

      Styles.apply("^ *\\(|\\)\\(.*\n\\)", nil, :quote_heading_pipe, :ls_quote)

      Styles.apply("^ *\\(|\\)\\(.+?\\)([+-].*[-+])", nil, :quote_heading_pipe, :ls_quote)   # quoted lines: beginnings of lines
      Styles.apply("^ *|.*([-+].*[+-])\\(.+\\)$", nil, :ls_quote)   # quoted lines: ends of lines
      Styles.apply("[+-])\\(.*?\\)([+-]", nil, :ls_quote)   # quoted lines: between diffs

      # | >... headings

      Styles.apply("^ *\\(|\\|:\\)\\( ?\\)\\(>\\)\\(\n\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading_h1)

      Styles.apply("^ *\\(|\\)\\( \\)\\(>>\\)\\(\n\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading_small)
      # | >...! headings
      Styles.apply("^ *\\(|\\)\\( ?\\)\\(>\\)\\(.*!\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading_h1_green)

      #    >... headings (indented)
      Styles.apply("^ +\\(> ?\\)\\(.*\n\\)", nil, :quote_heading_bracket, :quote_heading_h1)
      Styles.apply("^ +\\(> ?\\)\\(.*!\n\\)", nil, :quote_heading_bracket, :quote_heading_h1_green)

      #    >>
      Styles.apply("^ +\\(>>\\)\\(.*\n\\)", nil, :quote_heading_bracket, :quote_heading_h2)
      Styles.apply("^ +\\(> ?\\)\\(.*:\n\\)", nil, :quote_heading_bracket, :quote_heading_h0)

      # |+... diffs
      Styles.apply("^ +\\(:[0-9]+\\)$", nil, :ls_quote)
      Styles.apply("^ *\\(|\\+\\|:\\+\\)\\(.*\\)", nil, :diff_green_pipe, :diff_green, :face=>"xiki")   # whole lines

      Styles.apply("^ *\\(|-\\|:-\\)\\(.*\\)", nil, :diff_red_pipe, :diff_red)

      Styles.apply("^ *\\(|\\)\\(@@ .*\n\\)", nil, :quote_heading_pipe, :diff_line_number)

      #Styles.apply('^[ -]*\\([ a-zA-Z0-9\/_\.$-()]*\\w/\\)$', nil, :ls_dir)  # Most dirs ? Has ")" at end
      Styles.apply('^ *\\(//?\\)$', nil, :ls_dir)  # /
      Styles.apply('^ *\\(\./\\)$', nil, :ls_dir)  # ./
    end

    def self.apply_styles_at_end
      Styles.apply('^ *[+-] \\(##.*/\\)$', nil, :ls_search)  # ##_/
      Styles.apply('^ *\\([+-] \\)?\\(@f/.*/\\)$', nil, nil, :ls_search)  # ##_/
      Styles.apply('^ *[+-] \\(\*\*.*/\\)$', nil, :ls_search)  # **_/
      Styles.apply('^ *\\([+-] \\)?\\(@n/.*/\\)$', nil, nil, :ls_search)  # ##_/
    end

    def self.handles? list=nil

      if list.nil?
        list ||= Tree.construct_path(:list=>true) rescue nil   # Use current line by default
      end

      list = list.first if list.is_a?(Array)

      return 0 if self.matches_root_pattern?(Line.without_label :line=>list)
      nil
    end

    def self.matches_root_pattern? item
      item =~ /^\/(\w|$)/ ||   # /... or /
      item =~ /^\$\w/ ||
      item =~ %r"^(~/|\.\.?/|\$\w)"
    end

    # Remove this after Unified refactor?
    #
    # Open the line in the tree that the cursor is on.  This is probably
    # be mapped to C-. .
    # TODO: remove ignore_prefix, and just use Keys.clear_prefix
    def self.open options={}
      # Ol["deprecated after Unified refactor"]
      # raise "deprecated after Unified refactor - shouldn't be used!"

      # If passed just a dir, open it in new view
      return self.ls :dir=>options if options.is_a? String

      original_file = View.file_name

      path = options[:path] || Tree.construct_path(:list=>true)
      path_orig = path

      # Split off quoted |... string if there...

      if path.is_a?(Array)
        quote_string = path.pop[/\|(.*)/, 1] if path.last =~ /^\|/
        quote_string.sub! /^[ +-]/, '' if quote_string   # Should start with space or -|+
        path = path.grep(/^[^|]/).join('')   # Discard rest of |... lines
      else
        path =~ /(.*?)-?\+?\|(.*)/
        path, quote_string = $1, $2 if $2
      end

      # Pull number off end of path if there
      path =~ /(.+):(\d+)$/
      path, line_number = $1, $2 if $2

      path = Bookmarks.expand(path)

      return Files.open_in_os(path) if Keys.prefix == 0

      remote = self.is_remote?(path)
      unless remote
        path = File.expand_path(path)
      end
      # Prefix keys with specific behavior
      prefix_was_u = false

      prefix = Keys.prefix

      case prefix
      when :u   # Just open file
        prefix_was_u = true
        Keys.clear_prefix
      when "update"   # Save ($ave) file
        return self.save_quoted path
      when "delete"   # Save ($ave) file
        return self.delete_file
      when "all"
        Keys.clear_prefix
        quote_string ?   # If quote, enter lines under
          Tree.enter_under :
          self.enter_lines(//)   # If file, enter all lines
        return
      else
        if prefix =~ /\boutline\b/
          prefix = Keys.prefix_n
          Keys.clear_prefix
          return self.drill_quotes_or_enter_lines path, quote_string, :prefix=>prefix
        end
      end

      # If numeric prefix, jump to nth window...

      if (! options[:ignore_prefix]) and Keys.prefix_n and Keys.prefix != 7
        # If number larger than number of windows, open new one first
        if Keys.prefix_n > View.list.size
          View.to_nth(View.list.size - 1)
          View.create
        end
        View.to_nth(Keys.prefix_n - 1)
      end

      # If there was a quoted string, remember the column...

      column = View.column
      if quote_string
        column -= (Line.value[/^.*?\|./] || '').length
        column = 0 if column < 0

        ControlTab.dash_prefix_buffer = View.name   # Remember where we were so subsequent 8+Tab can continue on from here
      end


      # Open or go to file...

      if remote
        self.remote_file_contents(path)   # Get text from server and insert

      else   # Normal file opening

        # See if it exists, and store contents if not?

        # TODO: why are we using Location.go - isn't it just delegating to View.open?
          # Does it do extra stuff?  Maybe goes somewhere if dir?
            # Try just calling View.open instead!

        if options[:other_view]
          View.open path, {:other_view=>1}.merge(prefix == :- ? {:no_recenter=>1} : {})
        else
          options[:same_view] ? View.open(path, :same_view=>true) : Location.go(path)
        end
        Effects.blink(:what=>:line) unless line_number || quote_string || path =~ /\.xiki$/
      end

      return unless line_number || quote_string

      if line_number   # If line number, go to it
        $el.goto_line line_number.to_i
        View.column = column
        Effects.blink(:what=>:line)
      elsif quote_string

        # Search for quoted |... string if it passed...

        Hide.reveal if View.hidden?

        found = View.to_quote quote_string

        unless found   # If not found, suggest creating or show error

          return if self.suggest_creating quote_string

          View.beep
          View.message "Didn't find: \"#{quote_string.strip}\"", :beep=>1
          return
        end

        $el.beginning_of_line

        $el.recenter(0) unless prefix_was_u || options[:no_recenter]
        View.column = column

        Effects.blink(:what=>:line)

        dir, name = path.match(/(.+\/)(.+)/)[1..2]

        return if original_file == "search_log.notes"

        # Add to log
        Search.append_log dir, "- #{name}\n    | #{quote_string}"
      end

    end

    def self.suggest_creating quote_string

      if quote_string =~ /^ *def self\.(.+)/   # If it's a method, suggest creating it
        Code.suggest_creating_method View.file, $1
        return true
      end

      return false   # We didn't handle it
    end


    def self.drill_quotes_or_enter_lines path, quote, options={}
      return self.enter_lines(nil, options) if ! quote   # If not quote must be whole file
      result = self.drill_quote path   # Try showing only children with children
      return Tree.<<(result) if result.any?
      return Tree.enter_under   # Only leafs, so show them
    end

    # Deprecated after Unified.
    # Make sure it's not used anywhere, then delete!
    def self.save_quoted path

      txt = Tree.siblings :quotes=>1

      if txt[0] !~ /^\|/
        # TODO: also check that it's a tree
        return self.move_or_delete_via_diffs
      end

      dir = File.dirname path

      if ! File.exists? dir
        View.flash "- Dir doesn\'t exist. Create it?", :times=>4

        key = Keys.input :chars=>1, :prompt=>"Create the \"#{dir}\" directory? "
        return if key != "y"

        `mkdir -p "#{dir}"`
      end

      txt = txt.map{|o| "#{o.sub /^\|.?/, ''}\n"}.join('')

      DiffLog.save_diffs :patha=>path, :textb=>txt

      File.open(path, "w") { |f| f << txt }

      View.flash "- Saved!"
    end

    # New Unified way to save a file
    def self.save path, options

      # If no prefix, prompt to save (file must not exist since we were called)
      if options[:prefix] != "update"
        return if ! View.confirm "Create file #{path} ?"
      end

      txt = Tree.siblings(:quotes=>1, :string=>1)

      # Eventually... do what if client !~ ^editor?

      # Bring this back at some point:
      # This is that thing where you can move or delete files in a file tree, and
      # do as+update to have it look at diff with saved and have it move and delete
      # the actual files accordingly.
      #   # Will probably have to check options[:path] or something
      #     if txt[0] !~ /^\|/
      #       # TODO: also check that it's a tree
      #       return self.move_or_delete_via_diffs
      #     end

      dir = File.dirname path

      if ! File.exists? dir
        View.flash "- Dir doesn\'t exist. Create it?", :times=>4
        return if ! View.confirm "Create the \"#{dir}\" directory?"

        `mkdir -p "#{dir}"`
      end

      DiffLog.save_diffs :patha=>path, :textb=>txt
      File.open(path, "w") { |f| f << txt }

      "@flash/- saved!"

      # - Possible long-term better solution > do later
      #   - if ! options[:prompt_response]
      #     - return @/prompt/Are you sure you want to create?
      #   - if :prompt_response says they're ok with saving, do it
      #     - __

    end

    def self.operations_via_diffs added, deleted
      tmp_path = "/tmp/saved.txt"
      operations = [[], []]

      file = View.file
      $el.with(:with_temp_buffer) do
        $el.insert_file file
        deleted.each do |line_number|
          View.line = line_number
          path = Tree.construct_path
          operations[1] << path
        end
      end

      if added.any?
        $el.with(:with_temp_buffer) do
          $el.insert_file tmp_path
          added.each do |add|
            line_number = add
            View.line = line_number
            path = Tree.construct_path
            operations[0] << path
          end
        end
      end
      operations
    end

    def self.move_or_delete_via_diffs

      # Save current version to temporary place, and diff
      tmp_path = "/tmp/saved.txt"
      $el.write_region nil, nil, tmp_path
      diff = Console.sync %`diff --old-group-format="d%df %dn%c'\012'" --new-group-format="a%dF %dN%c'\012'" --unchanged-line-format="" "#{View.file}" "#{tmp_path}"`

      added, deleted = DiffLog.parse_tree_diffs diff

      return Tree.<<("> Error\n| You haven't changed any lines (since you last saved).", :no_slash=>1) if added.blank? && deleted.blank?

      return Tree.<<("> Message from as+update\n| You\'ve deleted #{deleted.length} lines but added #{added.length} (since you last saved).\n| It\'s unclear what you\'re trying to do.", :no_slash=>1) if added.any? && added.length != deleted.length
      operation = added.any? ? :moves : :deletes

      operations = self.operations_via_diffs added, deleted

      View.flash "- Are you sure?", :times=>1

      if operation == :moves
        message = "Move these files?\n"
        operations[0].each_with_index do |o, i|
          delete = operations[1][i]
          message << "- #{delete} -> #{o}\n"
        end
      else
        message = "Delete these files?\n"
        operations[1].each do |o|
          message << "- #{o}\n"
        end
      end

      choice = Keys.input :prompt=>"#{message}", :chars=>1

      return if choice !~ /[ym]/i

      if operation == :moves
        operations[0].each_with_index do |o, i|
          delete = operations[1][i]
          command = "mv \"#{delete}\" \"#{o}\""
          Console.sync command, :dir=>"/tmp/"
        end
      else
        operations[1].each do |o|
          command = "rm \"#{o}\""
          Console.sync command, :dir=>"/tmp/"
        end
      end

      View.flash "- done!", :times=>1

      nil
    end

    def self.drill_quote path
      parent = Line.value
      parent.sub! /^ *\| /, ''

      found, indent, candidate, result = false, 0, nil, ""
      IO.foreach(path) do |line|
        line = line[/.*/]

        if ! found
          if line == parent
            found = true
            indent = (line[/^ */].length / 2) + 1
          end
          next
        end

        # Found

        # Skip if blank
        next if line.blank?

        current_indent = line[/^ */].length / 2

        # If indented exactly 2 under, add it
        if current_indent == indent
          candidate = "| #{line}\n"

        # If child of candidate, append candidate if one needs appending
        elsif current_indent == indent + 1
          next if candidate.nil?
          result << candidate
          candidate = nil
        # If indented less, stop
        elsif current_indent < indent
          break
        end
      end

      result
    end

    # Goes through files in reverse order and deletes empty dirs
    def self.select_next_file
      $el.search_forward_regexp /[^\/]$/
      $el.beginning_of_line
      $el.skip_chars_forward " "
    end

    def self.select_previous_file
      $el.search_backward_regexp /[^\/]$/
      $el.beginning_of_line
      $el.skip_chars_forward "\t"
    end

    # Draw tree, use dir of bookmark from user input
    def self.ls options={}
      dir = options[:dir]
      # If no dir, do tree in current dir
      dir ||= $el.elvar.default_directory

      line = Line.value

      Line << "\n" if line =~ /^>/   # If it's on a >... line, insert linebreak

      # If to be in bar, go to the tree file in the bar
      if options[:open_in_bar]
        self.open_in_bar :ignore_prefix   # Open tree
        # If not on a blank line, go to the top
        View.to_top unless line =~ /^$/
        # If on line, add another blank
        unless line =~ /^$/
          View.insert "\n\n";  backward_char 2
        end
      end
      dir = View.expand_path(dir)   # Expand out ~
      # If file, back up to dir
      dir = Bookmarks.dir_only dir unless File.directory?(dir)
      # If dir or recursive
      if File.directory?(dir) || options[:recursive]
        dir.sub!(/([^\/])$/, "\\1/")  # Add slash on end if not there
      end

      name = dir[/.+\/(.+)\//, 1]
      # Don't open new dir if not appropriate
      if options[:open_in_bar] || options[:here]
        # Don't upen buffer
      else
        View.to_buffer "*tree #{name}"
        View.clear
        View.dir = Bookmarks.dir_only dir
        Notes.mode
        $el.use_local_map $el.elvar.notes_mode_map
      end
      # If recursive
      if options[:recursive]
        left = $el.point
        self.ls_here dir   # Draw actual tree
        right = $el.point
        $el.goto_char left
        self.select_next_file
        # Start incremental search
        Tree.search(:recursive => true, :left => left, :right => right)
        #self.draw(dir)
      else
        # Insert with linebreak if file
        if File.file?(dir)
          #insert dir
          View.insert self.filename_to_next_line(dir)  # Add linebreak before filename
          #insert dir.sub(/(.+)\//, "\\1/\n  ")  # Add linebreak before filename
          open_line 1
          #Tree.search
          Line.to_words
          Tree.search(:left => Line.left, :right => Line.left(2))
          return
        end
        bullet = ""

        bullet = "" if line =~ /^[ @]+$/
        bullet = "@" if line =~ /^ +$/

        View.insert "#{bullet}#{dir}\n"
        $el.previous_line
        self.dir options   # Draw actual tree
      end
    end


    def self.open_in_bar options={}
      prefix = Keys.prefix :clear=>1

      # If numeric prefix, open nth thing in tree
      if prefix and prefix != :u and !(options[:ignore_prefix])
        View.flash "- Don't know what this was supposed to do!"

        # Remember original view
        #       start = $el.selected_window
        # Open tree (ignoring prefix)
        #       self.open_in_bar# :ignore_prefix=>true
        # Find nth file in tree
        #       View.to_highest

        #       prefix.times do
        #         re_search_forward "^  +[a-zA-Z0-9_.-]+$"
        #       end
        # Go to next line if comment

        #       Line.next if Line.next_matches(/^ *\|/)
        #       Line.to_beginning
        #       self.open :ignore_prefix=>true
        return
      end

      unless View.bar?   # If already open, just go there
        View.bar
      end
      View.to_nth 0
      $el.find_file Bookmarks.expand("$t")

      only_one_view_in_bar = prefix == :u
      only_one_view_in_bar = ! only_one_view_in_bar if @@one_view_in_bar_by_default

      unless only_one_view_in_bar  # Unless u prefix, open $tl as well (under bar)

        # If 2nd view isn't at left margin, open 2nd view
        if View.left_edge(View.list[1]) != 0
          View.create
        end

        View.to_nth 1

        # If 2nd view isn't $f, open it
        if $el.buffer_file_name( $el.window_buffer( View.list[1] ) ) != Bookmarks["$f"]
          $el.find_file Bookmarks["$f"]
        end

        View.to_nth 0

      end
    end

    # Creates tree snippet of text in file
    def self.snippet options={}
      txt = options[:txt] || View.selection
      file = options[:file] || View.file

      # Remove linebreak from end
      txt = txt.sub(/\n\z/, "")
      if file
        txt = "#{File.dirname(file)}/\n  - #{File.basename(file)}\n" +
          txt.gsub(/^/, "    | ").
          gsub(/^    \| $/, "    |")   # Remove trailing spaces on blank lines
        "#{txt}\n"
      else
        #       "- From #{buffer_name}:\n" + txt.gsub(/^/, "    #")
        "- From #{View.name}:\n" + txt.gsub(/^/, "  | ")
      end
    end

    # Recursively display dir in tree   # Insert dir contents at point (usually in existing tree)
    def self.dir options={}
      return Files.open_in_os(Tree.construct_path) if Keys.prefix == 0

      Tree.plus_to_minus_maybe
      Line.to_left
      prefix = Keys.prefix
      if prefix == 8 || prefix == "all"
        self.dir_recursive
      elsif prefix == "delete"
        return self.delete_file
      else
        self.dir_one_level options
     end
    end

    # Insert all files in dirs within a dir, and allow user to
    # incremental search therein.
    def self.dir_recursive

      $el.beginning_of_line
      line = Line.value
      left = $el.point
      indent = line[/^ */] + "  "  # Get indent
      dir = Tree.construct_path

      Line.next

      # Get tree
      t = self.new
      dir = Bookmarks.expand(dir)
      dir.sub!(/\/$/, '')
      t.traverse dir

      # Adjust indent
      result_indent = t.res[/^ */]   # Get indent of first line
      View.insert t.res.gsub(/^#{result_indent}/, indent)

      right = $el.point

      $el.goto_char left
      #    isearch_forward
      self.select_next_file
      Tree.search(:recursive => true, :left => left, :right => right)
    end

    # New Unified way to show list of dirs.
    def self.expand_dir_recursively options

      dir = options[:file_path]

      indent = ""

      # Get tree
      t = self.new
      dir.sub!(/\/$/, '')
      t.traverse Bookmarks.expand(dir)

      # Adjust indent
      result_indent = t.res[/^ */]   # Get indent of first line
      txt = t.res.gsub(/^#{result_indent}/, "")

      return txt
    end



    def self.dir_one_level options={}

      Line.to_left
      line = Line.value
      indent = line[/^ */] + "  "  # Get indent

      dir = Bookmarks.expand(Tree.construct_path)

      remote = self.is_remote?(dir)
      unless remote
        # If C-2, just open in dired
        if Keys.prefix == 2
          # TODO: Open in 1st window
          View.to_after_bar
          $el.find_file dir
          return
        end
      end

      dirs, files = self.files_in_dir(dir, options)   # Get dirs and files in it

      if files.empty? && dirs.empty?
        if ! File.exists? dir   # If doesn't exist, show message
          return Tree << "
            - @mkdir/
            | ...dir '#{dir}' doesn't exist.  Create it?
            "
        end
        View.flash "- Directory is empty"#, :times=>2
        Move.to_end
        Notes.enter_junior
        View << "- "
        return
      end

      # Change path to proper indent
      dirs.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1/")}

      # Change path to proper indent
      files.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1")}

      Line.next
      left = $el.point

      # Move .notes files to top
      files = files.select{|i| i =~ /\.notes$/} + files.select{|i| i !~ /\.notes$/}

      both = options[:date_sort] || options[:size_sort] ?
        files + dirs : dirs + files
      View.insert(both.join("\n") + "\n")
      right = $el.point
      $el.goto_char left

      Line.to_beginning

      if options[:focus]
        Search.forward "^ +\\+ #{options[:focus]}$"
        Line.to_beginning
        Color.mark "light"
      end

      Tree.search(:left=>left, :right=>right, :always_search=>true)
    end

    # New Unified way to show one dir.
    def self.expand_one_dir options
      dir = options[:file_path]

      dirs, files = self.files_in_dir dir   # Get dirs and files in it

      if files.empty? && dirs.empty?
        if ! File.exists? dir   # If doesn't exist, show message
          return "
            - @mkdir/
            | ...dir '#{dir}' doesn't exist.  Create it?
            ".unindent
        end
        return "@flash/- Directory is empty"
      end

      # Add bullets and slashes
      dirs.collect!{|i| i.sub(/.*\/(.+)/, "+ \\1/")}
      files.collect!{|i| i.sub(/.*\/(.+)/, "+ \\1")}

      # Move .notes files to top
      files = files.select{|i| i =~ /\.notes$/} + files.select{|i| i !~ /\.notes$/}

      both = options[:date_sort] || options[:size_sort] ?
        files + dirs : dirs + files
      result = both.join("\n") + "\n"


      # TODO How to deal with focus? - return option telling them line to put cursor on - or, maybe, make that happen somehow on the other side of the call?
      #     if options[:focus]
      #       Search.forward "^ +\\+ #{options[:focus]}$"
      #       Line.to_beginning
      #       Color.mark "light"
      #     end

      result

    end

    #   def self.enter_snippet
    #     start = selected_window
    #     snippet = self.snippet
    #     self.open_in_bar

    #     loc = Location.new
    #     Line.to_left
    #     #orig = $el.point

    #     # If at indented line
    #     while(Line.matches(/^ /))
    #       Line.previous
    #     end

    #     # Check to see if it's the same file
    #     if snippet[/.+\n.+\n/] == buffer_substring($el.point, Line.left(3))
    #       Line.next 2
    #       View.insert "#{snippet.sub(/.+\n.+\n/, '')}\n"
    #     # Check to see if it's just in same dir
    #     elsif snippet[/.+\n/] == buffer_substring(point, Line.left(2))
    #       Line.next
    #       View.insert "#{snippet.sub(/.+\n/, '')}\n"
    #     else
    #       View.insert "#{snippet}\n"
    #     end

    #     loc.go
    #     #goto_char orig

    #     select_window(start)
    #   end

    # Enter tree of selection at the spot (saved by AS).
    def self.enter_at_spot
      snippet = self.snippet
      Location.jump("0")
      View.insert snippet
      Location.save("0")  # So any more will be entered after
    end

    # Enter what's in clipboard with | to on the left margin, with appropriate indent
    def self.enter_quote txt=nil

      prefix = Keys.prefix :clear=>1

      # Skip forward if on heading
      Line.to_left
      if Line[/^\|/]
        Move.to_end
        $el.newline
      end

      txt ||= Clipboard.get(0, :add_linebreak=>1)

      dir = Tree.construct_path rescue nil

      if self.dir? && self.handles?   # If current line is path
        Tree.plus_to_minus_maybe
        indent = Line.indent
        dir = Tree.construct_path
        t = Clipboard.get("=")
        t = t.gsub(/^#{dir}/, '')
        if t.sub!(/\A\n/, '')   # If no dir left, indent one over
          t.gsub!(/^  /, '')
          #         t = t.gsub(/^  /, '')
        end
        Tree.add_pluses_and_minuses t, '-', '-'
        Line.next
        View.insert "#{t}\n".gsub(/^/, "#{indent}  ")
        return
      end

      # If empty line, just enter tree...

      if Line.blank?

        if prefix == :u || txt =~ /\A  +[-+]?\|[-+ ]/   # If C-u or whole thing is quoted already, unquote
          txt = txt.split("\n").grep(/\|/).join("\n")
          return $el.insert(txt.gsub(/^ *[-+]?\|([-+ ]|$)/, ""))   # Remove | ..., |+...., |<blank>, etc
        end

        start = $el.point
        txt = Clipboard.get("=")
        indent = prefix || 0   # Indent prefix spaces, or 2
        txt = txt.gsub(/^/, " " * indent)

        View.insert txt
        $el.set_mark(Line.left(2))
        $el.goto_char start
        return
      end

      # Line has content, so indent under...

      txt = txt.unindent   # Unindent
      # TODO: don't unindent if up+?

      indent = Line.indent   # Get current indent
      on_quoted_line = Line.matches /^ +\|/
      Line.next

      # Indent one level further unless on comment already
      unless on_quoted_line
        indent = "#{indent}  "
      end

      indent += " " * Keys.prefix_or_0   # If numeric prefix, add to indent
      txt = txt.sub /\n+\z/, ''   # Remove last \n

      # Quote unless already quoted
      quote = txt =~ /^\|/ ? '' : "| "

      txt = txt.gsub /^/, "#{indent}#{quote}"
      txt = txt.gsub /^( *\|) $/, "\\1"   # Remove blank lines

      View.insert "#{txt}\n"
    end

    def self.enter_as_search
      indent = Line.indent
      Move.to_end
      View.insert "\n#{indent}  - ###{Clipboard["0"]}/"
      Launcher.launch_unified
    end

    # Remove the following lines indented more than the current one

    # Expand if dir, or open if file
    def self.launch options={}
      line = Line.value

      indent = Line.indent
      list = nil
      path = Tree.construct_path  # Get path

      without_label = Line.without_label
      if Launcher.wrapper path   # run files followed by slash as menus
        # Do nothing if it returned true
      elsif without_label =~ /^ *\$ /   # $ shell command inline (sync)
        Console.launch :sync=>true
      elsif without_label =~ /^ *%( |$)/   # % shell command (async)
        Console.launch_async
      elsif without_label =~ /^ *&( |$)/   # % shell command in iterm
        Console.launch_async :iterm=>1
      elsif line =~ /^[^|\n]* (\*\*|##)/   # **foo or ## means do grep
        self.grep_syntax indent
      elsif self.dir?   # if has slash - foo/ is a dir (if no | before)
        self.dir
      else
        options[:other_view] = 1 if Keys.prefix == :-
        self.open options
      end   # => {:no_recenter=>1}
    end

    # Grabs matching lines in file and starts hide search
    def self.outline_pattern extension=nil, options={}
      extension ||= View.extension

      if extension == "rb"
        "^\s*(def|class|module|it|describe|create_table|context) "
      elsif extension == "rake"
        "^\\s*(task|def|class) "
      elsif extension == "js"
        "(^ *(function)| = function\\()"
      elsif extension =~ /^notes|deck$/
        "^[\\|>]( |$)"
      else
        "^[^ \\t\\n]"
      end
    end


    # Called by to+outline and enter+outline, (others?).
    #
    # ...and @outline________
    #
    # > Don't insert, just return
    # p FileTree.enter_lines /foo/, :just_return=>1, :path=>View.file
    def self.enter_lines pattern=nil, options={}

      $xiki_no_search = false

      # If prefix is 6, delegate to Git to enter methods by date
      if options[:prefix] == 6
        txt = Git.methods_by_date(Tree.path[-1]).join("\n")
        Tree.<< Tree.quote(txt), :no_slash=>1
        return
      end

      # If dir, delegate to C-. (they meant to just open it)
      return Launcher.launch_unified if self.dir?

      Tree.plus_to_minus

      if Line.blank?   # If blank line, get bookmark and enter into current file

        path =
          if options[:path]
            options[:path]
          else
            bm = Keys.input(:timed => true, :prompt => "Enter bookmark to show outline for: ")
            File.expand_path Bookmarks.expand(bm, :just_bookmark => true)
          end

          #Files.directory? Bookmarks.expand("h", :just_bookmark=>true)

        # If it's a dir, delegate to Open Tree
        if path =~ /\/$/
          self.ls :here=>true, :dir => path
          return
        end

        View.insert "" + self.filename_to_next_line(path)
        $el.open_line 1
      end

      line = options[:path] || Line.value
      extension = line[/\.(\w+)$/, 1]

      if pattern.nil?
        return self.enter_lines(/#{self.outline_pattern extension}/, options)
      end

      Line.to_left
      path ||= options[:path] || Tree.construct_path  # Get path
      path = Bookmarks.expand(path)
      indent = Line.indent   # get indent


      # If image, insert it
      return self.enter_image path if extension =~ /^(jpg|jpeg|png|gif)$/i

      # Get matches from file
      matches = ""
      indent_more = '  '

      if path =~ /^\/\w+@/
        contents = Remote.file_contents path
        Tree.under contents, :escape=>'| ', :no_slash=>1
        return
      end

      # Adjust so it finds if we're on the line
      current_line = options[:current_line] + 1 if options[:current_line]

      line_found, matches_count, i = nil, 0, 0

      if ! File.exists? path
        self.file_not_found_from_template(path)
        return
      end

      # Go through file and extract pattern...

      IO.foreach(path, *Files.encoding_binary) do |line|
        i+=1
        line.sub!(/[\r\n]+$/, '')

        if current_line && line_found.nil?
          line_found = matches_count if i == current_line
        end

        next unless line =~ pattern
        line = line == "" ? "" : " #{line}"
        line.sub! /^ > $/, ' >'
        matches << "#{indent}#{indent_more}|#{line}\n"

        matches_count+=1
      end

      matches = matches.split("\n").uniq.join("\n")+"\n" if options[:unique]

      return matches if options[:just_return]

      Tree.insert_quoted_and_search matches, :line_found=>line_found
    end

    def self.enter_image image
      tmp_dir = "/tmp/insert_image"
      Dir.mkdir tmp_dir if ! File.exists? tmp_dir

      width, height = Console.sync("identify \"#{image}\"").match(/(\d+)x(\d+)/)[1..2]
      max = 300
      if width.to_i > max || height.to_i > max
        dest = tmp_dir+"/"+File.basename(image).sub(".", "_#{max}.")
        Console.sync %`convert "#{image}" -resize #{max}x#{max} "#{dest}"`, :dir=>"/tmp"
      else
        dest = image
      end

      Image.>> dest

      Line.previous
      Line.to_beginning

      nil
    end

    def self.file_not_found_from_template path

      # If .../menu/foo.rb

      if path =~ /.*\/menu\/(.+)\.rb$/   # "
        name = $1
        txt = File.read "#{Xiki.dir}/etc/templates/menu_template.rb"
      elsif path =~ /.*\/(.+)_spec\.rb$/   # "
        name = $1
        txt = File.read "#{Xiki.dir}/etc/templates/template_spec.rb"
      else

        name, extension = path.match(/([^\/]+)\.(.+)$/)[1..2] rescue [nil, nil]
        extension = path.match(/[^\/]+\.(.+)$/)[1] rescue nil

        [File.expand_path("~/xiki_stuff/templates/"), Bookmarks["$x/etc/templates"]].each do |dir|
          file = "#{dir}/template.#{extension}"
          next unless File.exists? file
          txt = File.read file
          break
        end
      end

      if txt
        View.flash "- File doesn't exist, start with this...", :times=>4
        txt.gsub!(/\{\{(.+?)\}\}/) { eval $1 }   # Expand {{these}}
        Xiki.dont_search
        return Tree.<< txt, :escape=>"| ", :no_slash=>1
      end

      # If no templates matched
      View.flash "- File doesn't exist!", :times=>2
      Tree.<< "|", :no_slash=>1
      Move.to_end
      View << ' '
    end

    # Mapped to shortcuts that displays the trees
    # enter+tree, do+tree
    def self.tree options={}

      prefix = Keys.prefix :clear=>true

      options.merge!(:recursive=>1) if prefix == :u   # If up+, expand dir recursively

      if prefix == :uu   # If up+up+, insert /the/path//
        Line << "#{Bookmarks["$#{Keys.input(:timed=>1)}"]}/"
        Launcher.go_unified
        return
      end

      $xiki_no_search = false

      bm = options[:bm] || Keys.input(:timed=>true, :prompt=>"Enter bookmark to show tree of: ")

      options.merge!(:focus=>View.file_name) if bm == "."
      dir = Keys.bookmark_as_path :bm=>bm, :include_file=>1

      # If file, delegate to FileTree.enter_lines...

      if File.file? dir
        return FileTree.enter_lines nil, :path=>dir
      end

      # If dir, delegate to .ls...

      # Put back slash if was dir
        dir = Bookmarks.dir_only dir
        dir << "/" unless dir =~ /\/$/

      return if dir == :bookmark_doesnt_exist

      dir = "/" if dir == :slash
      dir = Bookmarks.dir_only(dir) if options[:recursive]
      options.merge!(:dir=>dir)

      self.ls options
    end

    # Just returns the parent
    def self.parent
      return nil unless Line[/^ /]

      orig = View.cursor   # Store original location

      Tree.to_parent
      parent = Line.without_label

      View.cursor = orig
      parent
    end

    # Returns a regexp to match the acronym
    def self.is_remote? path
      return path =~ /^\/\w+@/
    end

    # Returns the files in the dir
    def self.files_in_dir dir, options={}
      if self.is_remote?(dir)
        self.remote_files_in_dir(dir)
      else
        dir = FileTree.add_slash_maybe dir
        all = Dir.glob("#{dir}*", File::FNM_DOTMATCH).
          select {|i| i !~ /\/\.(\.*|svn|git)$/}.   # Exclude some dirs (exclude entensions here too?)
          select {|i| i !~ /\/\.#/}.sort

        if options[:date_sort]
          all = all.sort{|a,b| File.mtime(b) <=> File.mtime(a)}
        elsif options[:size_sort]
          all = all.sort{|a,b| File.size(b) <=> File.size(a)}
        end

        dirs = all.select{|i| File.directory?(i)}#.sort
        files = all.select{|i| File.file?(i)}#.sort
        [dirs, files]
      end
    end

    def self.remote_file_contents file
      path, file = file.match(/(.+\/)(.+)/)[1..2]
      Remote.dir path, file   # Delegate to Remote.dir
    end

    def self.remote_files_in_dir dir
      res = Remote.dir(dir)
      res.map!{|i| "#{dir}#{i}"}
      [res.select{|i| i =~ /\/$/}.map{|i| i.sub(/\/$/, '')}, res.select{|i| i !~ /\/$/}]
    end

    def self.url_from_path verb, path
      server, path = path.match(/([\w.-]+)(\/.*)/)[1..2]
      "http://#{server}:7433/#{verb}#{path}"
    end

    def self.indentify_path path
      result = ""
      indent = ""
      path.each do |i|
        result << "#{indent}#{i}\n"
        indent += "  "
      end
      result
    end

    def self.tree_to_paths tree
      # TODO: implement
    end

    def self.filename_to_next_line path
      path.sub(/(.+)\//, "\\1/\n  - ")  # Add linebreak before filename
    end

    # If cursor on a tree, returns it, otherwise return path of current file
    def self.tree_path_or_this_file dir_only=false

      # If in tree, use that dir
      path = self.handles? && Keys.prefix != :u ?
        Tree.construct_path :
        View.file

      #     path = Files.just_dir(path) if dir_only
      if dir_only
        path = path =~ /\/$/ ? path : File.dirname(path)+"/"
      end
      path
    end

    def self.do_create_dir

      path = self.tree_path_or_this_file :dir_only

      `mkdir -p "#{path}"`
      View.flash "- Created: #{path}"
    end

    # Indent txt to be one level lower than current line
    def self.one_view_in_bar_by_default= to
      @@one_view_in_bar_by_default = to
    end

    def self.copy_path options={}
      Effects.blink :what=>:line
      # Return dir of view's file if at left margin, U, or not ^[|@-]
      if Line !~ /^ +[|@+-]/  # || Keys.prefix_u# It will never be :u, because the prefix is cleared before this is called
        path = View.file
      else
        path = Xiki.trunk.last   # Grab from wiki tree

        path.sub! /\/$/, '' if Line.value !~ /\/$/   # If current line doesn't have trailing slash, remove it
      end

      View.flash "- copied: #{path}", :times=>2
      return Clipboard["0"] = path
    end

    # Adds extra line if we're at the end of the file.
    # If there's no linebreak, it causes weird errors.
    def self.extra_line_if_end_of_file
      if Line.right == View.bottom
        Line.to_right
        $el.open_line(1)
      end
    end

    def self.add_slash_maybe dir
      dir =~ /\/$/ ? dir : "#{dir}/"
    end

    def self.grep_syntax indent
      Tree.plus_to_minus_maybe
      dir = Tree.construct_path

      raw = Tree.construct_path(:labels=>true, :list=>true)
      files = contents = nil

      if raw.join('') =~ /\*\*(.+)##(.+)/  # *foo means search in files
        files, contents = $1, $2.sub!(/\/$/, '')
      elsif raw.last =~ /\*\*(.+)/  # *foo means search in files
        files = $1
      elsif raw.last =~ /##(.+)/  # ##foo means search in filenames
        contents = $1.sub!(/\/$/, '')
        if raw[-2] =~ /\*\*(.+)/   # If prev is **, use it
          files = $1
        elsif raw[-2] =~ /(.+[^\/])$/

          # If file.foo/##, search in the one file...

          contents = Regexp.new(contents, Regexp::IGNORECASE)
          list = self.filter_one_file Bookmarks.expand(dir), contents, :indent=>"  "
        end
      end


      files.sub!(/\/$/, '') if files
      options = {}
      options.merge!({:files => files}) if files

      unless list   # If not already gotten
        Search.append_log dir, "- ###{contents}/"

        # Do actual search...

        list = self.grep dir, contents, options
        list.shift   # Pull off first dir, so they'll be relative
      end
      Line.to_next

      left = $el.point
      tree = list.join("\n") + "\n"
      View.insert tree.gsub(/^/, indent)
      right = $el.point
      $el.goto_char left

      # Do appropriate incremental search...

      if Line.matches(/^\s*$/)  # Do nothing
      elsif Line.matches(/^\s+\|/)
        if Search.outline_goto_once   # If we want to go back to a line
          Line.next Search.outline_goto_once
          Search.outline_goto_once = nil
        end

        Tree.search :left=>left, :right=>right
      elsif contents   # If we searched for something (and it wasn't just the dir)
        Move.to_quote
        Tree.search :left=>left, :right=>right, :recursive_quotes=>true
      else
        Move.to_junior
        Tree.search :left=>left, :right=>right, :recursive=>true
      end
    end

    def self.move_dir_to_junior
      Keys.prefix_times.times do
        orig = Location.new
        indent = Line.indent
        txt = View.paragraph(:start_here => true, :delete => true)
        View.insert self.move_dir_to_junior_internal(txt, Keys.prefix_u?)

        orig.go

      end
    end

    def self.move_dir_to_junior_internal txt, prefix_u=nil

      first, rest = txt.match(/(.+?\n)(.+)/m)[1..2]
      indent = first[/ */]

      # Split off any text not under branch (of first line)
      # Text indented the same as the first line (and after)
      rest_and_siblings = rest.match(/(.*?)^(#{indent}[^ \n].*)/m)
      rest, siblings = rest_and_siblings ? rest_and_siblings[1..2] : [rest, ""]

      # What does this do??
      if prefix_u
        rest.sub! /^\s*[+-] /, ''
        rest.gsub! /^  /, ''
        "#{first}#{rest}#{siblings}"
      else

        first.sub! /(.+\/)(.+\/)$/, "\\1\n#{indent}  - \\2"
        rest.gsub! /^/, "  "
        "#{first}#{rest}#{siblings}"
      end
    end

    # Returns whether line is a dir (ends with "/")
    def self.dir? txt=nil
      txt ||= Line.value
      txt =~ /^[^,|\n]*\/$/
    end

    # Restructuring for Unified...
    def self.delete_file path

      #     in_file_being_deleted = Line !~ /^[ +-]/ || Keys.prefix_u

      # If not on tree, must want to delete this file
      #     if in_file_being_deleted
      #       path = View.file
      #     else
      #     path = Tree.construct_path
      #     end

      #     path = View.expand_path path


    # Still has dependencies on editor.  First step for if we want to delete
    # from a non-editor client - make below editor lines just not run when
    # not :client =~ ^editor\b.

      return View.flash("- File doesn't exist: #{path}", :times=>5) if ! File.exists?(path)

      View.flash "- Delete file for sure?"
      answer = Keys.input :chars=>1, :prompt=>"For sure delete?: #{path}"   #"

      return unless answer =~ /y/i

      executable = File.directory?(path) ? "rm -r" : "rm"
      command = "#{executable} \"#{path}\""

      result = Console.run command, :sync=>true
      if (result||"").any?
        View.beep
        View.message "#{result}"
        return
      end

      # Eventually only do if :client =~ ^editor\b

      Tree.kill_under
      Effects.glow :fade_out=>1
      Line.delete
      Line.to_beginning

      View.message "File deleted"

    end

    def self.move_latest_screenshot_to dest_path, dest_dir
      desktop = Bookmarks['$dt']

      latest_screenshot = `ls -t #{desktop} "Screen shot*"`[/Screen shot .*/]

      return View.message("No screenshot found.") if latest_screenshot.empty?

      command = "mv \"#{desktop}#{latest_screenshot}\" \"#{dest_path}\""

      result = Console.run command, :sync=>true
      View.message result
    end

    def self.move_to
      self.copy_to :move=>1
    end

    def self.copy_current_file_to options={}

      return View.beep '- Save the file first!' if View.modified?

      # Go from current file to bookmark
      source_path = View.file
      View.flash "- to which bookmark?"
      verb = options[:move] ? "Move" : "Copy"
      dest_path = Keys.bookmark_as_path :prompt=>"#{verb} current file to which bookmark? "
      return if ! dest_path.is_a? String
      #     dest_path = File.dirname dest_path

      dest_path = Bookmarks[dest_path]

      executable = options[:move] ? "mv" : "cp -r"

      command = "#{executable} \"#{source_path}\" \"#{dest_path}\""

      result = Console.run command, :sync=>true

      return View.beep result if (result||"").any?   # If output isn't as expected, beep and show error

      if options[:move]
        View.kill
        View.open "#{dest_path}#{File.basename source_path}"
        verb = "Moved"
      else
        verb = "Copied"
      end

      View.flash "- #{verb} to: #{dest_path}"
    end

    def self.copy_to options={}
      prefix = options[:prefix] || Keys.prefix
      Keys.clear

      # Error if not in a file tree

      return self.copy_current_file_to options if ! FileTree.handles?

      dest_path = Tree.construct_path

      dest_dir = dest_path.sub(/(.+\/).*/, "\\1")
      slash = dest_dir =~ /\/$/
      dest_dir = File.expand_path dest_dir
      dest_dir = "#{dest_dir}/" if slash

      if prefix == 2
        self.move_latest_screenshot_to dest_path, dest_dir
        return View.flash "- Moved the latest screenshot to here!"
      end

      arg = options[:move] ? {:delete=>1} : {}
      source_path = Tree.dir_at_spot arg

      stem = File.basename source_path   # Cut of path of source
      indent = Line.indent

      dest_is_dir = dest_path =~ /\/$/
      source_is_dir = source_path =~ /\/$/

      if dest_is_dir   # If dest is a dir, insert junior
        Line.next
        indent << "  "
        dest_stem = stem

      else   # If file, use as dest
        dest_stem = Line.without_label
      end
      executable = options[:move] ? "mv" : "cp -r"

      # Add ".1" if prefix is 1
      if prefix == 1
        dest_stem << '.1'
      end

      command_dest_stem = dest_stem

      if source_path =~ /\/$/   # If source is dir
        source_path.sub! /\/$/, ''   # Remove slash, so it copies dir, not contents
        command_dest_stem = ""   #     put into, not replace
      end

      command = "#{executable} \"#{source_path}\" \"#{dest_dir}#{command_dest_stem}\""

      result = Console.run command, :sync=>true

      if (result||"").any?   # If output isn't as expected, beep and show error
        View.beep
        View.message "#{result}"
        return
      end

      dest_stem << "/" if source_is_dir

      Line.to_left
      if dest_is_dir || prefix == 1   # If it's a file, we didn't delete it
        Line.next if prefix == 1
        View.insert "#{indent}+ #{dest_stem}\n", :dont_move=>true
        Line.to_beginning
        Effects.glow :fade_in=>1
        Line.previous if prefix == 1
      end
      View.flash "- copied!"
    end

    def self.rename_file
      # If dired mode, use wdired
      return $el.wdired_change_to_wdired_mode if $el.elvar.major_mode.to_s == "dired-mode"

      column = View.column

      if ! Line[/^ *[+-] /]   # Error if not indented and ^/- /
        View.beep
        return View.message "TODO: implement renaming current file?"
      end
      source_path = Tree.construct_path
      is_dir = source_path =~ /\/$/
      source_path.sub! /\/$/, ''

      new_name = Keys.input(
        :prompt=>"Rename #{source_path} to what?: ",
        :initial_input=>(Keys.prefix_u? ? File.basename(source_path) : "")
        )

      dest_path = "#{source_path.sub(/(.+\/).+/, "\\1#{new_name}")}"

      command = "mv \"#{source_path}\" \"#{dest_path}\""

      Console.run command, :sync=>true

      indent = Line.indent
      Effects.glow :fade_out=>1
      Line.delete
      View.insert((is_dir ? "#{indent}- #{new_name}/\n" : "#{indent}+ #{new_name}\n"), :dont_move=>true)

      View.column = column
      Effects.glow :fade_in=>1
    end

    def self.open_as_upper where=false
      orig_u = Keys.prefix_u
      view = View.current
      path = Tree.construct_path(:list=>true)

      if where == :lowest
        Move.to_window 9
      else
        View.to_upper
      end
      was_at_top = View.start == $el.point
      $el.split_window_vertically

      View.next if where   # :lowest or :second

      if was_at_top
        where ? View.previous : View.next
        $el.recenter 0
        where ? View.next : View.previous
      end

      self.open :path=>path, :same_view=>true
      View.to_window(view) if orig_u
    end

    def self.to_outline options={}
      prefix = Keys.prefix :clear=>true
      extension = View.extension

      if (prefix == :u || prefix == :-) && ! Notes.enabled?
        args = [View.txt, View.line]
      end
      current_line = Line.number
      mode = View.mode

      path = View.file
      View.to_buffer("*tree outline")
      View.clear;  Notes.mode

      if path
        dir, file = path.match(/(.+\/)(.+)/)[1..2]
        txt = "- #{dir}\n  - #{file}\n"
        View.insert txt
        View.to_top
        self.select_next_file
      else
        View.insert "- buffer/\n"
        View.to_top
      end

      case prefix
      when 2   # Prompt to add @... menu under file
        Move.to_end
        Xiki.insert_menu
      when 4   # Get ready to run $... shell command on file
        $el.delete_char(1)
        View << "$ "
        ControlLock.disable
      when 5   # Unique
        self.enter_lines nil, :current_line=>current_line, :unique=>1
      when 6   # List methods by date (use 'git blame')
        self.enter_lines nil, :prefix=>6
      when 7
        Move.to_end
        View << "\n    @info"
        Launcher.launch_unified
      when 8
        Launcher.enter_all
      when 0
        # Just show path if 0+...
      when :-   # Just show # foo... comments...
        self.enter_lines(/(^ *(def|function) |(^| )(#|"|\/\/) .+\.\.\.$)/, :current_line=>current_line)
      when :u
        if mode == :notes
          self.enter_lines /^> .*:$/, options.merge(:current_line=>current_line)
          return
        end

        txt, line = Search.deep_outline *args
        Tree.<< txt, :line_found=>line, :escape=>'| ', :no_slash=>1
      else
        self.enter_lines nil, options.merge(:current_line=>current_line)
      end

    end

    def self.skip_dirs dir, skip_these
      dir = Bookmarks[dir].sub /\/$/, ''
      (@skip ||= {})[dir] = skip_these
    end


    def self.expands? options
      return unless options[:file_path]
      (options[:expanders] ||= []).push self
    end

    def self.expand options
      prefix = options[:prefix]

      file_path = options[:file_path]

      Path.unescape! file_path

      # If as+delete...

      if prefix == "delete"
        return self.delete_file file_path
      end

      # If ##foo/ or **foo/ filter at end (removing all as we check)...

      filters_at_end = self.extract_filters! file_path   # Removes ## and ** filters, returning any that were at the end

      return options[:output] = self.expand_filter(filters_at_end, options) if filters_at_end

      # If it's a $... shell command...

      if file_path =~ %r"^(/[\w./ -]+/)([$%&]) (.+)"   # If /foo// or //foo
        options.merge!(:dir=>$1, :prompt=>$2, :command=>$3)
        return options[:output] = self.expand_shell(options)
      end

      # If quoted line, pull it off into :quote...

      if quote = Path.extract_quote(file_path)
        ControlTab.dash_prefix_buffer = View.name
        options[:quote] = quote
      end

      if line_number = Path.extract_line_number(file_path)
        options[:line_number] = line_number
      end

      # If it's a file...

      if File.file? file_path
        options[:no_slash] = 1

        return options[:output] = self.filter_one_file(file_path).join("\n") if prefix == "outline"

        return options[:output] = Tree.quote(File.read(file_path, *Files.encoding_binary)) if prefix == "all"
        return options[:output] = self.save(file_path, options) if prefix == "update" && options[:quote]

        # If editor, tell it to open the file...

        if options[:client] =~ /^editor\//
          txt = "@open file/#{file_path}"
          txt << "/:#{options[:line_number]}" if options[:line_number]   # If a quote, pass line number after a colon!
          txt << "/|#{options[:quote]}" if options[:quote]   # If a quote, pass line number after a colon!
          return options[:output] = txt
        end

        # If API, return contents...
        return options[:output] = File.read(file_path)
        # If not editor, |... will be ignored for now.  Any reasons would api pass this in?  Probably to create non-existant files?  Maybe grab lines indented under it?  Maybe return line number of it?
      end

      # If a dir...

      if File.directory? file_path
        # TODO: maybe deprecate C-8 prefix for this, only make it enter+all ("all")
        if prefix == 8 || prefix == "all"
          return options[:output] = self.expand_dir_recursively(options)
        end

        return options[:output] = self.expand_one_dir(options)
      end

      # File doesn't exist...

      if options[:file_path] =~ /\/$/
        return options[:output] = self.suggest_mkdir(file_path)
      end

      # Quote means to create the file (with or without "update" prefix)...

      return options[:output] = self.save(file_path, options) if options[:quote]

      # File path (no quote)...

      # If enter+all, do what?
      return options[:output] = "@flash/- file doesn't exist!" if prefix == "all" || prefix == "outline"

      options[:output] = "@open file/#{file_path}"   # So just open the new file

      # What about when :client !~ ^editor
      # - can probably assume there won't be an update prefix
      #   - so, what will it do?  Maybe it'll send a multiline string if it wants to save?  Probably worry about this later?

    end

    def self.suggest_mkdir file_path
      "
        @mkdir/
        | Dir '#{file_path}' doesn't exist.  Create it?
        ".unindent
    end


    def self.expand_shell options
      options[:no_slash] = 1
      Console.shell_command_per_prompt options[:prompt], options
    end


    # Removes all ##.../ and **.../ strings.  Returns those that
    # are at the end.
    #
    # FileTree.extract_filters("/projects/foo/##hey/")
    def self.extract_filters! file_path
      patterns_at_end = []

      # Treat \/ as part not a slash delimiter (gsub it to 021 temporarily)
      file_path.gsub!("\\/", "\021")

      # While a (rightmost) filter exists
      while file_path =~ %r"^([^|]*/)((##|\*\*).+?/)"
        start, filter = $1.length, $2

        # If it was at end, store it so we can do filter, and ignore the rest
        if start + filter.length == file_path.length
          patterns_at_end.unshift filter.gsub("\021", "\\/")
        end

        file_path.slice! start, filter.length   # Delete it
      end

      patterns_at_end.any? ? patterns_at_end : nil
    end


    def self.expand_filter filters, options

      raise "We're not yet set up to handle multiple ## or ** filters of the same type.  Should be easy though, just run both regex's as you grep." if filters.length > 2

      file_path = options[:file_path]

      content_filter, file_filter = nil, nil
      filters.each do |filter|
        is_content_filter = filter =~ /^##/
        filter = filter[/^..(.+).$/, 1]
        is_content_filter ?
          content_filter = filter :
          file_filter = filter
      end

      if File.file? file_path.sub(/\/$/, '')
        return self.filter_one_file(file_path.sub(/\/$/, ''), /#{content_filter}/i, :indent=>"  ").join("\n")
      end

      options = {}
      options.merge!({:files => file_filter}) if file_filter

      list = self.grep file_path, content_filter, options

      list.shift   # Pull off first dir, so they'll be relative
      list = list.join "\n"   # + "\n"
      list
    end

    # Return whether path is file-like or dir-like
    # Looks at the actual item on disk unless :no_look.
    #
    # FileTree.examine("/tmp")
    #   [true, :dir]   # Exists and is a dir
    # FileTree.examine("/tempizzle/what.txt")
    #   [false, :dir]    # Doesn't exist, but looks like a file path (has an extension)
    def self.examine path
      exists = File.exists? path

      kind =
        if exists
          File.file?(path) ? :file : :directory
        else
          path =~ /\.\w+\/?/ ? :file : :directory
        end

      [exists, kind]
    end

    # Returns html version of a file tree
    def self.to_html txt
      folder = '<img src="http://wiki.oni2.net/w/images/0/0c/Folder_icon-Mac.jpg" class="icon">'
      file = '<img src="http://wiki.oni2.net/w/images/1/11/Generic_file_icon-Mac.jpg" class="icon">'

      # - dir/
      txt.gsub! /^( *)[+-]( .+\/)$/, "\\1#{folder}<span class='folder'>\\2</span>"
      # |...
      txt.gsub! /^( *)(\| ?)(.*)$/, "\\1<span style='color:#ddd; font-weight:bold;'>\\2</span><span class='quote'>\\3</span>"
      # - hey.txt
      txt.gsub! /^( *)([+-])(.+)/, "\\1#{file}<span class='arial'>\\3</span>"

      txt = "<pre>\n#{txt}\n</pre>\n"
      txt << %`
        <style>
          .arial {font-family: arial;}
          .icon {padding: 0 3px 0 0; vertical-align: middle;}
          .quote { font-family: monaco; color:#666; font-size:11px; }
          .folder {
            font-weight: bold;
            font-family: arial;
            /* padding: 0 0 0 2px; */
          }
        </style>
        `.unindent
      txt
    end

  end
  FileTree.define_styles
end
