# -*- coding: utf-8 -*-

#
# > Summary
# For handling file system paths
# and the like.
#
# > .add
# ! 1 + 1
#
# > .subtract
# ! 10 + 1
#


require 'xiki/core/styles'
require 'xiki/core/line'
require 'xiki/core/view'
require 'xiki/core/cursor'
require 'xiki/core/code'

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
        | (the "= " isn't required when there are no spaces at the beginning of the
        | line).
        |
        = /
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

    def initialize
      @res = ""
      @list = []
      @took_too_long = false
      @file_regex, @reverse_sort = nil, nil
      @time_recursive_started = Time.now.to_f
    end
    attr :res
    attr :list
    attr_accessor :file_regex, :reverse_sort

    # Change dirs into spaces, etc
    def clean path, indent=0
      path = path.gsub(/.+?\//, "  ")
      indent == 0 ?
        path :
        path.sub(/^#{indent}/, '')
    end

    # Recursively draws out tree
    def traverse path

      return if @took_too_long   # Stop recursing if took too long

      at_root = @res == ""

      entries = Dir.glob("#{path == '/' ? '' : View.expand_path(path)}/*", File::FNM_DOTMATCH).
        select {|i| i !~ /\/\.(\.*|svn|git)$/}#.   # Exclude some dirs (exclude entensions here too?)

      # Sort, handling error when certain type of file
      entries = entries.sort{|a, b|
        a_time = File.mtime(a) rescue nil
        b_time = File.mtime(b) rescue nil
        next 0 if ! a_time || ! b_time
        b_time <=> a_time
      }

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

      # Too much time has elapsed, so raise timeout

      if Time.now.to_f - @time_recursive_started > 2
        @took_too_long = true
      end

      # At root and took to long, so just show root dir listing
      if at_root && @took_too_long
        @res = FileTree.expand_one_dir :file_path=>File.expand_path(path)
        View.message "Only showing one level, it was taking a long time"
      end

    end

    # Returns tree of files and their contents matching the
    # regex (as an array of lines).
    # FileTree.grep "/projects/xiki/spec/fixtures/roots/dd/", ""
    def self.grep dir, regex, options={}

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
        t.reverse_sort = files.slice! /^\*/   # If * at beginning, it means to do reverse sort
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

      Search.append_log path, "- #{prepend}#{regex}"

      View.to_buffer "tree filter"
      View.dir = Files.dir_of(path)
      View.clear
      Notes.mode :wrap=>false

      if File.directory?(File.expand_path path)
        View << "#{Files.tilde_for_home path}\n  - #{prepend}#{regex}\n"
      else

        dir, name = path.match(/(.+)\/(.+)/)[1..2]
        View << "#{dir}/\n  - #{name}\n    - #{prepend}#{regex}\n"
      end

      View.to_bottom; Line.previous
      Launcher.launch
    end

    def self.grep_one_file(f, regex, indent)
      result = []

      return result if f =~ /\.(ai|icns|png|gif|jpg|gem|ttf)$/

      IO.foreach(f, *Files.encoding_binary) do |line|
        line.chomp!
        if regex
          next unless (line =~ regex rescue nil)
        end
        result.<< line == "" ?
          "#{indent}:" :
          "#{indent}: #{line}"
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

      current_line = options[:current_line]# || Search.outline_goto_once

      line_found, matches_count, i = nil, 0, 0
      IO.foreach(file, *Files.encoding_binary) do |line|
        i+=1

        # Should fix utf error
        line.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

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

        result << "#{indent}:#{line}"
        matches_count+=1
      end

      if line_found   # If line we were on was found, remember it so we can hililght it when we show the outline
        options[:line_found] = line_found
      end

      return ["<*"] if result.empty?

      result
    end

    def self.skip
      @skip || {}
    end

    def grep_inner path, regex, first_time=nil

      path.sub!(/\/$/, '')

      entries = Dir.glob(["#{path}/*"], File::FNM_DOTMATCH).
        select{|i| i !~ /\/\.(\.*|svn|git)$/}.entries#.sort_by{|w| w.downcase}

      # Sort, handling error when certain type of file
      entries = entries.sort{|a, b|
        a_time = File.mtime(a) rescue nil
        b_time = File.mtime(b) rescue nil
        next 0 if ! a_time || ! b_time
        b_time <=> a_time
      }

      entries = entries.select{|o| o !~ /\/(vendor|log)$/}   # Exclude some dirs (why doing it here?

      # Sort reverse if flag set
      if reverse_sort
        entries.reverse!
      end

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

      Code.cache(:file_tree_define_styles) do
        Styles.define :ls_bullet, :face=>'menlo', :size=>"+2", :fg=>"d70", :bold=>1

        Styles.define :ls_bullet_darker, :face=>'menlo', :size=>"+2", :fg=>"555", :bold=>nil

        Styles.define :quote_heading_h0, :size=>"+8", :face=>"arial"#, :bold=>true
        Styles.define :quote_heading_h1, :size=>"2", :face=>"arial"#, :bold=>true
        Styles.define :quote_heading_h2, :size=>"-2", :face=>"arial"#, :bold=>true

        Styles.define :quote_heading_pipe, :fg=>"555", :size=>"0", :face=>"xiki", :bold=>1 # , :bg=>"unspecified"

        Styles.define :quote_light, :fg=>"333", :size=>"0", :face=>"xiki", :bold=>true

        Styles.define :quote_heading_h1_green, :fg=>"7c3", :size=>"2", :face=>"arial", :bold=>true

        Styles.define :quote_heading_bracket, :fg=>"555", :size=>"-2", :face=>"arial black", :bold=>1


        Styles.define :quote_heading_small, :size=>"-2", :face=>"arial black"#, :bold=>true


        Styles.define :diff_line_number, :bold=>true, :size=>"-2", :fg=>"666"
        Styles.define :diff_red, :bg=>"400", :fg=>"ee3333", :size=>"-1", :bold=>nil
        Styles.define :diff_red_pipe, :bg=>"400", :fg=>"711", :size=>"0", :face=>"xiki", :bold=>true

        Styles.define :diff_green, :bg=>"130", :fg=>"4d3", :size=>"-1", :bold=>nil
        Styles.define :diff_green_pipe, :bg=>"130", :fg=>"228822", :size=>"0", :face=>"xiki", :bold=>true



        Styles.define :wrapper_pattern, :bg=>"330", :fg=>"ec0", :size=>"-1"
        Styles.define :wrapper_pattern_pipe, :bg=>"330", :fg=>"770", :size=>"0", :face=>"xiki", :bold=>true

        Styles.define :diff_small, :fg=>"222", :size=>"-11"


        Styles.define :diff_yellow, :bg=>"950", :fg=>"fa0", :size=>"-1", :bold=>nil
        Styles.define :diff_yellow_pipe, :bg=>"950", :fg=>"b80", :size=>"0", :face=>"xiki", :bold=>true

        Styles.tree_letters :underline=>1
        Styles.tree_letters2 :underline=>1

        Styles.define :ls_dir, :fg=>"999", :face=>"verdana", :size=>"-1", :bold=>true
        Styles.define :task_bullet_slash, :fg=>"777", :face=>"verdana", :size=>"-1", :bold=>true
        Styles.define :task_bullet, :fg=>"888"

        Styles.define :quote_medium, :size=>140, :fg=>"666", :bold=>1

        # ##search/
        Styles.define :ls_search, :fg=>"ff7700", :face=>"verdana", :size=>"-2", :bold=>true

        Styles.define :ls_quote, :size=>"-1", :fg=>"999"
        Styles.define :ls_quote_light, :size=>"-1", :fg=>"555", :bg=>"222"

        # 001| Quoted text lines
        Styles.define :ls_quote_line_number, :size=>"-4", :fg=>"eee"

        # Highlight in search
        Styles.define :ls_quote_highlight, :size=>"-1", :bg=>"ffff44", :fg=>"666666"

        # Because default color is too dark for some backgrounds
        Styles.define :comint_highlight_prompt, :fg=>"#06c"

      end

      return if $el.caching

    end

    def self.apply_styles

      Code.cache(:file_tree_apply_styles) do

        Styles.clear

        # return if $el.caching

        # - bullets...

        # Make "* foo" bullets have gray asterix
        Styles.apply("^[ \t]*\\([*+-]\\)\\( \\)", nil, :ls_bullet_darker, :variable)   # - fooo, + foo

        Styles.apply("^[ \t]*\\([+-]\\)\\( \\)", nil, :ls_bullet, :variable)   # - fooo, + foo

        # ~ foo
        # ~ foo/ > exists at end of this method, because it has to override the following ones

        Styles.apply("^ +\\(:[0-9]+\\)\\(|.*\n\\)", nil, :ls_quote_line_number, :ls_quote)   # :NN|foo... used any more?

        # Path-like lines and parts of lines (make gray)

        # Single "@" or bullets at beginning
        Styles.apply("^[ <+@-]*\\(=\\)", nil, :ls_dir)   # @, <= @, etc.

        Styles.apply("^[ \t]*\\(<+[=|@:*+~>-]?\\)", nil, :ls_bullet)   # <<, <=, etc bullets

        Styles.apply("^ *@[a-z]+", :ls_dir)   # @foo   < Username

        # Slash after almost anything

        Styles.apply("^ *\\([ <=|@:*+~-]+ \\)?=?\\([+~!$%^&#a-zA-Z0-9_,?* ().:;@='<>-]*[^ \n]\/\\)", nil, nil, :ls_dir)   # foo/ or <=foo/, etc.

        Styles.apply("^ *\\([ <=+-]+ \\)?=?\\(\/\\)", nil, nil, :ls_dir)   # - /


        # Covers paths in files by themselves
        Styles.apply("^ *\\([ <=+-]+ \\)?=?\\([@=~$%&a-zA-Z0-9_,*+? ().^;<>-]*\/[@=\#'$a-zA-Z0-9_,*? ().:;\/<>-]+\/\\)", nil, nil, :ls_dir)   # Paths with multiple slashes

        # < next) menus/
        Styles.apply("^[ \t]*[<+-][<+=-]* [a-zA-Z0-9_,? ().:;+-]+?[:)] \\(\[.@=a-zA-Z0-9 ]+\/\\)", nil, :ls_dir)   # - label) oneword/slash
        Styles.apply("^[ \t]*[<+-][<+=-]* [a-zA-Z0-9_,? ().:;+-]+?[:)] \\([.@=a-zA-Z0-9 ]+\/[.@=a-zA-Z0-9 \/]+\/\\)", nil, :ls_dir)   # - label) oneword/path/slash

        # Bullets
        Styles.apply("^[ \t]*[+-] [^(\n]+?) \\(.+/\\)$", nil, :ls_dir)   # - hey) /what/
        Styles.apply("^[ \t]*[+-] [a-zA-Z0-9_,? ().:;-]+?: \\(.+/\\)$", nil, :ls_dir)   # - hey: /what/

        Styles.apply('\\(https?\\|file\\|xiki\\|source\\):/[a-zA-Z0-9\/.~_:;,?%&*=|+!@#()-]+', :notes_link)   # http://url.com
        Styles.apply('^[a-z][a-z.]+@[a-z][a-z.]+[a-z] ', :notes_link)   # email@address.com Message to user


        Styles.apply("^ *\\([`!:]\\)\\($\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote)   # :... lines (quotes)
        Styles.apply("^ *\\([\\\\]\\)\\($\\| .*\n\\)", nil, :quote_heading_pipe, :default)   # :... lines (quotes)


        # |... lines (quotes)

        Styles.apply("^ *\\(|\\)\\(.*\n\\)", nil, :quote_heading_pipe, :ls_quote)   # |... lines (quotes)

        # | hey : you (makes colon big)
        Styles.apply("^ *\\(|\\)\\( .* \\)\\(:\\)\\( .*\n\\|$\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_pipe, :ls_quote)

        Styles.apply("^ *\\(|\\)\\(.+?\\)([+-].*[-+])", nil, :quote_heading_pipe, :ls_quote)   # quoted lines: beginnings of lines
        Styles.apply("^ *|.*([-+].*[+-])\\(.+\\)$", nil, :ls_quote)   # quoted lines: ends of lines
        Styles.apply("[+-])\\(.*?\\)([+-]", nil, :ls_quote)   # quoted lines: between diffs

        Styles.apply("^ *\\([|:]\\)\\([()].*\n\\)", nil, :quote_heading_pipe, :ls_quote_light)   # |(... or :(... lines
        Styles.apply("^ *\\(||\\)\\(.*\n\\)", nil, :quote_heading_pipe, :ls_quote)   # ||... or ::... lines

        # | >... headings
        Styles.apply("^ *\\([=|:]\\)\\( ?\\)\\(>\\)\\(\n\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading_h1)

        # | >>
        Styles.apply("^ *\\(|\\)\\( \\)\\(>>\\)\\(\n\\| .*\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading_small)
        # | >...! headings
        Styles.apply("^ *\\(|\\)\\( ?\\)\\(>\\)\\(.*!:?\n\\)", nil, :quote_heading_pipe, :ls_quote, :quote_heading_bracket, :quote_heading_h1_green)

        # |+... diffs
        Styles.apply("^ +\\(:[0-9]+\\)$", nil, :ls_quote)
        Styles.apply("^ *\\([|:]\\+\\)\\(.*\\)", nil, :diff_green_pipe, :diff_green, :face=>"xiki")   # :+...
        Styles.apply("^ *\\([|:]-\\)\\(.*\\)", nil, :diff_red_pipe, :diff_red)   # :-...
        Styles.apply("^ *\\([|:]\\?\\)\\(.*\\)", nil, :diff_yellow_pipe, :diff_yellow, :face=>"xiki")   # :?...

        Styles.apply("^ *\\(|:\\)\\(.*\\)", nil, :wrapper_pattern_pipe, :wrapper_pattern, :face=>"xiki")   # |:...


        # ::... draw attention to lines with extra colon > in shell output, it means line's expandable
        Styles.apply("^ *\\(::\\)\\(.*\n\\)", nil, :quote_heading_pipe, :ls_quote)   # ::...

        Styles.apply("^ *\\([|:]\\)\\(@@ .*\n\\)", nil, :quote_heading_pipe, :diff_line_number)

        Styles.apply('^ *\\(//?\\)$', nil, :ls_dir)  # /
        Styles.apply('^ *\\(\./\\)$', nil, :ls_dir)  # ./

        # Has to be at bottom, to override other styles...


        # Styles for option bullets
        Styles.apply("^[ \t]*\\([~^]\\)\\( \\)\\(.*\\)", nil, :ls_bullet_darker, :variable, nil)   # "~ foo" or "^ foo"
        Styles.apply("^[ \t]*\\([~*^]\\)\\( \\)\\(.*\/$\\)", nil, :ls_bullet_darker, :variable, :task_bullet_slash)   # ~ foo/ (similar but bold)


        Styles.apply("^[ \t]*\\(<-\\|->\\) ", nil, :ls_bullet_darker)   # "<- foo" or "-> foo"

      end

    end

    def self.apply_styles_at_end
      Styles.apply('^[ =]*\\([+-] \\)?\\(##.*/?\\)$', nil, nil, :ls_search)  # ##_/
      Styles.apply('^[ =]*\\([+-] \\)?\\(\\*\\*.*/?\\)$', nil, nil, :ls_search)  # **_/

      Styles.apply('^#:.*', :ls_search)   # #:foo

      Styles.apply('^ *\\([+-] \\)?\\(@f/.*/\\)$', nil, nil, :ls_search)
      Styles.apply('^ *[+-] \\(\*\*.*/\\)$', nil, :ls_search)  # **_/
      Styles.apply('^ *\\([+-] \\)?\\(@n/.*/\\)$', nil, nil, :ls_search)
    end

    # FileTree.handles? "%foo"
    # FileTree.handles? "/a"
    # FileTree.handles? "~/a"
    # FileTree.handles? "./a"
    # FileTree.handles? "foo"
    def self.handles? list=nil

      if list.nil?
        list ||= Tree.construct_path(:list=>true) rescue nil   # Use current line by default
      end

      list = list.first if list.is_a?(Array)

      return 0 if self.matches_root_pattern?(Line.without_label :line=>list)
      nil
    end

    # FileTree.matches_root_pattern? "%f"
    # FileTree.matches_root_pattern? "%f/blaa"
    def self.matches_root_pattern? item

      item == "." ||   # just ~ or .
      item == "~" ||   # just ~ or .
      item =~ /^\/(\w|$)/ ||   # /... or /
      item =~ /^%\w/ ||   # %bookmark

      item =~ %r"^(~/|\.\.?/)"

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
      if options[:prefix] != "update" && options[:task] != ["save"] && options[:task] != ["create"]
        return "* create"
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

      "<* - saved!"

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
      diff = Shell.sync %`diff --old-group-format="d%df %dn%c'\012'" --new-group-format="a%dF %dN%c'\012'" --unchanged-line-format="" "#{View.file}" "#{tmp_path}"`

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
          Shell.sync command, :dir=>"/tmp/"
        end
      else
        operations[1].each do |o|
          command = "rm \"#{o}\""
          Shell.sync command, :dir=>"/tmp/"
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
          candidate = ": #{line}\n"

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

    def self.select_next_file options={}
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


      # Removed this > might cause problems

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
        View.to_buffer "#{name}/"
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
        Tree.filter(:recursive=>true, :left=>left, :right=>right)
      else
        # Insert with linebreak if file
        if File.file?(dir)
          View.insert self.filename_to_next_line(dir)  # Add linebreak before filename
          open_line 1
          Line.to_words
          Tree.filter(:left => Line.left, :right => Line.left(2))
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
        return
      end

      unless View.bar?   # If already open, just go there
        View.bar
      end
      View.to_nth 0
      $el.find_file Bookmarks.expand("%n")

      only_one_view_in_bar = prefix == :u
      only_one_view_in_bar = ! only_one_view_in_bar if @@one_view_in_bar_by_default
      unless only_one_view_in_bar  # Unless u prefix, open $tl as well (under bar)

        # If 2nd view isn't at left margin, open 2nd view
        if View.left_edge(View.list[1]) != 0
          View.create_horizontal
        end

        View.to_nth 1

        # If 2nd view isn't :n, open it
        if $el.buffer_file_name( $el.window_buffer( View.list[1] ) ) != Bookmarks["%links"]
          $el.find_file Bookmarks["%links"]
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
        dir = File.dirname file
        dir = Files.tilde_for_home dir

        txt = "#{dir}/\n  - #{File.basename(file)}\n" +
          txt.gsub(/^/, "    : ").
          gsub(/^    \: $/, "    :")   # Remove trailing spaces on blank lines
        "#{txt}\n"
      else
        "- From #{View.name}:\n" + txt.gsub(/^/, "  : ")
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
      Tree.filter(:recursive => true, :left => left, :right => right)
    end

    # New Unified way to show list of dirs.
    def self.expand_dir_recursively options

      dir = options[:file_path]

      indent = ""

      # Get file tree
      t = self.new
      dir.sub!(/\/$/, '') if dir != "/"

      t.traverse Bookmarks.expand(dir)

      # Adjust indent
      result_indent = t.res[/^ */]   # Get indent of first line
      txt = t.res.gsub(/^#{result_indent}/, "")

      return txt
    end


    def self.dir_one_level options={}

      Line.to_left
      indent = "#{Line.indent}  "

      dir = Bookmarks.expand(Tree.construct_path)

      # If C-2, just open in dired
      if Keys.prefix == 2
        # TODO: Open in 1st window
        View.to_after_bar
        $el.find_file dir
        return
      end

      dirs, files = self.files_in_dir(dir, options)   # Get dirs and files in it

      if files.empty? && dirs.empty?
        if ! File.exists? dir   # If doesn't exist, show message
          return Tree << "* create dir/"
          # - =mkdir/
          # | ...dir '#{dir}' doesn't exist.  Create it?
        end
        View.flash "- Directory is empty"#, :times=>2
        Move.to_end
        Notes.enter_junior
        View << "- "
        return
      end

      # Change path to proper indent
      dirs.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1/")}
      files.collect!{|i| i.sub(/.*\/(.+)/, "#{indent}+ \\1")}

      Line.next
      left = View.cursor

      # Move .notes files to top

      both = options[:date_sort] || options[:size_sort] ?
        files + dirs : dirs + files
      View.insert(both.join("\n") + "\n")
      right = View.cursor
      View.cursor = left

      Line.to_beginning

      if options[:focus]
        Search.forward "^ +\\+ #{options[:focus]}$"
        Line.to_beginning
        Color.mark "light"
      end

      Tree.filter(:left=>left, :right=>right, :always_search=>true)
    end

    # New Unified way to show one dir.
    def self.expand_one_dir options

      dir = options[:file_path]
      dirs, files = self.files_in_dir dir, options   # Get dirs and files in it

      if files.empty? && dirs.empty?
        if ! File.exists? dir   # If doesn't exist, show message
          return "* create dir/"
        end

        return "<* Directory is empty"
      end

      # For now, try bullets for both
      dirs.collect!{|i| i.sub(/.*\/(.+)/, "+ \\1/")}
      files.collect!{|i| i.sub(/.*\/(.+)/, "+ \\1")}

      # Figure out > why no :date_sort when normal dir expand

      # Temporarily always do dirs first > see how it feels
      both = files + dirs

      result = both.join("\n") + "\n"

      # TODO How to deal with focus? - return option telling them line to put cursor on - or, maybe, make that happen somehow on the other side of the call?
      #     if options[:focus]
      #       Search.forward "^ +\\+ #{options[:focus]}$"
      #       Line.to_beginning
      #       Color.mark "light"
      #     end

      result

    end


    # Enter what's in clipboard with | to on the left margin, with appropriate indent
    def self.enter_quote txt=nil, options={}

      prefix = Keys.prefix :clear=>1

      # Skip forward if on heading
      Line.to_left

      if Line[/^[:|]/]
        Move.to_end
        $el.newline
      end

      txt ||= Clipboard.get(0, :add_linebreak=>1)

      dir = Tree.construct_path rescue nil

      # Current line is dir path...
      if self.dir? && self.handles?
        Tree.plus_to_minus_maybe
        indent = Line.indent
        dir = Tree.construct_path
        t = Clipboard.get("=")
        t = t.gsub(/^#{dir}/, '')
        if t.sub!(/\A\n/, '')   # If no dir left, indent one over
          t.gsub!(/^  /, '')
        end
        Tree.add_pluses_and_minuses t, '-', '-'
        Line.next
        View.insert "#{t}\n".gsub(/^/, "#{indent}  ")
        return
      end

      # Empty line, so just enter tree...

      if Line.blank?

        if prefix == :u || txt =~ /\A *[-+]?[:|][-+ ]/   # If C-u or whole thing is quoted already, unquote
          txt = txt.split("\n").grep(/:|/).join("\n")
          return View.insert(txt.gsub(/^ *[:|]([-+ ]|$)/, ""))   # Remove | ..., |+...., |<blank>, etc
        end

        start = View.cursor
        txt = Clipboard.get("=")
        indent = prefix || 0   # Indent prefix spaces, or 2

        txt = txt.gsub(/^/, " " * indent)

        View.insert txt
        $el.set_mark(Line.left(2))
        $el.goto_char start
        return
      end

      # Dash+, so start with blank... ??

      # Line has content, so indent under...

      txt = txt.unindent unless options[:leave_indent]

      indent = Line.indent   # Get current indent

      target_quote = Line[/\A *([|:!])/, 1]
      existing_quote = txt[/\A *([|:!])/, 1]

      Line.next

      if existing_quote && ! options[:leave_quote_chars]
        txt.gsub!(/^ *[|:!] ?/, '')
      end

      if ! target_quote
        # Indent one level further unless on comment already
        indent = "#{indent}  "
      end

      indent += " " * Keys.prefix_or_0   # If numeric prefix, add to indent
      txt = txt.sub /\n+\z/, ''   # Remove last \n

      quote = target_quote || existing_quote
      quote ||= ':' if options[:char] || prefix == :u || prefix == :-
      quote ||= '|'

      quote = options[:char] if options[:leave_quote_chars]

      txt = txt.gsub /^/, "#{indent}#{quote} "
      txt = txt.gsub /^( *[|:#]) $/, "\\1"   # Remove trailing spaces

      if prefix == :-
        View.insert "#{indent}#{quote} \n"
        Move.backward
        return
      end

      View.insert "#{txt}\n"
    end

    def self.enter_as_search
      indent = Line.indent
      Move.to_end
      View.insert "\n#{indent}  - ###{Clipboard["0"]}/"
      Launcher.launch
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
      elsif extension =~ /^notes|deck|xiki$/
        # "^[\\|>]( |$)"
        "^>( |$)"
      else
        "^[^ \\t\\n]"
      end
    end


    # Called by to+outline and enter+outline, (others?).
    #
    # ...and @outline________
    #
    # Don't insert, just return
    # p FileTree.enter_lines /foo/, :just_return=>1, :path=>View.file
    def self.enter_lines pattern=nil, options={}

      # If prefix is 6, delegate to Git to enter methods by date
      if options[:prefix] == 6
        txt = Git.methods_by_date(Tree.path[-1]).join("\n")
        Tree.<< Tree.quote(txt), :no_slash=>1
        return
      end

      # If dir, delegate to C-. (they meant to just open it)
      return Launcher.launch if self.dir?

      Tree.plus_to_minus

      if Line.blank?   # If blank line, get bookmark and enter into current file

        path =
          if options[:path]
            options[:path]
          else
            bm = Keys.input(:timed=>true, :prompt=>"Enter bookmark to show outline for: ")
            if bm == "."
              View << "outline/"
              Launcher.launch
              return
            end

            File.expand_path Bookmarks.expand(bm, :just_bookmark=>1)
          end

          #Files.directory? Bookmarks.expand("h", :just_bookmark=>true)

        # If it's a dir, delegate to Open Tree
        if File.directory? path
          self.ls :here=>true, :dir=>path, :recursive=>1
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

      # commit: Works? Get enter+lines to work with remote files
      if path =~ /^\/\w+@/
        contents = Remote.file_contents path
        Tree.under contents, :escape=>': ', :no_slash=>1
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

        # .rb or .js file, so extract embedded xiki notes > To show in outline > "> ...."

        if ["rb", "js"].member?(extension) && line =~ /^ *(#|\/\/) > ./
          matches_count += 1
          if line =~ /^ *(#|\/\/) > \./
            heading = line[/\.(.+)/, 1]
            path = Notes.heading_to_path heading
            next matches << "    + #{path}\n"
          end
          heading = line[/>.+/]
          next matches << "    #{heading}\n"
        end

        next unless line =~ pattern

        line = line == "" ? "" : " #{line}"
        line.sub! /^ > $/, ' >'
        quoted_line = "#{indent}#{indent_more}:#{line}\n"

        # If :no_dups and a dup, skip
        if options[:no_dups] && matches.end_with?(quoted_line)
          next
        end

        matches << quoted_line

        matches_count+=1
      end

      matches = matches.split("\n").uniq.join("\n")+"\n" if options[:unique]

      return matches if options[:just_return]

      Tree.insert_quoted_and_search matches, :line_found=>line_found
    end

    def self.enter_image image
      tmp_dir = "/tmp/insert_image"
      Dir.mkdir tmp_dir if ! File.exists? tmp_dir

      width, height = Shell.sync("identify \"#{image}\"").match(/(\d+)x(\d+)/)[1..2]
      max = 300
      if width.to_i > max || height.to_i > max
        dest = tmp_dir+"/"+File.basename(image).sub(".", "_#{max}.")
        Shell.sync %`convert "#{image}" -resize #{max}x#{max} "#{dest}"`, :dir=>"/tmp"
      else
        dest = image
      end

      Image.>> dest

      Line.previous
      Line.to_beginning

      nil
    end

    def self.file_not_found_from_template path

      # If .../roots/foo.rb

      if path =~ /.*\/roots\/(.+)\.rb$/   # "
        name = $1
        txt = File.read "#{Xiki.dir}/misc/templates/menu_template.rb"
      elsif path =~ /.*\/(.+)_spec\.rb$/   # "
        name = $1
        txt = File.read "#{Xiki.dir}/misc/templates/template_spec.rb"
      else

        name, extension = path.match(/([^\/]+)\.(.+)$/)[1..2] rescue [nil, nil]
        extension = path.match(/[^\/]+\.(.+)$/)[1] rescue nil

        [File.expand_path("~/xiki_stuff/templates/"), File.expand_path("~/.xiki/misc/templates")].each do |dir|
          file = "#{dir}/template.#{extension}"
          next unless File.exists? file
          txt = File.read file
          break
        end
      end

      if txt
        View.flash "- File doesn't exist, start with this...", :times=>4
        txt.gsub!(/\{\{(.+?)\}\}/) { eval $1 }   # Expand {{these}}
        raise "- Commenting this could cause problems!!!"
        return Tree.<< txt, :escape=>": ", :no_slash=>1
      end

      # If no templates matched
      View.flash "- __File doesn't exist!", :times=>2
      Tree.<< ":", :no_slash=>1
      Move.to_end
      View << ' '
    end

    # Mapped to shortcuts that displays the trees
    # enter+tree, do+tree
    def self.tree options={}

      prefix = Keys.prefix :clear=>true

      options.merge!(:recursive=>1) if prefix == :u   # If up+, expand dir recursively

      if prefix == :-   # If up+up+, insert /the/path//
        Line << "#{Bookmarks["%#{Keys.input(:timed=>1)}"]}/"   # "
        Launcher.launch_or_collapse
        return
      end

      dir =
        if options[:dir]
          options[:dir]
        else

          bm = options[:bm] || Keys.input(:timed=>true, :prompt=>"Enter bookmark to show tree of: ")

          options.merge!(:focus=>View.file_name) if bm == "."
          bm = Keys.bookmark_as_path :bm=>bm, :include_file=>1   # Change . to current dir, etc
          Files.tilde_for_home bm
        end

      # If file, delegate to FileTree.enter_lines...

      # commit: open+bookmark+slash
      dir = "/" if dir == :slash

      if File.file? File.expand_path(dir)
        # Commenth this out because of with+file > Might cause problem when inserting all?

        History.open_current :file=>dir, :outline=>1

        return
      end

      # If dir, delegate to .ls...

      return if dir == :bookmark_doesnt_exist

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

    # Returns the files in the dir
    # FileTree.files_in_dir "/tmp/d", :date_sort=>1
    def self.files_in_dir dir, options={}
      dir = FileTree.add_slash_maybe dir

      all = Dir.glob("#{dir}*", File::FNM_DOTMATCH).
        select {|i| i !~ /\/\.(\.*|svn|git|DS_Store)$/}.   # Exclude some dirs (exclude entensions here too?)
        select {|i| i !~ /\/\.#/}

      # Sort by date, size, or alphabetical...

      all = all.sort{|a, b|
        a_time = File.mtime(a) rescue nil
        b_time = File.mtime(b) rescue nil
        next 0 if ! a_time || ! b_time
        b_time <=> a_time
      }

      return all if options[:merged]

      dirs, files = all.partition{|f| File.directory?(f)}

      [dirs, files]

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

      path = Bookmarks[path]

      if dir_only
        path = path =~ /\/$/ ? path : File.dirname(path)+"/"
      end
      path
    end

    # Grab file path on current line (climbs tree if there is one).
    # Returns nil if none or not a file
    def self.grab_file_on_line
      return nil if ! self.handles?

      Tree.construct_path
    end

    # Indent txt to be one level lower than current line
    def self.one_view_in_bar_by_default= to
      @@one_view_in_bar_by_default = to
    end

    def self.copy_path options={}

      # Return dir of view's file if at left margin, U, or not ^[|@-]
      if Line !~ /^ *[~\/|=+-]/
        path = View.file
      else
        path = Tree.path.last   # Grab from wiki tree

        path.sub! /\/$/, '' if Line.value !~ /\/$/   # If current line doesn't have trailing slash, remove it
      end

      FileTree.extract_filters! path

      View.flash "- copied: #{path}", :times=>2
      Clipboard["0"] = path
      nil
    end

    # Adds extra line if we're at the end of the file.
    # If there's no linebreak, it causes weird errors.
    def self.extra_line_if_end_of_file
      if Line.right == View.bottom
        Line.to_right
        $el.open_line(1)
      end
    end

    # Add slash to the variable
    def self.add_slash_maybe dir
      dir =~ /\/$/ ? dir : "#{dir}/"
    end

    # Add slash to the variable
    def self.add_slash_maybe! dir
      if dir !~ /\/$/
        dir.replace "#{dir}/"
      end
      dir
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
    def self.cd path
      Shell.cd path
      View.flash "- current dir set", :times=>1
    end

    def self.delete_file path, options={}

      # Still has dependencies on editor.  First step for if we want to delete
      # from a non-editor client - make below editor lines just not run when
      # not :client =~ ^editor\b.

      return View.flash("- File doesn't exist: #{path}", :times=>5) if ! File.exists?(path)

      if ! options[:no_prompt]
        View.flash "- Delete file for sure?"
        answer = Keys.input :chars=>1, :prompt=>"For sure delete?: #{path}"   #"
        return unless answer =~ /y/i
      end

      executable = File.directory?(path) ? "rm -r" : "rm"
      command = "#{executable} \"#{path}\""

      result = Shell.run command, :sync=>true
      if (result||"").any?
        View.beep
        View.message "#{result}"
        return
      end

      # Eventually only do if :client =~ ^editor\b

      Tree.collapse
      Effects.glow :fade_out=>1
      Line.delete
      Line.to_beginning

      # Remember to delete again when C-. pressed...

      View.message "File deleted"

    end

    def self.move_latest_screenshot_to dest_path, dest_dir
      desktop = Bookmarks['%dt']

      latest_screenshot = `ls -t #{desktop} "Screen shot*"`[/Screen shot .*/]

      return View.message("No screenshot found.") if latest_screenshot.empty?

      command = "mv \"#{desktop}#{latest_screenshot}\" \"#{dest_path}\""

      result = Shell.run command, :sync=>true
      View.message result
    end

    def self.move_to
      self.copy_to(:move=>1)
    end

    def self.move_up
      # Moves line to above all siblings
      prefix = Keys.prefix
      line, column, indent = View.line, View.column, Line.indent

      txt = Line.delete
      Search.backward "^#{indent.sub '  ', ''}[^\t \n]"
      Line << "\n#{txt}"
      Line << "\n" if prefix == :u
      View.line, View.column = line+1, column
      nil
    end

    def self.copy_current_file_to options={}

      return View.beep '- Save the file first!' if View.modified?

      # Go from current file to bookmark
      source_path = View.file
      View.flash "- to which bookmark?"
      verb = options[:move] ? "Move" : "Copy"
      dest_path = Keys.bookmark_as_path :prompt=>"#{verb} current file to which bookmark? "
      return if ! dest_path.is_a? String

      dest_path = Bookmarks[dest_path]

      executable = options[:move] ? "mv" : "cp -r"

      command = "#{executable} \"#{source_path}\" \"#{dest_path}\""

      result = Shell.run command, :sync=>true

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

      # Error if not in a file tree

      return self.copy_current_file_to options if ! FileTree.handles?

      dest_path = Tree.construct_path
      dest_path = Bookmarks[dest_path]

      dest_dir = dest_path.sub(/(.+\/).*/, "\\1")   # /dir/file -> /dir/
      slash = dest_dir =~ /\/$/
      dest_dir = File.expand_path dest_dir
      dest_dir = "#{dest_dir}/" if slash

      if prefix == 2
        self.move_latest_screenshot_to dest_path, dest_dir
        return View.flash "- Moved the latest screenshot to here!"
      end

      arg = options[:move] ? {:delete=>1} : {}
      source_path = Tree.file_at_spot arg

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

      result = Shell.run command, :sync=>true

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

      if ! dest_is_dir   # If copying to existing file, do something visually distinctive
        View.flash options[:move] ? "- moved!" : "- copied!"
      end
    end

    # Just puts "=rename/" underneath item the cursor is on...
    def self.command_on_filename command

      Tree.<< "#{Line.indent}= #{command}/", :no_slash=>1, :no_search=>1
      Move.to_end

      return ""

      # Old implementation > it gave just the extension as a starting point

      # Duplicate line, putting =rename after indent...

      line = Line.value

      has_dot = line =~ /\./
      has_dot ?
        line.sub!(/^( *)([+-] )?.+(\..+)/, "\\1=#{command}/\\3") :
        line.sub!(/^( *)([+-] )?.+/, "\\1=#{command}/")

      Tree.<< line, :no_slash=>1, :no_search=>1
      Move.to_end
      Search.backward("\\.") if has_dot   # Move cursor to before extension

      ""

    end

    def self.open_as_upper where=false
      orig_u = Keys.prefix_u
      view = View.current
      path = Tree.path[-1]
      FileTree.extract_filters! path

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

      Launcher.open_file path
      View.to_window(view) if orig_u
    end

    def self.to_outline options={}
      prefix = options[:prefix] || Keys.prefix(:clear=>true)

      # @x/x/x view, so use "@u/..." to show outline...

      name = View.name

      if xikihub_view = name[/^xiki\.\w+\/(@.+)/, 1]
        Launcher.open xikihub_view
        return
      end

      path = View.file

      if path
        dir, file = path.match(/(.+\/)(.+)/)[1..2]
        extension = File.extname(path).sub(".", "")
      end

      link_filename = Notes.expand_link_file(path)

      if ! options[:not_topic_outline]

        # In a ~/xiki/foo.xiki file, so expand topic instead of showing file outline...

        if extension == "xiki" && dir == File.expand_path("~/xiki")+"/" || link_filename

          file = link_filename if link_filename   # Use link > if it exists
          topic = File.basename(file, ".*").gsub("_", " ")
          Line.value
          View.name

          # Jump up to heading and grab it, so we can move the cursor to it

          cursor = View.cursor
          Line.to_right
          Search.backward "^> "
          original_heading = Line.value

          # Remove @ or @foo
          TopicExpander.delete_username_from_heading original_heading

          View.cursor = cursor

          Launcher.open topic, :bar_is_fine=>1, :original_heading=>original_heading, :prefix=>prefix
          return

        elsif extension == "xiki"

          # .xiki file in other dir, so use "/dir/foo//" for outline

          path = Files.tilde_for_home path

          path = path.sub(/\.xiki$/, "//")

          Launcher.open path, :bar_is_fine=>1
          return
        end
      end

      if (prefix == :u || prefix == :-) && ! Notes.enabled?
        args = [View.txt, View.line]
      end
      current_line = Line.number
      mode = View.mode

      View.to_buffer("outline/")
      View.clear
      Notes.mode

      View.wrap :off

      if path
        txt = "#{dir}\n  - #{file}\n"
        View.insert txt
        View.to_top
        self.select_next_file
      else
        View.insert "buffer/\n"
        View.to_top
      end

      case prefix
      when 2   # Prompt to add @... menu under file
        Move.to_end
        Launcher.insert_menu
      when 3   # Prompt to add @... menu under file
        Line << "\n    =outline/history/"
        return
      when 4   # Get ready to run $... shell command on file
        $el.delete_char(1)
        View << "$ "
      when 5   # Unique
        self.enter_lines nil, :current_line=>current_line, :unique=>1
      when 6   # List methods by date (use 'git blame')
        self.enter_lines nil, :prefix=>6
      when 7
        Move.to_end
        View << "\n    @info"
        Launcher.launch
      when 8
        Launcher.enter_all

      when 9
        self.enter_lines(/^> (feature|important) > /i, :current_line=>current_line)

      when 0
        # Just show path if 0+...
      when :u   # Just show # foo... comments...
        if mode == :notes
          self.enter_lines /^> .*:$/, options.merge(:current_line=>current_line)
          return
        end
             # when :uu   # Just show # foo... comments...
        self.enter_lines(/(^ *(def|function) |(^| )(#|>|"|\/\/) .+\.\.\.$)/, :current_line=>current_line)
      when :-   # Show outline down to current line
        if mode == :notes
          self.enter_lines /^> .*:$/, options.merge(:current_line=>current_line)
          return
        end

        txt, line = Search.deep_outline *args
        Tree.<< txt, :line_found=>line, :escape=>': ', :no_slash=>1
      else
        self.enter_lines nil, options.merge(:current_line=>current_line)
      end

    end

    def self.skip_dirs dir, skip_these
      dir = Bookmarks[dir].sub /\/$/, ''
      (@skip ||= {})[dir] = skip_these
    end

    def self.expands? options   #> nil
      return unless options[:file_path]
      (options[:expanders] ||= []).push self
    end

    def self.expand options

      # Ctrl+X, so delegate to extension notes
      if options[:ctrlx]
        return TopicExpander.expand_by_extension options
      end

      #> Might cause problems > added items to file path!

      prefix, task = options[:prefix], options[:task]
      file_path = options[:file_path]

      #> Might cause problems > added items to file path!

      if items = options[:items]
        file_path << items.join("/")
      end

      # If ##foo/ or **foo/ filter at end (removing all as we check)...

      # Removes ## and ** filters, returning any that were at the end   #> ["##dir, name"]
      filters_at_end = self.extract_filters! file_path

      # If right-click, set prefix to item...

      if filters_at_end
        txt = self.expand_filter(filters_at_end, options)   #> {:client=>"editor/emacs", :target_view=>"tree filter", :dir=>"/Users/craig/Dropbox/xiki/", :file_path=>"/Users/craig/Dropbox/xiki/", :expanders=>[Xiki::FileTree], :expanders_index=>0, :no_slash=>1}
        return options[:output] = txt
      end

      # If quoted line, pull it off into :quote...

      # Handling |... like a quote > If it has a quote > Ask for @replace... here?!
        # "@replace/pipe as quote"

      # If a linebreak (means pipes?), tell them to use colons...

      # > Todo > Don't complain if linebreaks are nested under heading!

      if options[:client] =~ /^editor/ && Path.unescape(file_path) =~ /\n/

        # Multi line items underneath embedded xiki commands are allowed
        file_with_embedded, args = self.extract_prepended_file_path file_path

        # if ! args || Path.split(args[0]) =~ /\n/
        #   return options[:output] = "
        #     > Use colons, not pipes
        #     : Use colons at the beginnings of lines to navigate
        #     : to those lines in files, not pipes.  (Colon-quoted lines under
        #     : files now mean to navigate.)  In a future Xiki version,
        #     : pipe-quoted lines under files will mean to over-write the file
        #     : contents with the pipe lines.
        #     "
        # end

      end

      # Maybe only do if client is editor
        # And maybe check actual line?

      # Where path coming from > why not escaped the second time

      if quote = Path.extract_quote(file_path)
        ControlTab.dash_prefix_buffer = View.name
        # options[:quote] = Path.unescape(quote)
        options[:quote] = quote
      end

      if line_number = Path.extract_line_number(file_path)
        options[:line_number] = line_number
      end

      # If it's an existing file...

      if File.file? file_path
        options[:no_slash] = 1
        extension = File.extname file_path

        # Task, so return or process the task...

        if task
          if options[:quote]
            # : foo/~ save, so do save
            return options[:output] = self.save(file_path, options) if task == ["save"]
            # : foo, so show "~ save/"
            return options[:output] = "* save"
          end

          # File tasks menu...

          menu = %`
            * search
              ! Search.enter_search
            * contents
              ! txt = File.read options[:file_path]
              ! txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')
              ! Tree.quote txt
            * outline
              ! FileTree.filter_one_file(options[:file_path]).join("\\n")

            * file/
              + rename
                ! FileTree.command_on_filename "rename"
              + copy
                ! FileTree.command_on_filename "copy"
              + copy group
                ! siblings = Tree.siblings
                ! Clipboard["0"] = "{\#{siblings.join(",")}}"
                ! ""
              + prompt here
                ! Tree.<< "$ ", :no_search=>1, :no_slash=>1
                ! Move.to_end

              + touch
                ! #options[:file_path]
                ! FileUtils.touch options[:file_path]
                ! "<* - touched file"
              + bookmark
                ! Bookmarks.save
              + restore version
                ! latest_file = History.latest_file File.basename(options[:file_path])
                ! "todo > implement this > replace latest file to it if exists > probably confirm first"
            * edit with/
              + sublime
                ! Shell.command "subl \#{options[:file_path]}"
                ! Ol "options[:file_path]", options[:file_path]
                ! ""
              + vim
                ! DiffLog.quit_and_run "vim \#{options[:file_path]}"
                ! ""
              + emacs
                ! # Probably delete -Q?
                ! DiffLog.quit_and_run "emacs -nw \#{options[:file_path]}"
              + default editor
                ! DiffLog.quit_and_run "\#{FileTree.default_external_editor} \#{options[:file_path]}"
                ! # DiffLog.quit_and_run "\#{ENV['EDITOR']} \#{options[:file_path]}"
              + xsh
                ! Ol.a options
                ! "- todoz!"
                ! View.open options[:file_path]
            * delete
              ! Keys.remember_key_for_repeat(proc {Launcher.launch :task=>["delete"], :no_prompt=>1})
              ! FileTree.delete_file options[:file_path], options
            * path to clipboard
              ! FileTree.copy_path
            * with path/
          `.unindent

          clipboard_menu = self.clipboard_menu

          menu.sub!("* with path/\n", clipboard_menu)

          menu = Xik.new menu

          task[0] = "* #{task[0]}" if task[0]

          result = menu.expand task, :eval=>options
          return options[:output] = result || ""

        end

        # If editor, tell it to open the file...

        if options[:client] =~ /^editor\b/
          txt = "=open file/#{file_path}"

          txt << "/:#{options[:line_number]}" if options[:line_number]   # If a quote, pass line number after a colon!

          # Should be this!
          txt << "/|#{Path.escape options[:quote]}" if options[:quote]   # If a quote, pass line number after a colon!

          return options[:output] = txt
        end

        # If API, return contents...

        options[:returned_file_contents] = 1
        return options[:output] = File.read(file_path)
        # If not editor, |... will be ignored for now.  Any reasons would api pass this in?  Probably to create non-existant files?  Maybe grab lines indented under it?  Maybe return line number of it?
      end

      # If an existing dir...

      if File.directory? file_path
        FileTree.add_slash_maybe! file_path

        if task

          require "#{Xiki.dir}roots/sample_menus/sample_menus_index.rb" # if !defined?(SampleMenus)

          if task == ["rake"]
            options[:nest] = 1
            options[:no_task] = 1
            rake = Shell.command("rake -T -A", :dir=>file_path)
            rake.gsub!(/ $/, '')
            return options[:output] = Tree.quote(rake)
          elsif task[0] == "rake"
            rake_task = task[1][/rake (\w+)/, 1]
            return DiffLog.quit_and_run "rake #{rake_task}"
          end

          # Dir tasks menu...

          # ~ examples/
          #   ! FileTree.example_commands options
          # menu = Xik.new menu
          # ~ modified date sort
          #   ! FileTree.dir :date_sort=>true
          #   ! ""
          menu = %`
            * search
              ! Tree.<< "- ##", :no_search=>1
              ! #View.column = -1
              ! Line.to_right
              ! ""
            * all files
              ! FileTree.expand_dir_recursively :file_path=>options[:file_path]
            * delete
              ! Keys.remember_key_for_repeat(proc {Launcher.launch :task=>["delete"], :no_prompt=>1})
              ! FileTree.delete_file options[:file_path], options

            * recent commands/
              ! options[:nest] = 1
              ! options[:no_task] = 1
              ! txt = Shell.external_plus_sticky_history
            * recent in dir/
              ! options[:nest] = 1
              ! options[:no_task] = 1
              ! Shell.history options[:file_path]
            * file/
              + rename
                ! FileTree.command_on_filename "rename"
              + prompt here
                ! Tree.<< "$ "
                ! Line.to_right
                ! ""
              + bookmark
                ! Bookmarks.save

              + cd
                ! FileTree.cd options[:file_path]
              + touch
                ! FileUtils.touch options[:file_path]
                ! "<* - touched file"

              + filenames search
                ! ControlLock.disable
                ! Tree.<< "- **/", :no_search=>1
                ! View.column = -1
                ! ""
              + all contents
                ! txt = FileTree.grep(options[:file_path], "").join("\\n")
                ! txt.sub(/.+\\n/, "")   # Delete 1st line > the redundant dir

              + exit and cd
                ! Shell.exit_and_cd options[:file_path]
              + make/
                + some files and dir
                  ! options[:no_search] = 1
                  ! File.write "\#{options[:file_path]}/a.txt", "aaa\\naaa\\n"
                  ! File.write "\#{options[:file_path]}/b.txt", "bbb\\nbbb\\n"
                  ! File.write "\#{options[:file_path]}/c.xiki", "> heading\\nccc\\nccc\\n\\n> .another\\n! args\\n"
                  ! Dir.mkdir("\#{options[:file_path]}/d") rescue nil
                  ! File.write "\#{options[:file_path]}/d/d.txt", "ddd\\nddd\\n"
                  ! "- a.txt\\n- b.txt\\n- c.xiki\\n- d/\\n  - d.txt"
                + a few files
                  ! options[:no_search] = 1
                  ! File.write "\#{options[:file_path]}/a.txt", "aaa\\naaa\\n"
                  ! File.write "\#{options[:file_path]}/b.txt", "bbb\\nbbb\\n"
                  ! File.write "\#{options[:file_path]}/c.xiki", "> heading\\nccc\\nccc\\n\\n> another\\nc c c\\n"
                  ! "- a.txt\\n- b.txt\\n- c.xiki"
                + txt
                  ! "+ foo.txt\\n\#{Tree.quote SampleMenus.by_extension("txt"), :indent=>"  "}"
                + rb
                  ! "+ foo.rb\\n\#{Tree.quote SampleMenus.by_extension("rb"), :indent=>"  "}"
                + py
                  ! "+ foo.py\\n\#{Tree.quote SampleMenus.by_extension("py"), :indent=>"  "}"
                + js
                  ! "+ foo.js\\n\#{Tree.quote SampleMenus.by_extension("js"), :indent=>"  "}"
                + dir
                  ! "+ foo/"

            * path to clipboard
              ! FileTree.copy_path
            * with path/
          `.unindent

          menu << "\n"

          # Append dir tasks...

          clipboard_menu = self.clipboard_menu
          menu.sub!("* with path/\n", clipboard_menu)

          # / and :mouse, so return whole menu...

          # return options[:output] = menu.txt_without_code if task == [] && options[:mouse]

          # /something, so expand menu...

          task[0] = "* #{task[0]}" if task[0]
          menu = Xik.new menu
          result = menu.expand task, :eval=>options

          options[:nest] = 1 if [["* dir"], ["* more"]].member?(task)

          return options[:output] = result || ""

          # If no output, it should just continue on...
          # Might cause problems?

        end

        # No task, so just expand dir...

        return options[:output] = self.expand_one_dir(options)
      end


      # A file or dir that doesn't exist?...

      # Parent is a file, so must be a file note

      ancestor_file = Files.ancestor_file_or_directory file_path

      # if File.file? File.dirname(file_path)

      if File.file? ancestor_file
        return options[:output] = TopicExpander.expand_by_extension(options)   #> |||
      end


      # ~ create dir, so create it...

      if task == ["create dir"]
        FileUtils.mkdir_p options[:file_path]
        return options[:output] = "<* created!"
      end

      # Non-existing dir (whether task or not), so show "* create dir"...

      return options[:output] = "* create dir/" if options[:file_path] =~ /\/$/

      # Quote means to create the file (with or without "update" prefix)...

      return options[:output] = self.save(file_path, options) if options[:quote]

      # File path (no quote)...

      # If enter+all, do what?
      if prefix == "all" || prefix == "outline"
        options[:no_slash] = 1
        return options[:output] = ": file doesn't exist!"
      end

      # task, so show options for non-existant file...

      return self.tasks_for_new_file options if task

      # if task == ""
      #   return options[:output] = "~ create/"
      # elsif task == "create"
      #   return options[:output] = "
      #     : Content of the
      #     : file to create...
      #     "
      # end

      # Not existing file or dir, so just open new file

      # If .rb file with embedded xiki notes, Expand them
      return if self.expand_embedded_notes options   #> ||

      options[:output] = "=open file/#{file_path}"

      # What about when :client !~ ^editor
      # - can probably assume there won't be an update prefix
      #   - so, what will it do?  Maybe it'll send a multiline string if it wants to save?  Probably worry about this later?

    end

    #
    # > Test .extract_prepended_file_path
    # ! p FileTree.extract_prepended_file_path "/tmp/dir/foo.rb/> Something/else"
    #   : ["/tmp/dir/foo.rb", "> Something/else"]
    #
    # ! p FileTree.extract_prepended_file_path "/tmp/dir/foo.txt/> Something/else"
    #   : nil
    #
    def self.extract_prepended_file_path file_path

      # See if the path is a foo.js|rb plus other stuff (args)

      extensions = [".rb", ".js"]
      extensions_regex = "(?:#{extensions.join('|').gsub(".", "\\.")})"

      match = file_path.match(/^(.+?\w#{extensions_regex})\/(.+)/)

      return if ! match

      file_with_embedded, args = match[1..2]
    end

    #
    # > Test .expand_embedded_notes
    # ! FileTree.expand_embedded_notes options
    #
    def self.expand_embedded_notes options

      # For now, just assume .rb or .js files



      # > Todo > extract > to self.extract_prepended_file_path
      file_with_embedded, args = self.extract_prepended_file_path options[:file_path]

      return false if ! file_with_embedded

      args = Path.split args

      # Do nothing if no existing rb file

      # Return false if we didn't handle
      return false if ! File.exists? file_with_embedded

      # Extract xiki notes from file

      txt = File.read file_with_embedded
      txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')
      txt = self.extract_embedded_notes txt

      # Expand action if it is one...

      options_in = {}

      txt = Notes.drill txt, args[0], options_in   #> |||

      # Ctrl+X, so just navigate
      if options[:ctrlx] || (args[0] =~ /^>/ && args[1] =~ /\n/)

        # Handle navigating manually
        View.open file_with_embedded
        heading = options_in[:heading_found]

        Search.forward "^ *\\(#\\|//\\) #{Search.quote_elisp_regex heading}", :from_top=>1, :beginning=>1

        return true
      end

      options_in[:items] = args
      Tree.unquote(txt)

      if args[0] !~ /^>/
        txt = TopicExpander.expand_action(Tree.unquote(txt), nil, options_in)
      end

      options[:no_slash] = 1
      options[:output] = txt

      # Return true since we handled
      return true

    end


    #
    # > Next steps?
    # - Maybe a little of this
    # - Maybe a little of that
    #
    # > .Test .extract_embedded_notes
    # ! txt = "
    # !   Some code
    # !   # > Hi
    # !   # Hello
    # ! ".unindent
    # ! FileTree.extract_embedded_notes txt
    #
    # > .Test .extract_embedded_notes on javascript
    # ! txt = "
    # !   Some code
    # !   // > Hi
    # !   // Hello
    # ! ".unindent
    # ! FileTree.extract_embedded_notes txt
    #
    def self.extract_embedded_notes txt

      txt = txt.split("\n")

      result, accumulating = "", false
      txt.each do |line|
        if line =~ /^ *(#|\/\/) > ./
          # Heading line, so start accumulating
          accumulating = true
        elsif line =~ /^ *(#|\/\/)/
          # Comment line, so keep accumulating if doing so already
        else
          # Other line, so stop accumulating
          accumulating = false
        end

        result << "#{line.sub(/^ *(#|\/\/) ?/, '')}\n" if accumulating
      end
      result

    end


    def self.expand_file_extension_options command

      Tree << "= #{command}"

      # foo/bar, so just insert and run
      if command =~ /\//
        return Launcher.launch
      end

      # Put just file options under

      txt = Xiki[command]

      file_options = txt.scan(/> \(file option\) .+/).join("\n")
      file_options.downcase!

      file_options.gsub!(/^.+?\) /, '')
      file_options.gsub!(/^\./, '+ ')
      Tree.<< file_options, :hotkey=>1

      # Todo > Tree.filter on items
        # Maybe use

    end

    def self.clipboard_to_here options={}

      clipboard = Clipboard[0]
      kind = FileTree.clipboard_file_kind! clipboard

      source = File.expand_path clipboard
      dest = Tree.path[-1]
      dest = File.expand_path dest
      FileTree.extract_filters! dest

      # Move or copy file...

      if options[:move]
        FileUtils.mv source, dest
      else
        FileUtils.cp_r source, dest
      end

      # Dest was dir, so show stem of source...
      # Dest was file, so just flash copied...

      past_tense = options[:move] ? "moved" : "copied"

      dest =~ /\/$/ ? "- #{File.basename source}" : "<* - #{past_tense} here"

    end

    # Returns "dir", "file", or "" if neither.
    # Also trims and unbookmarks the arg.
    def self.clipboard_file_kind! clipboard
      return "" if ! clipboard

      return "" if clipboard =~ /\n/
      clipboard.strip!
      return "" if ! FileTree.matches_root_pattern? clipboard

      # Do based on what's in clipboard
      # Make items based on whether path has file, dir, or neither...

      # Hmm > how about when > file doesn't exist?
      # Just look at path > ends in slash?

      clipboard.replace Bookmarks[clipboard]

      clipboard =~ /\/$/ ? "dir" : "file"
    end


    def self.clipboard_file_diff

      clipboard = Clipboard[0]

      kind = FileTree.clipboard_file_kind! clipboard
      return "- Not implemented yet > diffing dir!" if kind != "file"

      source = File.expand_path clipboard
      dest = Tree.path[-1]
      dest = File.expand_path dest
      FileTree.extract_filters! dest

      DiffLog.diff_files dest, source, :diffs_only=>1

    end

    def self.clipboard_menu

      clipboard = Clipboard[0]

      # See whether clipboard holds dir or file
      kind = self.clipboard_file_kind! clipboard
      return "" if kind == ""

      # Here so we can search for it
      # + move file here, + copy file here
      # + move dir here, + copy dir here
      clipboard_menu = "
        * with path/
          + move #{kind} here
            ! FileTree.clipboard_to_here :move=>1
          + copy #{kind} here
            ! FileTree.clipboard_to_here :copy=>1

          + move to #{kind}
          + copy to #{kind}

          + diff
            ! FileTree.clipboard_file_diff
      ".unindent

      # Todo
      # Not a file or dir, so show nothing

      clipboard_menu
    end


    def self.suggest_mkdir file_path
      "* create dir/"
    end

    def self.tasks_for_new_file options

      options[:no_slash] = 1
      task = options[:task]

      file_path = options[:file_path]

      extension = File.extname(file_path)[/\w+/]


      if task == []
        # Special treatment for .rb > 2 options
        return options[:output] = "* create script\n* create class" if extension == "rb"

        return options[:output] = "* create"
      end

      # Special treatment for .rb...

      case "#{extension}/#{task[0]}"
      # when "rb/script"   # The default behavior is fine
      when "rb/create class"
        clazz = TextUtil.camel_case File.basename options[:file_path], ".*"
        return options[:output] = Tree.quote(%`
          class #{clazz}
            def self.menu *args
              "
              hello
              "
            end
          end
          `)
      end

      # No extension, so look up by the full name...

      if ! extension
        file_name = File.basename file_path

        # Todo > move this out into a file
        # file format:
        # Dockerfile/
        #   | FROM ubuntu:latest
        #   | RUN apt-get update
        # Rakefile/
        #   | require 'rake'
        #   | ...
        txt = {
          "Dockerfile"=>%`
            # Sample Dockerfile from xsh
            FROM ubuntu:latest
            RUN apt-get update
            RUN apt-get install -y ruby
            RUN apt-get install -y nodejs npm
            RUN sudo gem install sinatra

            # add application sources
            COPY . /app
            RUN cd /app; npm install

            # Expose the default port
            EXPOSE  5000

            # Start command
            CMD ["nodejs", "/app/web.js"]
            `.unindent
        }[file_name]

        return options[:output] = Tree.quote(txt) if txt
      end

      require "#{Xiki.dir}roots/sample_menus/sample_menus_index.rb" # if !defined?(SampleMenus)
      txt = SampleMenus.by_extension(extension)

      return options[:output] = Tree.quote(txt)
    end

    # Removes all ##.../ and **.../ strings.  Returns those that
    # are at the end.
    #
    # FileTree.extract_filters!("/projects/foo/##hey/")
    # FileTree.extract_filters!("/projects/foo/**ls/bar.txt")
    def self.extract_filters! file_path   # Clean ##... and **... out of the path

      patterns_at_end = []

      # Treat \/ as part not a slash delimiter (gsub it to 021 temporarily)
      file_path.gsub!("\\/", "\021")

      # While a (rightmost) filter exists

      while file_path =~ %r"^([^|]*/)((##|\*\*)[^/]+/?)"
        start, filter = $1.length, $2

        # If it was at end, store it so we can do filter, and ignore the rest
        if start + filter.length == file_path.length
          patterns_at_end.unshift filter.gsub("\021", "\\/")
        end

        file_path.slice! start, filter.length   # Delete it
      end

      file_path.gsub!("\021", "\\/")
      patterns_at_end.any? ? patterns_at_end : nil
    end

    def self.expand_filter filters, options   #> ["##dir, name"]

      raise "We're not yet set up to handle multiple ## or ** filters of the same type.  Should be easy though, just run both regex's as you grep." if filters.length > 2

      options[:no_slash] = 1

      file_path = options[:file_path]

      content_filter, file_filter = nil, nil
      filters.each do |filter|
        is_content_filter = filter =~ /^##/
        filter = filter[/^..(.+)$/, 1]
        filter.sub! /\/$/, ''
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
      list = list.join "\n"

      return "<*" if list.blank?

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
      path_without_slash = path.sub(/\/$/, '')
      exists = File.exists? path_without_slash

      kind =
        if exists
          File.file?(path_without_slash) ? :file : :directory
        else   # If it has a dot, assume it's a file
          path =~ /\.\w+\/?/ ? :file : :directory
        end

      # Remove slash if a file
      path.sub!(/\/$/, '') if kind == :file && path =~ /\/$/

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

    def self.hop_file

      file = View.file
      if ! file
        if View.name == "active links/"
          View.to_highest
          Search.forward "^  "
          Tree.collapse
          Launcher.launch

          return "" # View.beep "- Todo > jump back to top and expand"
        end

        return View.beep "- This view isn't saved in a file!"
      end

      file = Files.tilde_for_home file

      View.to_buffer "active links/" #, :dir=>dir

      View.clear
      Notes.mode
      View.wrap :off

      View.>> "active links/\n  #{file}\n\n\n"
      View.line = 2

      Launcher.launch

    end

    def self.default_external_editor
      editor = ENV['EDITOR']
      # If the variable is set, just use it
      return editor if editor

      # In mac > default to "open -t"
      if Environment.os == "osx"
        return "open -t"
      end

      # In linux or other > default to "less"
      "nano"

    end


    def self.open_with_external_editor file, options={}

      if file =~ /\/:/
        file, quote = file.split("/: ", 2)
      end

      file_quoted = file =~ /^[a-z_\/.-]+$/i ? file : "\"#{file}\""

      editor = self.default_external_editor
      commands = "#{editor} #{file_quoted}"

      line = options[:line]

      if quote

        # Jump to quote, after Looking in the file to find which line number it's on
        txt = File.read file
        txt.encode!('UTF-8', 'binary', invalid: :replace, undef: :replace, replace: '')

        line = View.find_snippet_in_txt txt, quote
      end

      # If GUI editor, don't quit
      if editor == "open -t"
        return Shell.command commands

      elsif editor =~ /\Asubl(ime)?\b/
        # Remove -w or --wait flag (would make xiki lock up)
        editor = editor.sub(/ --?w(ait)?\b/, '')
        commands = "#{editor} #{file_quoted}"
        commands = "#{commands}:#{line}" if line
        return Shell.command commands


      elsif editor =~ /\Aatom\b/
        # Remove -f or --foreground flag (would make xiki lock up)
        editor = editor.sub(/ --?f(oreground)?\b/, '')

        commands = "#{editor} #{file_quoted}"
        commands = "#{commands}:#{line}" if line
        return Shell.command commands


      elsif editor == "vim"
        commands = "#{commands} +#{line}" if line

      elsif editor == "emacs"
        commands = "#{editor} +#{line} #{file_quoted}"

      end

      # Assume terminal editor, and so just quit

      return DiffLog.quit_and_run commands

      nil

    end


  end
  FileTree.define_styles
end
