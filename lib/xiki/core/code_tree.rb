require 'xiki/core/line'
require 'xiki/core/ol'

module Xiki
  class CodeTree

    def self.menus; @menus; end

    def self.add_menu item
      @menus ||= []
      @menus << item unless @menus.member? item
    end

    def self.launch options={}
      line = Line.without_indent

      return Search.open_file_and_method line if Keys.open?   # If as+open, just navigato to it!

      FileTree.extra_line_if_end_of_file
      Tree.plus_to_minus_maybe
      orig = Location.new
      Line.to_left
      path = options[:path] || Tree.construct_path(:list=>true)
      path.each do |l|
        # if '- .xx:/", get rid of trailing slash
        l.sub!(/^([+-] .*\..+)\/$/, "\\1")
      end
      # Determine code to eval
      code = self.determine_code_from_path path
      orig.go

      self.run code, options
    end

    def self.returned_to_s returned

      if returned.is_a? String   # Join and bulletize
        returned
      elsif returned.is_a? Array   # Join and bulletize
        returned.map{|l| "#{l =~ /\/$/ ? '+' : '-'} #{l}\n"}.join('')
      elsif returned.is_a? Hash
        (returned.map{|k, v| v =~ /\/$/ ? "+ #{k}: #{v}" : "- #{k}: #{v}"}.join("\n")) + "\n"
      else
        returned.to_s.strip
      end
    end

    def self.draw_exception exception, code=nil
      message = exception.message

      # TODO: pay attention to this option?
      #     Ol << "options[:suggest_args]: #{options[:suggest_args].inspect}"

      # TODO
      # For now, don't do it!
      # - How to not show "arg1" etc
      #   - when ArgumentError is code deeper than the immediate one
      #     if exception.is_a? ArgumentError
      #       count, required = message.match(/\((\d+) for (\d+)\)/)[1..2]
      #       if count < required   # We can't add sample args if too many were supplied
      #         return (count.to_i+1..required.to_i).to_a.map{|o| "arg#{o}"}.join('/')+"/"
      #       end
      #     end


      if exception.is_a? RuntimeError
        # If it was in the format of tree output, just show it

        message = message.unindent if message =~ /^\s/

        # Some messages start with "path:line:...: " at beginning again

        message.sub!(/.+?: /, '') if message =~ /in `/

        return message if message =~ /\A[>|+-] /   # /^- /
        return View.prompt($1) if(message =~ /\A\.prompt (.+)/)
        return View.flash($1) if(message =~ /\A\.flash (.+)/)
      end

      backtrace = exception.backtrace[0..8].join("\n").gsub(/^/, '  @') + "\n"

      # If path in message, move it to the stack trace
      if message =~ /(.+\d:in `.+'): (.+)/m
        path, message = $1, $2
        backtrace = "  #{path}\n#{backtrace}"
      end

      message = self.format_exception_message_for_tree message

      txt = ""

      if code.is_a? Proc
        txt << "- tried to run:#{code.to_s}\n"
      elsif code.is_a? String
        code = code.strip
        if code =~ /\n/   # If multi-line, quote it
          code = "\n#{Tree.quote(code).strip.gsub /^/, '  '}"
        else
          code = " #{code}"
        end
        txt << "- tried to run:#{code}\n"
      end
      txt << "- error:#{message}\n- backtrace:\n#{backtrace}"
      txt
    end

    def self.format_exception_message_for_tree message
      return " #{message}" if message !~ /\n/
      "\n"+message.strip.gsub(/^/, "  | ").gsub(/^( +\|) ([@+-])/, "\\1\\2")
    end

    def self.run code, options={}
      b = View.buffer

      orig = Location.new
      orig_left = $el.point

      returned, stdout, e = Code.eval code, View.file, Line.number

      # If no stdout (and something was returned), print return value
      if ! stdout.nonempty? and ! returned.nil?
        stdout = self.returned_to_s returned

      # What was printed out wasn't overly useful, and causes a flicker in emacs 24
        # (Due to the minibuffer quickly growing then shrinking).
        # TODO: probably make it only print out if output is single-line?
        # Or maybe add "..." if multi-line
        #     else
        #       $el.message(returned.to_s) if returned and (!returned.is_a?(String) or returned.size < 500)
      end

      stdout = self.draw_exception(e, code) if e

      buffer_changed = b != View.buffer   # Remember whether we left the buffer

      # Insert output if there was any
      if stdout.nonempty?
        # Pull out flags
        if stdout =~ /^:code_tree_option_tree_search\n/
          options[:tree_search] = true
          stdout.sub! /.+\n/, ''   # Remove option
        elsif stdout =~ /^:quote_search_option\n/
          options[:quote_search] = true
          stdout.sub! /.+\n/, ''   # Remove option
        elsif stdout =~ /^:code_tree_option_no_search\n/
          options[:no_search] = true
          stdout.sub! /.+\n/, ''   # Remove option
        end
        ended_up = Location.new

        # Go back to where we were before running code
        orig.go
        indent = Line.indent
        Line.to_left
        Line.next
        left = $el.point

        # Move what they printed over to left margin initally, in case they haven't
        stdout = TextUtil.unindent(stdout)
        stdout.sub!(/\n\n\z/, "\n")   # Remove any double linebreaks at end

        stdout = Tree.quote stdout if options[:quote] && ! e

        stdout.gsub!(/^/, "#{indent}  ")

        View << stdout  # Insert output
        right = $el.point

        orig.go   # Move cursor back
        ended_up.go   # End up where script took us

        # These are deprecated - eventually pull them out
        if options[:tree_search]   # If they want to do a tree search
          $el.goto_char left
          FileTree.select_next_file
          Tree.search(:left=>left, :right=>right, :recursive=>true)
          return
        end
        if options[:quote_search]   # If they want to do a tree search
          $el.goto_char left
          Search.forward "|"
          Line.to_beginning
          Tree.search(:left=>left, :right=>right, :recursive_quotes=>true)
          return
        end

        # error or unit test output
        return Line.to_beginning :down=>1 if e || stdout =~ /\n  - error:\n    \| expected: /

        # If script didn't move us (line or buffer), do incremental search
        if !options[:no_search] && !buffer_changed && $el.point == orig_left
          # TODO No search if there aren't more than 3 lines
          $el.goto_char left

          Line.to_words

          # Determine how to search based on output!

          Tree.search_appropriately left, right, stdout

        elsif options[:no_search]
          Line.to_beginning :down=>1
        end

      end
    end

    def self.menu
      l = []
      ObjectSpace.each_object(Class) do |c|
        next unless c.respond_to?(:menu)
        l << c.to_s
      end
      l.map! {|c| c.sub(/^#.+::/, '')}
      (l + menus).sort.each do |c|
        next if ["CodeTree"].member?(c)
        puts "+ #{c}.menu/"
      end
      ""
    end

    def self.layout_menu
      View.bar if Keys.prefix_u?

      buffer = "*CodeTree CodeTree menu"
      if View.buffer_open? buffer   # If open, switch to it
        View.to_buffer(buffer)
      else # Else open it
        Launcher.open("- CodeTree.menu/")
      end
    end

    # Determine whether we should handle it
    def self.handles? list=nil
      list ||= Tree.construct_path(:list => true)   # Use current line by default

      code_tree_root = nil
      index = list.size - 1

      # If @..., don't bother with rest of path
      list = [list[-1]] if list[-1][/^ *([+-] )?@/]

      list.reverse.each do |l|

        # If it has a char that wouldn't be in a file, must be code tree
        #return index if l =~ /[\[\{=]/

        # If last one was suspected as root, confirm we're not a dir (must be a file if parent is a dir)
        if code_tree_root
          if l =~ /\/$/  # Dir means it was a file that looked like code
            # This means never interpret classes indented under foo/
            code_tree_root = nil
          else
            return index + 1  # Must be legit, so return our index
          end
        end
        # If function call, it might be the root
        if l =~ /^[+-]? ?@?[A-Z][A-Za-z0-9]*\.[a-z_]/
          code_tree_root = index
        end
        index -= 1
      end
      code_tree_root
    end

    # Rules for constructing code from path
    # - examine path consisting of where C-. occurred and all its ancestors
    # - root of tree is first ancestor with Class.method
    # - invoke first ancestor method (using its params)
    # - append all ancestor data nodes as params
    #   - include self if a data node
    def self.determine_code_from_path path
      data = []
      clazz = metho = nil
      metho_maybe = clazz_maybe = nil

      i = -1

      path.reverse.each do |l|   # Climb up path
        i += 1

        l.sub! /^@/, ''   # Remove delegate, so it's respected

        metho_tmp = self.extract_method(l)
        clazz_tmp = self.extract_class(l)
        l.sub(/^(\s+)[+-] /, "\\1")   # Remove bullets
        is_root = i + 1 == path.size
        code_node = false
        if metho_tmp   # If line has method
          if clazz_tmp   # If line has class
            if is_root || self.definite_code_tree_root(l)
              metho ||= metho_tmp
              code_node = true
              clazz ||= clazz_tmp
              break if clazz
            else   # Use this if nothing else is found
              metho_maybe ||= metho_tmp
              clazz_maybe ||= clazz_tmp
            end
          else   # Otherwise, it can be a method
            metho ||= metho_tmp
            code_node = true
          end
        end
        if ! code_node   # If not code node, must be data node
          # Prepend to data list
          data << self.paramify(l)
        end
      end

      unless clazz   # If no code found
        return nil unless clazz_maybe
        metho = metho_maybe
        clazz = clazz_maybe
        data = []
      end

      # If one line, return it literally
      if i == 0
        return Line.without_label(:line=>path[-1]).sub(/\/$/, '')
      end

      # Extract any params from after method
      method_with_params, params = metho.match(/(\w+\??)(.*)/)[1..2]
      params.sub!(/^\((.*)\)$/, "\\1")  # Remove parens if they're there

      # Surround with curly brackets if it's a hash, and no curly brackets already
      # Assumes all params are in a hash, which may not be right
      if params =~ /=>/ && params !~ /\}$/
        params = " {#{params.strip}}"
      end

      # If last parameter was |..., make it be all the lines
      if data.first =~ /^ *\|/
        data.first.replace( self.escape(
          Tree.siblings.map{|i| "#{i[/^ *\| ?(.*)/, 1]}\n"}.join('')
          ))
      end

      # If any data nodes, pass as params
      if ! data.empty?
        data.reverse!
        data.map! {|a| "\"#{a}\""}
        params << ", " + data.join(", ")
      end
      # TODO Get rid of comma if there is one
      params.sub!(/^, /, '')
      "#{clazz}.#{method_with_params}(#{params})"
    end

    def self.extract_class l
      l[/^([A-Z][A-Za-z0-9]*)\.[a-z_]/, 1]
    end

    def self.extract_method l
      l = l.sub(/^[+-] [\w -]+?: /, '').sub(/^[+-] /, '')  # Remove bullets
      # Either class or bol
      result = l[/^[A-Z][A-Za-z0-9]*\.([a-z_].*)/, 1] ||  # Class and method
        l[/^\.([a-z].*)/, 1]  # Method at beginning of line
      result ? result.sub(/\/$/, '') : nil
    end

    def self.paramify l
      l = Line.without_label(:line=>l)

      l = self.escape(l)
      if l =~ /^\|/   # If |..., escape single-quotes
        #       l.gsub!("'", "\\'")
      else
        l.gsub! ', ', '", "'
      end

      l
    end

    # Always escape backslashes, single-quotes, #'s
    def self.escape l
      l = l.gsub("\\", "\\\\\\\\")
      l.gsub!("\"", "\\\\\"")
      l.gsub!("#", "\\\\#")
      l
    end


    def self.kill_siblings options={}
      prefix = Keys.prefix :clear=>true

      left1, right1, left2, right2 = Tree.sibling_bounds

      # If number, adjust

      if prefix.is_a?(Fixnum)
        left2 = Line.left prefix + 1
      elsif prefix == :u
        right2 = left2
      elsif prefix == :uu || prefix == :-
        right1 = left1
      end

      View.delete left2, right2
      View.delete left1, right1
    end

    def self.do_kill_indented options={}
      if Keys.up? || options[:cross_blank_lines]
        left, right = Tree.sibling_bounds :cross_blank_lines=>1
      else
        left, ignore1, ignore2, right = Tree.sibling_bounds
      end

      View.cursor = left
      $el.set_mark right
      Effects.blink :left=>left, :right=>right # unless options[:dont_cross_blank_lines]

      View.delete left, right

    end

    def self.as_indented
      prefix = Keys.prefix :clear=>1

      indent = Line.indent.length

      # If at left margin or current line followed by child
      if prefix != :u && (prefix == :- || indent == 0 || indent == Line.indent(Line.value(2)).length - 2)
        orig = View.cursor
        left = Line.left
        Line.next
        ignore, right = Tree.sibling_bounds :cross_blank_lines=>1
        View.cursor = orig
      elsif prefix == :u
        left, ignore1, ignore2, right = Tree.sibling_bounds
      else
        left, right = Tree.sibling_bounds :cross_blank_lines=>1
      end
      View.cursor = left
      $el.set_mark right
      Effects.blink :left=>left, :right=>right

      Clipboard.copy "0"

    end

    def self.kill_rest
      prefix = Keys.prefix(:clear=>true)
      column = View.column

      right1 = Line.left
      left2 = Line.left 2

      Line.next if Search.backward("^$", :go_anyway=>true)   # Go to beginning of paragraph
      left1 = Line.left

      Search.forward("^$", :go_anyway=>true)   # Go to end of paragraph
      right2 = Line.left

      if prefix.is_a?(Fixnum)
        View.cursor = left2
        right2 = Line.left(prefix + 1)
      end

      View.delete left2, right2   # Always delete after
      Line.previous

      # Optionally delete before
      View.delete(left1, right1) if prefix == :u
    end

    def self.tree_search_option
      ":code_tree_option_tree_search\n"
    end

    def self.quote_search_option
      ":quote_search_option\n"
    end

    def self.no_search_option
      ":code_tree_option_no_search\n"
    end

    def self.definite_code_tree_root line
      line =~ /^[\s+-]*[A-Z].+\..+[\/)]$/
    end

    def self.maybe_code_tree_root line
      line =~ /^[\s+-]*[A-Z].+\.[a-z_]/
    end

    def self.definite_dir_tree_root line
      line =~ /^[\s+-]*\//
    end

    # Turn list into bulleted string (adding + or - based on ending in slash)
    def self.bulletize list
      (list.map{|l| l =~ /\/$/ ? "+ #{l}" : "- #{l}"}.join("\n")) + "\n"
    end

    def self.remove_slashes *args
      args.each do |arg|
        arg.sub! /\/$/, '' if arg
      end
    end

  end
end
