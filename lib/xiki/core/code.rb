require 'xiki/core/block'
require 'stringio'

module Xiki
  class Code

    def self.menu
      %`
      > Eval code
      | Get return value and stdout
      |
      p Code.eval("puts 'printed'; 1 + 2")
      `
    end

    def self.location_from_proc id
      path = id.to_s
      path =~ /@(.+):([0-9]+)/
      file, line = $1, $2
      [file, line]
    end

    def self.bounds_of_thing left=nil, right=nil
      return [left, right] if left.is_a?(Fixnum)

      return [Line.left, Line.right+1] if left == :line

      n = Keys.prefix_n :clear=>true   # Check for numeric prefix

      return View.paragraph(:bounds=>1) if n == 0
      return [Line.left, Line.left(n+1)] if n   # If prefix, move down

      return View.range if left.is_a?(Hash) && left[:default] == :region

      [Line.left, Line.right]   # Do line by default
    end

    def self.to_comment
      $el.comment_search_forward View.bottom
    end

    def self.comment left=nil, right=nil
      prefix = Keys.prefix
      line = Line.value

      # No prefix add no selection, so use the line
      if ! prefix && ! View.selection?
        left, right = Line.left, Line.right+1

        # Line is blank, so insert a comment...

        return self.enter_insert_comment if line =~ /^ *$/

      elsif prefix == :u   # If C-u prefix, add comment on end
        return self.enter_insert_comment
      elsif prefix == 0   # If 0 prefix, comment paragraph
        left, right = View.paragraph(:bounds => true)
      else

        if prefix == 2
          a_commented = line =~ /^ *(#|\/\/)/
          b_commented = Line.value(2) =~ /^ *(#|\/\/)/
          if !!a_commented ^ !!b_commented
            Keys.clear_prefix
            orig = Location.new
            self.comment Line.left(1), Line.left(2)   # Toggle commenting of this line
            self.comment Line.left(2), Line.left(3)   # Toggle commenting of next line
            orig.go
            return
          end
        end

        Line.to_left

        left ||= {:default=>:region}
        left, right = self.bounds_of_thing(left, right)
        left, right = right, left if View.cursor == right   # In case cursor is at right side
      end

      # Nothing was selected, so select the line...

      View.to left
      View.set_mark right

      # Maybe implement this > In a .notes file, so just comment with "|" at beginning of line

      $el.comment_or_uncomment_region View.range_left, View.range_right

      # Disabled indenting > see how this feels
      self.indent View.range_left, View.range_right

    end

    # Evaluates file, paragraph, or next x lines using el4r
    def self.run options={}

      prefix = Keys.prefix

      Ol.clear_pause

      if options[:left]
        left, right = options[:left], options[:right]
        txt = View.txt left, right
      elsif prefix.is_a?(Fixnum) && 0 <= prefix && prefix <= 7
        txt, left, right = View.txt_per_prefix nil, :blink=>1, :remove_heading=>1
        file, line = View.file, Line.number(left)
      else

        prefix ||= :u if View.extension == "rb"   # In .rb file, so pretend like prefix was :u, to load file

        case prefix
        when :u   # Load file
          return self.load_this_file

          # These were superceded by .txt_per_prefix apparently

        when :uu   # Load file in Xiki shell command instance
          `xiki "! load '#{View.file}'"`
          return View.flash "- loaded in Xiki shell command instance!"

        when :-   # Load file at point in tree
          path = Tree.construct_path
          load path
          return View.flash "- loaded!"

        when 8   # Put into file and run in console
          File.open("/tmp/tmp.rb", "w") { |f| f << Notes.current_section_object("^>").text }
          return Shell.run "ruby -I. /tmp/tmp.rb", :dir=>View.dir
        when 9   # Pass whole file as ruby
          return Shell.run("ruby #{View.file_name}", :buffer => "*console ruby")
        else   # Move this into ruby - block.rb?
          ignore, left, right = View.block_positions "^>"
          file, line = View.file, Line.number(left)
        end

        txt = View.txt(:left=>left, :right=>right).to_s
        Effects.blink :left=>left, :right=>right
      end

      txt.sub! /\A( *)@ /, "\\1"   # Remove @ if at beginning
      txt.gsub! /^ *[|:] ?/, '' if txt =~ /\A *[|:]/   # Remove quoted lines if it's quoted

      orig = Location.new
      $el.goto_char right
      after_code = Location.new  # Remember right of code
      orig.go

      # Eval the code
      returned, out, exception = self.eval txt, file, line
      begin
        if returned != nil and returned != ""
          returned = returned.to_s[0..49]
          returned << "..." if returned.length == 50
          Message << returned
        end
      rescue
      end
      ended_up = Location.new

      # Insert output
      after_code.go

      if prefix
        View.insert(out.gsub /^/, '  ') unless out.blank?
      else
        View.insert(">>\n#{out}") unless out.blank?
      end

      if exception
        backtrace = exception.backtrace[0..8].join("\n").gsub(/^/, '  = ') + "\n"
        error = CodeTree.format_exception_message_for_tree exception.message
        View.insert ">>\n"
        View.insert "- error:#{error}\n- backtrace:\n#{backtrace}".gsub(/^/, '  ')
      end

      orig.go   # Move cursor back to where we started
      ended_up.go   # Go to where we ended up
    end

    def self.run_in_rails_console
      left, after_header, right = View.block_positions "^>"
      block = $el.buffer_substring after_header, right

      View.to_after_bar if View.in_bar?

      View.to_buffer "*rails console"
      $el.erase_buffer
      View.to_bottom

      #     if elvar.current_prefix_arg
      #       insert "reload!"
      #       command_execute "\C-m"
      #     end

      $el.insert block
      $el.command_execute "\C-m"

      $el.beginning_of_buffer

    end

    def self.show_el4r_error
      View.open $el.elvar.el4r_log_path
      $el.revert_buffer(true, true, true)
      View.wrap
      View.to_end
      $el.re_search_backward "^  from "
      $el.re_search_backward "^[A-Z]"
      $el.recenter 0
      Color.mark "red"
    end

    def self.enter_ruby_log
      txt = Clipboard.get("0")
      $el.insert "Ol << \"#{txt}: \#{#{txt}}\""
    end

    def self.indent left=nil, right=nil
      left, right = View.range if left.nil?
      $el.indent_region(left, right)
    end

    # Thin wrapper, but will probably need to conditionally not call $el.instance_eval
    def self.simple_eval code, file=nil, line=nil, options={}

      Object.module_eval code, file||__FILE__, line||__LINE__

      #       if options[:global] || ! $el
      #         Object.module_eval code, file||__FILE__, line||__LINE__
      #       else
      #         Object.module_eval code, file||__FILE__, line||__LINE__
      #       end

    end

    # xiki api > Eval ruby code

    # Evaluates a string, and returns the output and the stdout string generated
    # Code.eval "p 11; 22", nil, nil, :simple=>1   # Returns 1 string, which is quoted return value or output, or unquoted formatted exception.
    #
    # params:
    #   | options : The last options param will be available to the code being eval'ed
    def self.eval code, file=nil, line=nil, eval_options={}, options={}

      if file.is_a? Hash
        eval_options = file
        file = nil
      end

      return ['- Warning: nil passed to Code.eval!', nil, nil] if code.nil?

      # Capture stdout output (saving old stream)
      orig_stdout = $stdout;  $stdout = StringIO.new
      stdout = nil
      exception = nil
      returned = nil

      # Run the code...

      begin
        if dir = eval_options[:dir]
          Dir.chdir(dir) do
            returned = self.eval_inner code, file, line, eval_options, options
          end
        else
          returned = self.eval_inner code, file, line, eval_options, options
        end
      rescue Exception => e
        exception = e
      end

      stdout = $stdout.string;  $stdout = orig_stdout  # Restore stdout output

      # Is this even worth doing?
      if exception && (eval_options[:pretty_exception] || eval_options[:simple])
         exception = CodeTree.draw_exception exception, code if exception
      end

      # This change might be risky
      stdout = nil if stdout == ""

      if eval_options[:simple]   # Return one string (quoted if result, or just exception)
        return exception if exception

        txt = stdout || returned
        return nil if ! txt
        txt = txt.to_s
        txt = Tree.quote txt if eval_options[:quoted]
        return txt
      end

      [returned, stdout, exception]
    end

    def self.eval_inner code, filename, line, eval_options, options
      target = eval_options[:target_module] || Object

      # These variables will be > passend to code > accessible by the evaled code
      args, path, dir, task, items = options[:args_relative]||options[:args]||[], options[:path_relative]||options[:path], options[:dir], options[:task], options[:items]
      shell_command, shell_output = options[:shell_command], options[:shell_output]
      file = options[:file]

      arg1, arg2, arg3 = (args||[])[0..2]

      if eval_options[:binding]
        return eval_options[:binding].eval(code, filename||__FILE__, line||__LINE__)
      end

      if code.is_a? Proc
        target.module_eval &code
      else
        # use class_eval if we change Xiki to a class instead of a module
        # target.class_eval code, filename||__FILE__, line||__LINE__
        target.module_eval code, filename||__FILE__, line||__LINE__
      end
    end

    def self.eval_snippet txt, file, line_number, eval_options, options

      language = txt[/^!(.)/, 1]
      language = {"."=>"ruby", " "=>"javascript"}[language]

      txt.gsub!(/^!.?/, "")   #> ["red"]

      # Make sure only one line break
      txt = "#{txt.sub(/\n+\z/, '')}\n"

      options[:args] = options[:items]

      # ".", so evaluate as javascript...

      if language == "javascript"
        # Add return if there was none
        txt.sub!(/.+\n\z/, "return \\0") if txt !~ /^ *return .+\n\z/

        txt = "print = p = console.log;\n#{txt}"
        return JavascriptHandler.eval txt
      end

      # " ", so evaluate as ruby...

      txt = Code.eval txt, file, line_number, {:pretty_exception=>1, :simple=>1}, options   #> ||||||
      txt

    end


    def self.do_as_align
      $el.align_regexp
    end

    def self.open_related_rspec

      # If in xiki project...

      if View.file =~ /\/xiki\//
        if View.file =~ /\/spec\//   # If in spec, open corresponding file

          path = View.file
          path.sub! "/spec/", "/lib/xiki/core/"   # "/projects/xiki/lib/xiki/spec/code_spec.rb"
          path.sub! "_spec.rb", ".rb"

          View.open path
        else   # Otherwise, open file corresponding spec

          method = Code.grab_containing_method :name=>1
          path = View.file
          path.sub! "/lib/xiki/core/", "/spec/"   # "/projects/xiki/lib/xiki/spec/code_spec.rb"
          path.sub! ".rb", '_spec.rb'

          View.open path

          View.recenter_under "^ *describe .+##{method}[^_a-zA-Z0-9]"
        end

        return
      end

      # If normal specs...

      if View.file =~ /\/(app|spec)\//
        if View.file =~ /\/app\//   # If in file, open corresponding spec
          unless Keys.prefix_u   # Unless C-u, store method
            orig = View.cursor
            Move.to_end
            Search.backward "^ +def "
            method = Line.value[/^ +def (self\.)?(\w+)/, 2]   # Save method name
            View.cursor = orig
          end

          View.open View.file.sub('/app/', '/spec/unit/').sub(/\.rb/, '_spec.rb')

          if method   # Jump to method if they were on def... line
            Keys.clear_prefix
            View.to_highest
            Search.forward "^ *describe .+##{method}[^_a-zA-Z0-9]", :beginning=>true
            Line.to_beginning
            View.recenter_top
          end

        else   # Otherwise, open corresponding file
          unless Keys.prefix_u   # unless C-u, store method
            orig = View.cursor
            Move.to_end
            Search.backward "^ *describe "
            method = Line.value[/^ *describe .+#(\w+)/, 1]   # Save method name
            View.cursor = orig
          end

          View.open View.file.sub('/spec/unit/', '/app/').sub(/\_spec.rb/, '.rb')
          View.open View.file.sub('/spec/', '/app/').sub(/\_spec.rb/, '.rb')

          if method   # Jump to method if they were on def... line
            Keys.clear_prefix
            View.to_highest
            Search.forward "^ *def \\(self\\.\\)?#{method}[^_a-zA-Z0-9]", :beginning=>true
            Line.to_beginning
            View.recenter_top
          end

        end
        return
      end


      View.beep
      View.message "Don't recognize this file."
    end

    def self.do_as_rspec options={}
      xiki = View.dir =~ /\/xiki/   # Identify if xiki
      args = []
      extra = "DS_SUPPRESS=true;"
      prefix = Keys.prefix

      return Specs.run_spec_in_place if prefix.nil?

      if prefix == :u
        args << "spec/unit"

        if View.mode == :shell_mode   # If already in shell, don't change dir
          dir = nil
        else
          begin
            dir, spec = View.file.match(/(.+)\/(spec\/.+)/)[1,2]
          rescue
            dir, spec = View.file.match(/(.+)\/(app\/.+)/)[1,2]
          end
        end

      # Prefix must be something - 9, I guess, by convention??

      elsif View.file !~ /_spec\.rb$/   # If not in an rspec file, delegate to: do_related_rspec
        orig = Location.new
        self.do_related_rspec
        orig.go
        return
      else

        if options[:line]   # If specific line, just use it
          args << '-l'
          args << options[:line]
        else   # Otherwise, figure out what to run
          orig = Location.new
          orig_index = View.index

          if prefix == 1   # If C-1, only run this test
          else
            before_search = Location.new
            Line.next

            # Find first preceding "it " or "describe "
            it = Search.backward "^ *it ", :dont_move=>true
            describe = Search.backward "^ *describe\\>", :dont_move=>true

            if it.nil? && describe.nil?
              View.beep
              before_search.go
              return View.message "Couldn't find it... or describe... block"
            end

            it ||= 0;  describe ||= 0

            if it > describe   # If it, pass rspec -e "should...
              extra = "DS_SUPPRESS=false; "
              View.cursor = it
              test = Line.value[/"(.+)"/, 1]
              args << '-e'
              args << "\"#{test}\""
            else   # If describe, pass rspec line number
              View.cursor = describe
              args << '-l'
              args << View.line_number
            end
            before_search.go
          end
        end

        # Chop off up until before /spec/
        dir, spec = View.file.match(/(.+)\/(spec\/.+)/)[1,2]
        args.unshift spec
      end

      buffer = "*console for rspec - #{dir}"
      # If spec buffer open, just switch to it
      if View.buffer_open? buffer
        View.to_buffer buffer
      else   # Otherwise open it and run console
        xiki ?
          Shell.run("", :dir=>dir, :buffer=>buffer) :
          Shell.run("bundle exec merb -i -e test", :dir=>dir, :buffer=>buffer)
      end
      View.clear

      View.wrap(:on) if prefix == :u

      if xiki
        command = "#{extra}spec #{args.join(' ')}"
      else
        args = args.map{|o| o =~ /^"/ ? o : "\"#{o}\"" }.join(",\n")   # Only add quotes if not already there
        command = "Spec::Runner::CommandLine.run(Spec::Runner::OptionParser.parse([#{args}], $stderr, $stdout))"
        # Rails version (commented out - it's currently hard-coded to use merb)
      end
      View.insert command
      Shell.enter
      View.to 1

      orig.go unless orig.nil? || View.index == orig_index   # Go back unless in same view
    end

    def self.do_related_rspec
      # Find method name
      orig = View.cursor
      Line.next
      Search.backward "^ +def "
      meth = Line.value[/def ([\w.!]+)/, 1].sub /^self\./, ''

      View.cursor = orig

      # Go to relevant test
      Code.open_related_rspec

      # Find test for method:
      View.to_highest
      Search.forward "^ *describe .+, \"##{meth}\" do"
      Move.to_axis
      View.recenter_top
      Code.do_as_rspec :line=>View.line_number

     # Run test
    end

    def self.load_this_file

      Effects.blink :what=>:all, :time=>0.08
      begin

        result = load View.file
        if e = result[2]
          View.open :txt=>""
          View.<<(CodeTree.draw_exception(e), :dont_move=>1)
          Search.forward "^    "
        end

      rescue Exception=>e
        Tree.<< "- Error:\n#{e.message.gsub /^/, '  '}!", :no_slash=>1
      end
      # Give it enough time to flash
    end

    def self.do_code_align
      left, right = $el.bounds_of_thing_at_point(:paragraph).to_a
      align_regexp(left, right, "\\( *\\)"+Keys.input(:prompt => "align to regex: "), 1, 3, true)
    end

    #
    # Indent selected lines.
    #
    # - do+indent       # Indent to the right (by 2 spaces)
    # - up+do+indent    # Indent to the left (by 2 spaces)
    # - 3+do+indent     # Make indent be 6 spaces from the left (3*2)
    #
    def self.indent_to options={}
      prefix = options[:prefix] || Keys.prefix
      if prefix == :-   # Just indent to where it should go
        Code.indent
        View.no_deselect
        return
      end
      prefix = :u if options[:left]

      txt = View.selection :always=>1
      return View.flash "- Select some lines first" if ! txt

      # Find lowest indent
      old_indent = txt.split("\n").reduce(999) do |acc, line|
        next acc if line.blank?
        indent = line[/^ */].length
        indent < acc ? indent : acc
      end

      new_indent =
        if ! prefix
          old_indent + 2
        elsif prefix == :u
          old_indent - 2
        elsif prefix.is_a?(Fixnum)
          prefix * 2
        else
          raise ".blink Don't know how to indent by '#{prefix}'"
        end

      return if new_indent < 0

      orig = Location.new
      View.delete

      # Grab indent if 1st line that has text
      txt.gsub!(/^\s+/) { |t| t.gsub("\t", '        ') }   # Untab indent
      txt.gsub! /^#{' ' * old_indent}/, ' ' * new_indent

      txt.gsub!(/^ +$/, '')   # Kill trailing spaces on lines with just spaces

      left = View.cursor
      right = left + txt.length
      View.insert txt, :dont_move=>1
      View.selection = left, right

    end

    def self.enter_as_backslash
      txt = Clipboard.get("0")
      txt.strip!
      txt.gsub!(/$/, "\\")  # Add \'s
      txt.gsub!(/\\\z/, '')  # Remove last \
      $el.insert txt
    end

    def self.kill_duplicates
      txt = View.selection :delete=>true, :always=>1
      l = txt.split("\n")
      orig = Location.new
      View.insert l.uniq.join("\n") + "\n"
      View.set_mark
      orig.go
    end

    def self.randomize_lines txt=nil
      txt ||= View.selection :delete=>true

      return View.flash "- Select some lines first" if ! txt

      l = txt.split("\n")
      orig = View.cursor
      View.insert l.sort_by{ rand }.sort_by{ rand }.join("\n") + "\n"

      View.selection = orig, View.cursor
    end

    def self.do_next_paragraph
      orig = Location.new
      line = Line.value 1, :include_linebreak=>true, :delete=>true   # Get line
      Move.backward
      Search.forward "\n\n+"
      View.insert line

      orig.go
    end

    def self.enter_log_clipboard
      clip = Clipboard[0]
      View.insert "Ol << \"#{clip}: \#{#{clip}}\""
      return
    end

    def self.enter_log_stack
      return View.insert "p_stack()" if View.extension == "js"
      View.insert "Ol.stack"
    end

    def self.enter_log_ancestors
      View.insert "Ol.ancestors"
    end

    def self.enter_log_thing txt
      Line.to_left   # AA - beginning of line (A's default)
      $el.open_line(1) unless Line.blank?
      View.insert txt
      Line.to_beginning
    end

    def self.open_log_view options={}

      prefix = options[:prefix] || Keys.prefix(:clear=>true)
      prefix = nil if options[:called_by_launch]

      orig = View.current if prefix == :u   # up+layout+output means to go back

      file = Ol.file_path

      buffer = "ol"

      # If already visible, just go to it
      if View.buffer_visible?(buffer)

        View.to_buffer(buffer)

        self.clear_and_go_back orig

        # If already visible, always just go to it.  Why was this not happening before?
        return
      end

      # If not in bar, open the bar

      # If buffer open (but not visible), just switch to it
      if View.buffer_open? buffer
        View.to_buffer buffer
        return if self.clear_and_go_back orig
        return
      end

      return if file.nil? or file.empty?

      # Create file if not there
      `touch #{file}` unless File.exists?(file)
      lines = "#{file}.lines"
      `touch #{lines}` unless File.exists?(lines)

      Shell.run "tail -n 100 -f #{file}", :buffer=>buffer, :dir=>'/tmp', :dont_leave_bar=>true
      Notes.mode

      return if self.clear_and_go_back orig
    end

    def self.enter_log_line options={}

      return Firefox.enter_log_javascript_line if View.extension == "js"

      Move.to_axis if ! Line.at_right?

      View.<<("\n", :dont_move=>1) if Line !~ /^[ |:!]*$/

      # Javascript
      if Tree.construct_path(:all=>1, :slashes=>1) =~ /<script/
        View << "console.log('!');"
        return Move.backward 4
      end

      prefix = Keys.prefix

      if prefix == 1 || options[:exclamation]
        View.insert "Ol \"!\""
        return Move.backward 2
      end

      View.insert "Ol \"#{options[:txt]}\""
      Move.backward 1

    end

    def self.enter_log_out
      orig = Location.new
      View.layout_outlog
      View.to_bottom
      Line.previous

      output = Line.value[/\) (.+)/, 1]
      output.sub! /: /, " = "
      orig.go

      return View.flash("- Not found!") if output.nil?

      View << output
      Line.to_beginning
    end

    def self.enter_log_time
      $el.open_line(1) unless Line.blank?
      View.insert("Ol.time")
      Line.to_left
    end

    def self.to_ruby o
      o.source
      # Fails?
        # Does o.source work in ruby 1.8.7?  Do I care?  Maybe fall back to it.
      #     o.to_source
    end

    def self.isearch_just_should
      Search.stop
      term = Search.match
      View.delete(Search.left, Search.right)
      View.insert term.sub(/\.(.+)/, ".should_receive(:\\1)")
    end

    def self.clear_and_go_back location
      if location   # Go back to starting point
        View.clear

        View.clear "ol"
        View.to_window location
        return true   # Indicate to exit method
      end
      return false   # Don't exit
    end

    def self.do_list_ancestors
      prefix = Keys.prefix :clear=>1

      orig = View.cursor   # If not on indented line, back up to one...
      Search.backward "^ " if Line !~ /^ /

      # Climb path...

      txt = Tree.ancestors_indented


      # Indent over, and add file path and percent...

      txt.gsub! /^/, "  "

      file = View.file
      file = Files.tilde_for_home file

      filename = nil

      # Separate file name so it can be white

      if file && found = file.slice!(/[^\/]+$/)
        filename = found
      end

      $el.elvar.filename = filename || View.name

      View.cursor = orig   # Go back in case we moved

      txt = txt.strip.gsub('%', '%%')   # Escape %'s, .message interprets them strangely

      if prefix == :u   # Actually insert it inline
        Line.next
        View << "#{txt}\n"
        Line.previous
        return
      end

      indent = txt == "" ? "" : txt.split("\n")[-1][/^ +/]

      txt.gsub!('"', "\\\"")
      txt.gsub!(/^/, "  | ")
      $el.elvar.xiki_tmp_txt = txt.sub(/\A/, "\n")

      $el.elvar.xiki_tmp_line = "\n      #{indent}line #{View.line}, #{View.percent}%%"

      Code.eval_lisp %`
        (message (concat
          (propertize "#{file}" 'face 'ls-dir)
          filename
          (propertize xiki-tmp-txt 'face 'ls-quote)
          xiki-tmp-line
        ))
      `
    end

    def self.add_space

      left, right = View.range
      right = Line.right if left == right

      scroll = View.scroll_position

      View.cursor = right
      View << "\n\n\n\n\n\n"
      View.cursor = left
      View << "\n\n\n\n\n\n"

      View.scroll_position = scroll

    end

    def self.open_related_file
      file = View.file

      return View.open(file.sub /\.menu$/, '.rb') if file =~ /\/roots\/\w+\.menu$/
      return View.open(file.sub /\.rb$/, '.menu') if file =~ /\/roots\/\w+\.rb$/

      View.flash "No matching file known."

    end


    # Can be used by menus with dsl's that need to parse strings like:
    #   "bb(hi(xx)) aa(11)"
    #
    # Usage:
    #   h = {}
    #   Code.parse_functions "a(hi(xx)) b(11)c(a)", h
    #   Code.parse_functions "z(1)", h
    #   p h
    #     => {0=>["a(hi(xx))", "z(1)"], 11=>["b(11)"], 17=>["c(a)"]}
    def self.parse_functions txt, initial_hash={}
      s = StringScanner.new(txt)

      i = 0

      # Loop until none left
      while(! s.eos?)
        item = ""

        i += s.scan(/ */).length
        break if s.eos?

        item << s.scan(/\w+/)

        chunk = s.scan(/[(]/)
        raise "There was supposed to be a paren at index #{i + item.length} of #{txt.inspect}" if chunk != "("
        item << chunk
        paren_depth = 1

        # Scan until found closing parens
        while(chunk && paren_depth > 0)
          chunk = s.scan(/.*?[()]/)
          item << chunk
          paren_depth += item[/\($/] ? 1 : -1   # Adjust depth based on char at end
        end

        initial_hash[i] ||= []
        initial_hash[i] << item

        i += item.length
      end
      initial_hash
    end

    def self.enter_whitespace

      prefix = Keys.prefix :clear=>1
      prefix = 1 if prefix == :u

      # Selection, so surround with space...
      if View.selection?
        n = prefix.is_a?(Fixnum) ? prefix : 2

        left, right = View.range
        View.cursor = right
        View << "\n"*n
        View.cursor = left
        View << "\n"*n
        return
      end

      # No prefix, so insert single linebreak...

      if ! prefix
        View >> "\n"
        return
      end

      Deletes.delete_whitespace :prefix=>prefix

    end

    # Searches upward for "def..." and returns the line
    def self.grab_containing_method options={}
      orig = Location.new
      Move.to_end
      Search.backward "^  def "
      txt = Line.value
      orig.go

      return txt[/def (self\.)?(\w+)/, 2] if options[:name]

      txt
    end

    # Convenience for entering "# " to start a comment
    def self.enter_insert_comment
      prefix = Keys.prefix :clear=>1

      if Line.blank?
        View << "# "
        $el.ruby_indent_line
      else
        Move.to_end
        View << "   # "
      end

      # Dash+, so end with ...
      if prefix == :-
        View.<< "...", :dont_move=>1
      end

    end

    def self.launch_dot_at_end line
      prefix = Keys.prefix :clear=>1

      # If just Foo., show methods

      if line =~ /\A\w+\.\z/
        txt = Code.eval("#{line}meths")[0].map{|o| "- #{o}" }.join("\n")
        return Tree.<< txt, :no_slash=>1
      end

      if prefix == "open"   # If as+open, just navigate there
        return Search.open_file_and_method line.sub("./.", '.')
      end

      # It's foo./.bar/, so go to parent, collapse, and add this at end

      last = line.split('/').last
      Tree.to_parent
      CodeTree.kill_rest
      Line.sub! /\.$/, "#{last}"
      Move.to_end

      # If Foo. ... .bar, merge it back to parent (make Foo.bar), then launch

    end

    def self.suggest_creating_method file, method

      View.open "method doesn't exist", :txt=>"
        > Method doesn't exist. Create it?

        #{file}
          :+
          :+  def self.#{method}
          :+
          :+  end
          : end
        ".unindent

      View.line = 4
      View.column = 3

    end

    def self.jump_to_proc_source proc
      file, line = Code.location_from_proc proc
      file = "#{Xiki.dir}#{file}" unless file =~ /^\//
      Location.go file
      View.to_line line.to_i
    end

    # Code.eval_lisp '(message "hi")'
    def self.eval_lisp txt
      $el.eval $el.read "(progn #{txt})"
    end

    def self.args options, opts={}
      "
        args, path, dir, task = options[:args]||[], options[:path], options[:dir], options[:task]
      ".unindent
    end

    # Load a file in the context of Xsh
    def self.load path
      path = Bookmarks[path]
      self.eval File.read(path), path, 1
    end


    def self.cache key, &block
      $el.cache key, &block
    end

    # Adds exception handling
    def self.fork_and_eval_wrapper code

      result = self.eval code, :simple=>1

      URI.encode result

    rescue Exception=>e

      Ol "#{e}!!!"
      URI.encode result

    end

  end
end
