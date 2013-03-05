require 'xiki/block'
require 'stringio'

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

    if prefix == 0   # If 0 prefix, comment paragraph
      left, right = View.paragraph(:bounds => true)
    else

      if prefix == 2
        a_commented = Line =~ /^ *(#|\/\/)/
        b_commented = Line.value(2) =~ /^ *(#|\/\/)/
        if !!a_commented ^ !!b_commented
          Keys.clear_prefix
          orig = Location.new
          Code.comment Line.left(1), Line.left(2)   # Toggle commenting of this line
          Code.comment Line.left(2), Line.left(3)   # Toggle commenting of next line
          orig.go
          return
        end
      end

      Line.to_left
      left ||= {:default=>:region}
      left, right = Code.bounds_of_thing(left, right)
      left, right = right, left if View.cursor == right   # In case cursor is at right side
    end

    View.to left
    View.set_mark right

    $el.comment_or_uncomment_region View.range_left, View.range_right
    Code.indent View.range_left, View.range_right
  end

  # Evaluates file, paragraph, or next x lines using el4r
  def self.run options={}
    prefix = Keys.prefix

    if prefix == :uu
      path = Tree.construct_path

      load path
      return View.flash "- loaded!"
    end

    if options[:left]
      left, right = options[:left], options[:right]
      txt = View.txt left, right
    elsif prefix.is_a?(Fixnum) && 0 <= prefix && prefix <= 7
      file, line = View.file, Line.number(left)
      txt, left, right = View.txt_per_prefix nil, :blink=>1, :remove_heading=>1
    else
      case prefix
      when :u   # Load file in emacsruby
        return self.load_this_file

        # These were superceded by .txt_per_prefix apparently

      when 8   # Put into file and run in console
        File.open("/tmp/tmp.rb", "w") { |f| f << Notes.current_section("^>").text }
        return Console.run "ruby -I. /tmp/tmp.rb", :dir=>View.dir
      when 9   # Pass whole file as ruby
        return Console.run("ruby #{View.file_name}", :buffer => "*console ruby")
      else   # Move this into ruby - block.rb?
        ignore, left, right = View.block_positions "^>"
        file, line = View.file, Line.number(left)
      end

      txt = View.txt(:left=>left, :right=>right).to_s
      Effects.blink :left => left, :right => right
    end

    txt.sub! /\A( *)@ /, "\\1"   # Remove @ if at beginning
    txt.gsub! /^ *\| ?/, '' if txt =~ /\A *\|/   # Remove quoted lines if it's quoted

    # If C--, define the launcher
    if prefix == :-
      if txt =~ /\A\s*class (\w+)/
        clazz = $1
        Launcher.add TextUtil.snake_case(clazz)
      end
    end

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
      backtrace = exception.backtrace[0..8].join("\n").gsub(/^/, '  @') + "\n"
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
    Color.colorize :r
  end

  def self.enter_ruby_log
    txt = Clipboard.get("0")
    $el.insert "Ol << \"#{txt}: \#{#{txt}}\""
  end

  def self.indent left=nil, right=nil
    left, right = View.range if left.nil?
    $el.indent_region(left, right)
  end

  # Evaluates a string, and returns the output and the stdout string generated
  def self.eval code, file=nil, line=nil
    return ['- Warning: nil passed to Code.eval!', nil, nil] if code.nil?

    # Capture stdout output (saving old stream)
    orig_stdout = $stdout;  $stdout = StringIO.new
    stdout = nil
    exception = nil
    begin   # Run code
      # Good place to debug
      returned = $el.instance_eval(code, file||__FILE__, line||__LINE__)
    rescue Exception => e
      exception = e
    end
    stdout = $stdout.string;  $stdout = orig_stdout  # Restore stdout output
    [returned, stdout, exception]
  end

  def self.do_as_align
    $el.align_regexp
  end

  def self.open_related_rspec

    # If in xiki project...

    if View.file =~ /\/xiki\//
      if View.file =~ /\/spec\//   # If in spec, open corresponding file

        path = View.file
        path.sub! "/spec/", "/lib/xiki/"   # "/projects/xiki/lib/xiki/spec/code_spec.rb"
        path.sub! "_spec.rb", ".rb"

        View.open path
      else   # Otherwise, open file corresponding spec

        method = Code.grab_containing_method :name=>1
        path = View.file
        path.sub! "/lib/xiki/", "/spec/"   # "/projects/xiki/lib/xiki/spec/code_spec.rb"
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
        Console.run("", :dir=>dir, :buffer=>buffer) :
        Console.run("bundle exec merb -i -e test", :dir=>dir, :buffer=>buffer)
        #         Console.run("merb -i", :dir=>dir, :buffer=>buffer)
      #       Console.run "merb -i -e test", :dir=>dir, :buffer=>buffer
    end
    View.clear

    View.wrap(:on) if prefix == :u

    #     args << '-D'   # Show diffs

    if xiki
      command = "#{extra}spec #{args.join(' ')}"
    else
      args = args.map{|o| o =~ /^"/ ? o : "\"#{o}\"" }.join(",\n")   # Only add quotes if not already there
      command = "Spec::Runner::CommandLine.run(Spec::Runner::OptionParser.parse([#{args}], $stderr, $stdout))"
      # Rails version (commented out - it's currently hard-coded to use merb)
      #       command = "#{extra}p :reload; reload!; #{command}"
    end
    View.insert command
    Console.enter
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
    Effects.blink :what=>:all
    begin
      load View.file
    rescue Exception=>e
      Tree.<< "- Error:\n#{e.message.gsub /^/, '  '}!", :no_slash=>1
    end
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
  def self.indent_to

    prefix = Keys.prefix
    return Code.indent if prefix == :-   # Just indent to where it should go

    txt = View.selection
    old_indent = txt[/^( *)[^ \n]/, 1]

    new_indent =
      if ! prefix
        old_indent.length + 2
      elsif prefix == :u
        old_indent.length - 2
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
    txt.gsub! /^#{old_indent}/, ' ' * new_indent

    txt.gsub!(/^ +$/, '')   # Kill trailing spaces on lines with just spaces

    View.insert txt

    if orig.line != Line.number   # If we're at the end
      View.set_mark
      orig.go
    end

  end

  def self.enter_as_backslash
    txt = Clipboard.get("0")
    txt.strip!
    txt.gsub!(/$/, "\\")  # Add \'s
    txt.gsub!(/\\\z/, '')  # Remove last \
    $el.insert txt
  end

  def self.enter_as_debug

    orig = View.range[0]
    txt = View.selection :delete=>true
    count = 0
    txt.gsub!(/^.+/) { |m|
      if m =~ /^\s+(end|else|elsif|\})/
        m
      else
        count += 1;
        (count & 1 == 0) ? " ol #{count}\n#{m}" : m
      end
    }

    View.insert txt
    View.to orig
  end

  def self.kill_duplicates
    txt = View.selection :delete=>true
    l = txt.split("\n")
    orig = Location.new
    View.insert l.uniq.join("\n") + "\n"
    View.set_mark
    orig.go
  end

  def self.randomize_lines txt=nil
    txt ||= View.selection :delete=>true
    l = txt.split("\n")
    orig = Location.new
    View.insert l.sort_by{ rand }.sort_by{ rand }.join("\n") + "\n"
    View.set_mark
    orig.go
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

  def self.enter_log_console
    View.insert "console.log();"
    Move.backward 2
  end

  def self.open_log_view options={}
    prefix = Keys.prefix :clear=>true
    prefix = nil if options[:called_by_launch]
    orig = View.current if prefix == :u

    file = Ol.file_path

    buffer = "*ol"

    # If already open, just go to it
    if View.buffer_visible?(buffer)
      View.to_buffer(buffer)
      return if self.clear_and_go_back orig
    end

    # If 2 or more windows open
    if View.list.size == 2
      View.to_nth(1)   # Go to 2rd
    elsif View.list.size >= 3
      View.to_nth(2)
      unless View.left_edge == 0   # If 3nd not at left, go to 2nd
        View.to_nth(1)
        unless View.left_edge == 0   # If not at left, go to first
          View.to_nth(0)
        end
        View.create
      end
    end

    # If buffer open, just switch to it
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

    Console.run "tail #{prefix == :- ? '-n 100' : ''} -f #{file}", :buffer=>buffer, :dir=>'/tmp', :dont_leave_bar=>true
    Notes.mode

    return if self.clear_and_go_back orig
  end

  def self.enter_log_line

    return Firefox.enter_log_javascript_line if View.extension == "js"

    $el.open_line(1) unless Line.blank?

    # Javascript
    if Tree.construct_path(:all=>1, :slashes=>1) =~ /<script/
      View << "console.log('!');"
      return Move.backward 4
    end

    if Keys.prefix_u?
      View.insert 'Ol["!"]'
      return Move.backward 3
    end

    View.insert "Ol[]"
    Line.to_left
  end

  def self.enter_log_output
    orig = Location.new
    View.layout_output
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
    o.to_source
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

      View.clear "*ol"
      View.to_window location
      return true   # Indicate to exit method
    end
    return false   # Don't exit
  end

  def self.do_list_ancestors
    prefix = Keys.prefix :clear=>1

    result = Tree.ancestors_indented

    result = result.strip.gsub('%', '%%')

    # If U, save in clipboard as quote, ready to be pasted into a tree
    #     Clipboard[0] = result if Keys.prefix_u

    if prefix == :-   # Actually insert it inline
      Line.next
      View << "#{result}\n"
      Line.previous
      return
    end

    if prefix == :u   # Just recenter to method
      View.recenter_under "^\\( *def \\| *it \\|^>\\)", :relative=>1
    elsif prefix == :uu
      result = "Cursor: #{View.cursor}"
    end

    View.message result
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

    return View.open(file.sub /\.menu$/, '.rb') if file =~ /\/menu\/\w+\.menu$/
    return View.open(file.sub /\.rb$/, '.menu') if file =~ /\/menu\/\w+\.rb$/

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

    if prefix == :u
      column = View.column
      Move.to_axis; View << "\n"
      Move.to_end; View >> "\n"
      View.column = column
      return
    end

    $el.open_line(prefix || 1)

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
    if Line.blank?
      View << "# "
      $el.ruby_indent_line
    else
      Move.to_end
      View << "   # "
    end

    ControlLock.disable    # insert date string (and time if C-u)
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
        |+
        |+  def self.#{method}
        |+
        |+  end
        | end
      ".unindent

    View.line = 4
    View.column = 2

  end

  # Grabs /foo/bar.rb:123 pattern from line, and jumps to file / line.
  def self.open_as_file
    return if Line.value !~ /(\/.+?):(\d+)/

    file, line = $1, $2
    View.open file
    View.line = line
  end

end

