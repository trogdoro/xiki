require 'block'
require 'stringio'
gem 'ruby2ruby'
require 'ruby2ruby'
require 'parse_tree'
require 'parse_tree_extensions'
class Code
  extend ElMixin

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
    # If 0 prefix, comment paragraph
    if Keys.prefix == 0
      left, right = View.paragraph(:bounds => true)
    else
      Line.to_left
      left ||= {:default=>:region}
      left, right = Code.bounds_of_thing(left, right)
      left, right = right, left if View.cursor == right   # In case cursor is at right side
    end

    View.to left
    View.set_mark right

    comment_or_uncomment_region View.range_left, View.range_right
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
      txt, left, right = View.txt_per_prefix nil, :blink=>1, :remove_heading=>1
    else
      case prefix
      when :u   # Load file in emacsruby
        return self.load_this_file

        # These were superceded by .txt_per_prefix apparently

      when 8   # Put into file and run in console
        File.open("/tmp/tmp.rb", "w") { |f| f << Notes.get_block("^>").text }
        return Console.run "ruby -I. /tmp/tmp.rb", :dir=>View.dir
      when 9   # Pass whole file as ruby
        return Console.run("ruby #{View.file_name}", :buffer => "*console ruby")
      else   # Move this into ruby - block.rb?
        ignore, left, right = View.block_positions "^>"
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
    returned, out, exception = self.eval txt
    begin
      if returned.any?
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
      backtrace = exception.backtrace[0..8].join("\n").gsub(/^/, '  ') + "\n"
      View.insert "- error: #{exception.message}\n- backtrace:\n#{backtrace}".gsub(/^/, '  ')
    end

    orig.go   # Move cursor back to where we started
    ended_up.go   # Go to where we ended up
  end

  def self.run_in_rails_console
    left, after_header, right = View.block_positions "^>"
    block = buffer_substring after_header, right

    View.to_after_bar if View.in_bar?

    View.to_buffer "*rails console"
    erase_buffer
    View.to_bottom

    #     if elvar.current_prefix_arg
    #       insert "reload!"
    #       command_execute "\C-m"
    #     end

    insert block
    command_execute "\C-m"

    beginning_of_buffer

  end

  def self.show_el4r_error
    View.open elvar.el4r_log_path
    revert_buffer(true, true, true)
    View.wrap
    View.to_end
    re_search_backward "^  from "
    re_search_backward "^[A-Z]"
    recenter 0
    Color.colorize :r
  end

  def self.enter_ruby_log
    txt = Clipboard.get("0")
    insert "Ol << \"#{txt}: \#{#{txt}}\""
  end

  def self.indent left=nil, right=nil
    left, right = View.range if left.nil?
    indent_region(left, right)
  end

  # Evaluates a string, and returns the output and the stdout string generated
  def self.eval code
    return ['- Warning: nil passed to Code.eval!', nil, nil] if code.nil?
    # Capture stdout output (saving old stream)
    orig_stdout = $stdout;  $stdout = StringIO.new
    stdout = nil
    exception = nil
    begin   # Run code
      # Good place to debug
      returned = $el.el4r_ruby_eval(code)
    rescue Exception => e
      exception = e
    end
    stdout = $stdout.string;  $stdout = orig_stdout  # Restore stdout output
    [returned, stdout, exception]
  end

  def self.do_as_align
    align_regexp
  end

  def self.open_related_rspec
    if View.file =~ /\/xiki\//   # If in xiki project
      if View.file =~ /\/spec\//   # If in spec, open corresponding file
        View.open View.file.sub('/spec/', '/').sub(/_spec\.rb/, '.rb')
      else   # Otherwise, open file corresponding spec
        View.open View.file.sub(/(.+)\/(.+)/, "\\1/spec/\\2").sub(/\.rb/, '_spec.rb')
      end
      return
    end

    if View.file =~ /\/(app|spec)\//   # If normal specs
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
    p meth
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
      Tree << "- Error:\n#{e.message.gsub /^/, '  '}!"
    end
  end

  def self.do_code_align
    left, right = bounds_of_thing_at_point(:paragraph).to_a
    align_regexp(left, right, "\\( *\\)"+Keys.input(:prompt => "align to regex: "), 1, 1, false)
  end

  def self.indent_to

    # If universal, indent current line 2 over
    if Keys.prefix_u
      cursor = View.cursor
      Move.to_axis
      View.insert "  "
      View.to cursor + 2
      return
    end

    if Keys.prefix_uu
      orig = View.cursor
      Move.to_axis
      was_near_axis = View.cursor+2 > orig
      View.delete View.cursor, View.cursor+2
      View.to orig-2 unless was_near_axis
      return
    end

    # If universal, indent current line 2 to left

    if View.cursor == View.mark   # If C-space was just hit, manually indent this line
      prefix = (Keys.prefix_n :clear=>true) || 0
      old_column = View.column
      line = Line.value 1, :delete=>true

      old_indent = line[/^ */].size
      View.insert line.sub(/^ */, (' ' * prefix))

      old_column == 0 ?
      Move.to_axis :
        Move.to_column(old_column + (prefix - old_indent))   # Move to old location + indent diff

      return
    end

    # If no prefix, just indent code according to mode
    return Code.indent if ! Keys.prefix

    new_indent = Keys.prefix || 0
    orig = Location.new
    txt = View.selection :delete => true   # Pull out block

    txt = TextUtil.unindent(txt)
    txt.gsub!(/^/, ' ' * new_indent)   # Add back new indent
    txt.gsub!(/^ +$/, '')   # Blank out lines with just spaces

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
    insert txt
  end

  #   def self.enter_as_trunk
  #     bm = Keys.input(:timed => true, :input => 'Enter bookmark: ')
  #     bm = Bookmarks["$#{bm}"]
  #     if Keys.prefix_u?
  #       View.insert "#{bm}\n  ##/"
  #       $el.backward_char
  #       return ControlLock.disable
  #     end
  #     View.insert "#{bm}\n  ###{Clipboard[0]}/"
  #     #View.insert "$tr/###{Clipboard[0]}/"
  #     Launcher.launch
  #   end

  def self.enter_as_debug

    orig = View.range[0]
    txt = View.selection :delete=>true
    count = 0
    txt.gsub!(/^.+/) { |m|
      if m =~ /^\s+(end|else|elsif|})/
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
    View.insert "Ol.stack"
  end

  def self.enter_log_console
    View.insert "console.log();"
    Move.backward 2
  end

  def self.open_log_view
    prefix = Keys.prefix :clear=>true
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
    $el.open_line(1) unless Line.blank?

    # Javascript
    if Tree.construct_path(:all=>1, :slashes=>1) =~ /<script/
      View << "console.log('!');"
      return Move.backward 4
    end

    if Keys.prefix_u?
      View.insert 'Ol << "!"'
      return Move.backward 2
    end

    View.insert "Ol.line"
    Line.to_left
  end

  def self.enter_log_time
    $el.open_line(1) unless Line.blank?
    View.insert("Ol.time")
    Line.to_left
  end

  def self.to_ruby o
    o.to_ruby
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
    path = Tree.construct_path(:list=>1, :ignore_ol=>1)[0..-1]
    result = ""
    path.each_with_index { |o, i|
      result << "#{'  ' * i}#{o}\n"
    }

    result = result.strip.gsub('%', '%%')

    # If U, save in clipboard as quote, ready to be pasted into a tree
    Clipboard[0] = result if Keys.prefix_u

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

    return View.open(file.sub /\.menu$/, '.rb') if file =~ /\/menus\/\w+\.menu$/
    return View.open(file.sub /\.rb$/, '.menu') if file =~ /\/menus\/\w+\.rb$/

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

end

