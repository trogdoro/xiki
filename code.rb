require 'block'
require 'stringio'
gem 'ruby2ruby'
require 'ruby2ruby'
require 'parse_tree'
require 'parse_tree_extensions'
class Code
  extend ElMixin

  CODE_SAMPLES = %q<
    # Eval code
    - get output and stdout: p Code.eval("puts 'printed'; 1 + 2")
  >

  def self.location_from_proc id
    path = id.to_s
    path =~ /@(.+):([0-9]+)/
    file, line = $1, $2
    [file, line]
  end

  def self.bounds_of_thing left=nil, right=nil
    if left == :line
      left, right = Line.left, Line.right+1
    elsif left.nil?
      n = Keys.prefix_n(:clear=>true)   # Check for numeric prefix
      if n   # If prefix, move down
        return [Line.left, Line.left(n+1)]
      else
        left, right = View.range
      end
    end
    [left, right]
  end

  def self.comment left=nil, right=nil
    Line.to_left
    left, right = Code.bounds_of_thing(left, right)
    left, right = right, left if View.cursor == right   # In case cursor is at right side
    View.to left
    View.set_mark right

    comment_or_uncomment_region View.range_left, View.range_right
    Code.indent View.range_left, View.range_right

  end

  def self.run   # Evaluates file, paragraph, or next x lines using el4r
    prefix = Keys.prefix
    if prefix.is_a?(Fixnum) && 0 <= prefix && prefix <= 7
      txt, left, right = View.txt_per_prefix
    else
      case prefix
      when :u   # Load file in emacsruby
        return self.load_this_file
      when 8   # Put into file and run in console
        File.open("/tmp/tmp.rb", "w") { |f| f << Notes.get_block.text }
        return Console.run "ruby -I. /tmp/tmp.rb", :dir=>View.dir
      when 9   # Pass whole file as ruby
        return Console.run("ruby #{View.file_name}", :buffer => "*console ruby")

      # If prefix of 1-6
      when 1..6
        started = point
        left = Line.left
        right = point_at_bol(elvar.current_prefix_arg+1)
        goto_char started
      else
        # Move this into ruby - block.rb?
        ignore, left, right = View.block_positions "^|"
      end

      txt = View.txt(:left=>left, :right=>right).to_s
      Effects.blink :left => left, :right => right
    end

    orig = Location.new
    goto_char right; after_code = Location.new  # Remember right of code
    orig.go

    # Eval the code
    returned, out, exception = self.eval(txt)
    begin
      message returned.to_s if returned.to_s.size < 50
    rescue
    end
    ended_up = Location.new

    # Insert output
    after_code.go
    insert out
    if exception
      backtrace = exception.backtrace[0..8].join("\n").gsub(/^/, '  ') + "\n"
      insert "- error: #{exception.message}\n- backtrace:\n#{backtrace}"
    end

    # Move cursor back to where we started
    orig.go
    # Go to where we ended up
    ended_up.go

  end

  def self.run_in_rails_console
    left, after_header, right = View.block_positions "^|"
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
    begin
      # Run code
      returned = el4r_ruby_eval(code)
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
    if View.file =~ /\/(app|spec)\//   # If normal specs
      if View.file =~ /\/app\//   # If in spec, open corresponding file
        View.open View.file.sub('/app/', '/spec/unit/').sub(/\.rb/, '_spec.rb')
      else   # Otherwise, open file corresponding spec
        View.open View.file.sub('/spec/unit/', '/app/').sub(/\_spec.rb/, '.rb')
      end
      return
    end

    if View.file =~ /\/xiki\//   # If in xiki project
      if View.file =~ /\/spec\//   # If in spec, open corresponding file
        View.open View.file.sub('/spec/', '/').sub(/_spec\.rb/, '.rb')
      else   # Otherwise, open file corresponding spec
        View.open View.file.sub(/(.+)\/(.+)/, "\\1/spec/\\2").sub(/\.rb/, '_spec.rb')
      end
      return
    end

    View.beep
    View.message "Don't recognize this file."
  end

  def self.do_as_rspec options={}
    args = []

    if Keys.prefix_u
      args << 'spec/unit'
      args << '-p'
      args << '**/*.rb'

      # If already in shell, don't change dir
      if View.mode == :shell_mode
        dir = nil
      else
        begin
          dir, spec = View.file.match(/(.+)\/(spec\/.+)/)[1,2]
        rescue
          dir, spec = View.file.match(/(.+)\/(app\/.+)/)[1,2]
        end
      end
      # /projects/memorizable/memorizable.merb/app/models/main.rb
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

        unless Keys.prefix == 8   # If not C-8, only run this test
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
            View.cursor = it
            test = Line.value[/"(.+)"/, 1]
            args << '-e'
            args << test
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

    buffer = '*console for rspec'
    # If spec buffer open, just switch to it
    if View.buffer_open? buffer
      View.to_buffer buffer
    else   # Otherwise open it and run console
      Console.run "merb -i", :dir=>dir, :buffer=>buffer
      #       Console.run "merb -i -e test", :dir=>dir, :buffer=>buffer
    end
    View.clear

    #     args << '-D'   # Show diffs

    command = "Spec::Runner::CommandLine.run(Spec::Runner::OptionParser.parse([#{args.map{|o| "\"#{o}\""}.join(",\n")}], $stderr, $stdout))"
    command = "p :reload; #{command}"
    View.insert command
    Console.enter
    #     View.to_highest
    View.to 1
    #       Console.run "spec #{spec}#{test}", :dir=>dir, :buffer=>buffer, :reuse_buffer=>true

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
    load View.file
  end

  def self.do_code_align
    left, right = bounds_of_thing_at_point(:paragraph).to_a
    align_regexp(left, right, "\\( *\\)"+Keys.input(:prompt => "align to regex: "), 1, 1, false)
  end

  def self.indent_to
    # If no prefix, just indent code according to mode
    return Code.indent if ! Keys.prefix
    new_indent = Keys.prefix || 0
    orig = Location.new
    # Pull out block
    txt = View.selection :delete => true
    txt = TextUtil.unindent(txt)
    # Add back new indent
    txt.gsub!(/^/, ' ' * new_indent)
    # Delete lines with just spaces
    txt.gsub!(/^ +$/, '')
    insert txt
    View.set_mark
    orig.go
  end

  def self.enter_as_backslash
    txt = Clipboard.get("0")
    txt.strip!
    txt.gsub!(/$/, "\\")  # Add \'s
    txt.gsub!(/\\\z/, '')  # Remove last \
    insert txt
  end

  def self.enter_as_trunk
    bm = Keys.input(:timed => true, :input => 'Enter bookmark: ')
    bm = Bookmarks["$#{bm}"]
    if Keys.prefix_u?
      View.insert "#{bm}\n  ##/"
      $el.backward_char
      return ControlLock.disable
    end
    View.insert "#{bm}\n  ###{Clipboard[0]}/"
    #View.insert "$tr/###{Clipboard[0]}/"
    LineLauncher.launch
  end

  def self.enter_as_debug

    orig = View.range[0]
    txt = View.selection :delete => true
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

  def self.do_kill_duplicates
    txt = View.selection :delete => true
    l = txt.split("\n")
    orig = Location.new
    View.insert l.uniq.join("\n") + "\n"
    orig.go
  end

  def self.randomize_lines
    txt = View.selection :delete => true
    l = txt.split("\n")
    orig = Location.new
    View.insert l.sort_by{ rand }.sort_by{ rand }.join("\n") + "\n"
    View.set_mark
    orig.go
  end

  def self.do_next_paragraph
    orig = Location.new
    line = Line.value 1, :include_linebreak => true, :delete => true   # Get line
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
    orig = View.current if Keys.prefix_u?

    prefix_u = Keys.prefix_u?
    Keys.prefix = nil

    file = Ol.file_path
    buffer = "*output - tail of #{file}"

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

    Console.run "tail -f #{file}", :buffer => buffer, :dir => '/tmp', :dont_leave_bar => true
    Notes.mode

    return if self.clear_and_go_back orig
  end

  def self.ol_launch
    # TODO: get total_lines - current_line
    distance_to_end = Line.number(View.bottom) - Line.number

    # Go to log.lines and get n from end
    arr = IO.readlines("#{Ol.file_path}.lines")
    line = arr[- distance_to_end]

    path, line = line.split(':')

    View.open path
    View.to_line line.to_i
  end

  def self.enter_log_line
    $el.open_line(1) unless Line.blank?
    View.insert("Ol.line")
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

private
  def self.clear_and_go_back location
    if location   # Go back to starting point
      View.clear
      View.to_window location
      return true   # Indicate to exit method
    end
    return false   # Don't exit
  end
end
