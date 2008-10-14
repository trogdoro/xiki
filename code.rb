require 'block'
require 'stringio'
gem 'ruby2ruby'
require 'ruby2ruby'

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

  def self.comment left=nil, right=nil
    left ||= region_beginning
    right ||= region_end
    comment_or_uncomment_region(left, right)
    Code.indent
  end

  def self.run   # Evaluates file, paragraph, or next x lines using el4r
    prefix = Keys.prefix

    case prefix
    when 0   # Do paragraph (aka "block" for some reason)
      left, right = Block.value
    when 7   # Do region
      left, right = region_beginning, region_end
    # If prefix of 8, run in rails console
    when :u   # In rails console
      return self.load_this_file
    when :uu   # In rails console
      return self.run_in_rails_console
    when 8   # Put into file and run in console
      File.open("/tmp/tmp.rb", "w") { |f| f << Notes.get_block.text }
      return Console.run "ruby -I. /tmp/tmp.rb", :dir=>View.dir
    when 9   # Pass whole file as ruby
      return Console.run("ruby #{View.file_name}", :buffer => "**ruby")

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

    # Blink
    Effects.blink :left => left, :right => right

    orig = Location.new
    goto_char right; after_code = Location.new  # Remember right of code
    orig.go

    # Eval the code
    returned, out, exception = self.eval(buffer_substring(left, right).to_s)
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
    end_of_buffer

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

  def self.indent
    indent_region(* View.range)
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

  def self.do_as_rspec

    test = ""
    # If not U, only run this test
    unless Keys.prefix_u?
      orig = Location.new
      Line.next
      if Search.backward("^ *it ")
        test = " -e " + Line.value[/".+"/]
        orig.go
      else
        beep
        message("Not currently in a spec!")
        Line.previous
        return
      end
    end

    path = buffer_file_name
    # Chop off up until before /spec/
    dir, spec = path.match(/(.+)\/(spec\/.+)/)[1,2]
    Console.run "spec #{spec}#{test}", :dir => dir, :buffer => '*spec', :reuse_buffer => true

  end

  def self.load_this_file
    Effects.blink :what => :all
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
    View.insert l.uniq.join("\n")
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

  def self.enter_log
    View.insert "Ol << \"\""
    Move.backward
  end

  def self.enter_log_console
    View.insert "console.log(\"\");"
    Move.backward 3
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
