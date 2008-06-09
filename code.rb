require 'block'
require 'stringio'

class Code
  extend ElMixin

  CODE_SAMPLES = %q<
    # Eval code
    - get output and stdout: p Code.eval("puts 'printed'; 1 + 2")
  >

  def self.location_from_id id
    path = ObjectSpace._id2ref(id).to_s
    path =~ /@(.+):([0-9]+)/
    file, line = $1, $2
    [file, line]
  end

  def self.comment
    comment_or_uncomment_region(region_beginning, region_end)
  end

  def self.run   # Evaluates file, paragraph, or next x lines using el4r

    prefix = Keys.prefix

    case prefix
    when 0  # Do paragraph (aka "block" for some reason)
      left, right = Block.value
    when 7  # Do region
      left, right = region_beginning, region_end
    # If prefix of 8, run in rails console
    when :u  # In rails console
      return self.load_this_file
    when :uu  # In rails console
      return self.run_in_rails_console
    when 8  # Block on command line
      return cm_pass_block_to_command("ruby -I.", " myTestCommandLineArg", "ruby")
    when 9  # Pass whole file as ruby
      return Shell.run("ruby #{View.file_name}", :buffer => "**ruby")

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
    insert "#{exception.message}\n" if exception

    # Move cursor back to where we started
    orig.go
    # Go to where we ended up
    ended_up.go

  end

  def self.run_in_rails_console
    left, after_header, right = View.block_positions "^|"
    block = buffer_substring after_header, right

    View.to_after_bar if View.in_bar?

  #  other_window 1
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

  #  other_window -1
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
    insert "Ml << \"#{txt}: \#{#{txt}}\""
  end

  def self.indent
    indent_region(* View.range)
  end

  # Evaluates a string, and returns the output and the stdout string generated
  def self.eval code
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
    unless Keys.prefix_u
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
    Shell.run "spec #{spec}#{test}", :dir => dir, :buffer => '*spec', :reuse_buffer => true

  end

  def self.load_this_file
    Effects.blink :what => :all
    load View.file
  end

  def self.do_code_align
    left, right = bounds_of_thing_at_point(:paragraph).to_a
    align_regexp(left, right, Keys.input(:prompt => "align to regex: "))
  end

  def self.indent_to
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
    orig.go
  end

  def self.enter_as_backslash
    txt = Clipboard.get("0")
    txt.strip!
    txt.gsub!(/$/, "\\")  # Add \'s
    txt.gsub!(/\\\z/, '')  # Remove last \
    insert txt
  end
end
