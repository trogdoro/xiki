# Meant to log short succinct messages to help with troubleshooting
# while coding.  Log statements hyperlink back to the line that logged it.

require 'fileutils'

class Ol
  @@last = [Time.now - 1000]
  @@timed_last = {""=>Time.now}
  @@roots = []

  def self.menu
    %`
    - .extract test/
    - docs/
      - summary/
        | "Ol" is short for "out log".  Add lines like "Ol()" to your code
        | to print stuff out, see the execution flow, or help debug.  It's
        | similar to adding "print" statements to your code.
        |
        | The output immediately shows up in the "ol" view on the bottom-left
        | of the screen (type layout+outlog to see it).  You can double-click
        | on the output to navigate back to the log statements, and step
        | through and "replay" what happened.
        |
        | This class has zero dependencies, so you can include it in other
        | apps to debug them, like a rails app.
      - navigating/
        - from statements/
          | With the cursor on a log statement, type layout+output to jump
          | to the last corresponding output in the outlog view.
        - from outlog/
          | With the cursor in teh outlog view, type control+return to jump
          | to the corresponding log statement.
        - step through/
          | Type 0+tab (Ctrl-0 Ctrl-tab) to step to the next visible
          | statement in the 'ol' view.  Then type Ctrl+tab subsequent
          | times to step through the other statements.
          |
          | It pulls the
          | values from log statements with, and adds the as comments
          | in the code.  When it gets to the end, it starts over at
          | the top of the last visible block.
      - key shortcuts/
        @facts/
          | show the outlog : layout+outlog
          | insert "Ol()" : enter+log+line
          | insert "Ol.stack": enter+log+stack
          | insert "Ol.time" : enter+log+time
          | insert "Ol.root" : enter+log+root
          | step through statements : 0+tab
      - review/
        @facts/
          | log that line ran : Ol()
          | log a string : Ol "hi"
          | log label and value : Ol "foo", foo
          | log a stack trace : Ol.stack
        - misc/
          - old tree implementation/
            @facts/
              | 2 methods that facilitate trees : .remove_extraneous, .remove_common_ancestors
              | .remove_extraneous does... : removes lines in dirs we don't care about
              | .remove_common_ancestors does... : removes lines we've already logged
    - api/
      - show line ran/
        | Ol()
      - values/
        | Ol "foo"
        | Ol "user", ENV["USER"]   # With a label
      - unquotetd/
        | Ol["foo"]
      - emphasis/
        | Ol["!"]   # Shown as green
        | Ol["foo!"]
      - timing/
        | Ol.time
        | sleep 1
        | Ol.time   # Shows elapsed time
      - multiple values/
        | Ol.>> 3, 4, 5
      - stack/
        | Ol.stack   # 6 levels deep
        | Ol.stack 3   # 3 levels deep
      - trees/
        | Ol.root   # Indent any subsequent statements that are nested
        |           # under this method.
        - example/
          @/tmp/
            - nested.rb
              | require '/projects/xiki/lib/xiki/core/ol.rb'
              | def a
              |   Ol.root
              |   b
              |   Ol()
              | end
              | def b
              |   Ol()
              |   c
              | end
              | def c
              |   Ol["deepest point"]
              | end
              |
              | a()
              | p "Check out the 'ol' view to see the nesting!"
            $ ruby nested.rb
    `
  end

  # Called by .line, to do the work of writing to the file
  #
  # Ol.log "hey"
  def self.log txt, l, options=nil

    path = self.file_path

    l_raw = l.dup

    # If l is array (it's the whole trace), use only first for now
    l = l[0].dup if l.is_a?(Array)

    if l.nil?   # If Ol.log "foo" called directly (probably never happens
      return self.line(txt, caller(0)[1])
    end

    heading = nil
    if self.pause_since_last? # time
      # If n seconds passed since last call
      heading = "\n>\n"

      # Set root to just this, if there isn't one yet
      @@roots = [self.nesting_match_regexp(l)]
    end

    # If .root called recently, check for match in stack...

    root_offset_indent = ""
    if @@roots.any?

      # If they passed in a trace, use it instead of this one
      trace = l_raw.is_a?(Array) ?
        l_raw :
        caller(0)[3..-1]   # Omit Ol calls

      root_offset = options[:indent] || self.nesting_match(trace)
      root_offset_indent = "  " * (root_offset||0)

      # Add this to @@roots if none found
      regex = self.nesting_match_regexp l
      txt = "#{txt}"
      @@roots << regex if ! root_offset && ! @@roots.member?(regex) && ! (options && options[:leave_root])
    else
      txt = "#{txt}"
      root_offset_indent = ""
    end

    # Remove trailing linebreaks
    txt.sub! /\n+\Z/, "" if txt

    # Indent lines if multi-line (except for first)
    if label = options && options[:label]
      # add on label
      if txt =~ /\n/   # If multi-line
        txt.gsub!(/^/, "  #{root_offset_indent}| ")
        txt.gsub!(/^( +\|) $/, "\\1")
        txt = "#{root_offset_indent}#{label}\n#{txt}"
      else
        txt = "#{root_offset_indent}#{label}#{txt =~ /./ ? " #{txt}" : ""}"
      end
    else
      txt.gsub!("\n", "\n  #{root_offset_indent}")
      txt.sub!(/ +\z/, '')   # Remove trailing
    end

    h = Ol.parse_line(l)

    self.write_to_file path, "#{heading}#{txt}\n"

    # Multiline txt: Write path to .line file once for each number of lines
    l = "#{h[:path]}:#{h[:line]}\n"
    result = ""

    result << "\n\n" if heading

    txt.split("\n", -1).size.times { result << l }
    self.write_to_file_lines "#{path}", result

    txt
  end

  def self.write_to_file path, txt
    existed = File.exists? path   # If file doesn't exist, chmod it to world writable later

    File.open(path, "a") do |f|
      f << txt
      f.chmod 0666 if ! existed
    end
  end

  def self.write_to_file_lines path, txt
    path = "#{path}.lines"
    existed = File.exists? path   # If file doesn't exist, chmod it to world writable later

    File.open(path, "a", 0666) do |f|
      f << txt
      f.chmod 0666 if ! existed
    end
  end

  def self.pause_since_last? time=nil, no_reset=nil
    time ||= @@last
    difference = Time.now - time[0]
    time[0] = Time.now unless no_reset

    difference > 5
  end

  # Remove eventually!
  def self.<< txt
    self.line txt, caller(0)[1]
  end

  def self.>> *args
    self.line args.inspect, caller(0)[1]
  end

  # Assume 1 string, that's just a label
  def self.[] *args
    txt = args.join ", "

    self.line txt, caller(0)[1]
  end

  # Deprecated in favor of a
  def self.ap *args
    self.a *args
  end

  def self.b *args
    args << {:truncate=>1}
    self.a *args#, :truncate=>1
  end

  def self.a *args
    options = args.pop if args.length > 1 && args[-1].is_a?(Hash)
    options ||= {}

    caption = args.shift if args.length > 1   # If 2 args, 1st is caption
    txt = args.shift

    if txt.is_a?(String)   # If string, show as multi-line (not inspect)
      txt = txt.dup
    elsif txt.respond_to?("ai")
      txt = txt.ai

      txt.gsub!(/^  /, "") if txt =~ /\A /
      if txt.count("\n") == 2   # If only one item in hash or array
        txt.gsub! /\n */, ""   # Leave brackets, so it's obvious what it is
      else
        txt.sub!(/\A[\[{]\n/, '')
        txt.sub!(/\n[\]}]\z/, '')
      end
    else   # .ai not defined, so just inspect and add message
      txt = "Do 'gem install awesome_print' for Ol.a to work properly. In the mean time, using .inspect:\n#{txt.inspect}"
    end

    line = options[:stack_line] || caller(0)[1]

    if options[:truncate]
      txt = "#{txt[0..100]}#{txt.length > 100 ? "\n[...]" : ''}"
    end

    self.line txt, line, :caption=>caption
  end

  def self.yaml *args
    txt = args.shift
    txt = txt.to_yaml

    txt.sub! /.+\n/, ''   # Remove 1st line - it's ---...
    txt.gsub!(/[:-] $/, "")

    self.line txt, caller(0)[1]
  end


  def self.xi *args
    txt = args.shift
    txt = Xi txt

    self.line txt, caller(0)[1]
  end


  def self.i txt
    self.line txt.inspect, caller(0)[1]
  end


  # The 'tag' param lets you have separate timers
  def self.time tag="" #nth=1

    @@timed_last[tag] ||= Time.now

    now = Time.now
    elapsed = self.pause_since_last? ? nil : (now - @@timed_last[tag])
    @@timed_last[tag] = now

    tag = "(#{tag}) " if tag != ""

    self.line "time #{tag}> #{elapsed ? "(#{elapsed}) " : ''}#{now.strftime('%I:%M:%S').sub(/^0/, '')}:#{now.usec.to_s.rjust(6, '0')}", caller(0)[1]
                                                             # self.line "time > #{elapsed ? "(#{elapsed}) " : ''}#{now.strftime('%I:%M:%S').sub(/^0/, '')}:#{now.usec.to_s.rjust(6, '0')}", caller(0)[1]
  end

  # The primary method of this file
  def self.line txt=nil, l=nil, options=nil
    l ||= caller(0)[1]
    l_raw = l.dup

    # If l is array (it's the whole trace), use only first here
    return if l == []   # Guard against error when stack is too deep?

    l = l[0].dup if l.is_a?(Array)

    l.sub! /^\(eval\)/, 'eval'   # So "(" doesn't mess up the label
    l.sub!(/ in <.+/, '')

    h = self.parse_line(l)

    return if @filter && l !~ /^#{Regexp.quote @filter}/

    options ||= {}
    if h[:clazz]
      self.log txt.to_s, l_raw, options.merge(:label=>self.extract_label(h))
    else
      display = l.sub(/_html_haml'$/, '')
      display.sub! /.*\//, ''   # Chop off path (if evalled)
      display.sub! /:in `eval'$/, ''
      display.sub!(/.+(.{18})/, "...\\1")   # Concat if really long

      label = "- #{display})"
      label << " #{options[:caption]}" if options[:caption]

      self.log txt.to_s, l_raw, options.merge(:label=>label)

    end
    nil
  end

  def self.extract_label h
    "- #{h[:clazz]}.#{h[:method]}:#{h[:line]})"
  end

  def self.parse_line path
    method = path[/`(.+)'/, 1]   # `
    path, l = path.match(/(.+):(\d+)/)[1..2] rescue ['not found', '1']
    path = File.expand_path path
    clazz = path[/.+\/(.+)\.rb/, 1]
    clazz = self.camel_case(clazz) if clazz
    {:path=>path, :line=>l, :method=>method, :clazz=>clazz}
  end

  def self.file_path
    return @file_path if @file_path

    # File not set yet, so create dir if necessary
    dir = File.expand_path "~/.xiki/misc/ol/"
    FileUtils.mkdir_p dir
    @file_path = "#{dir}/out_ol.xiki"

  end

  def self.camel_case s
    s.gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")
  end

  # Logs short succinct stack trace
  def self.ancestors n=6, nth=1

    ls = caller(0)[nth..(n+nth)]

    self.line "ancestors...", ls.shift, ""

    ls.each_with_index do |l, i|
      self.line((i+1).to_s, l, :leave_root=>1, :indent=>2)
    end

    nil
  end

  # If stack is very deep, stop execution
  # Ol.limit 200   # Does nothing because stack smaller than 200
  # Ol.limit 4     # Raises because stack deeper than 4
  def self.limit n=50

    stack = caller(0)

    # Too deep...

    if stack.length > n
      self.stack n, 1, :caller=>stack #[n..-1]
      raise "Ol.limit raised, because stack exceeded #{n} levels deep"
    end

    # Not too deep, so show "less than N"...

    self.line "stack limit less than #{n}", stack[1..-1]

    nil
  end


  # Logs stack with indenting, like:
  # Ol.stack
  # Foo.a)
  #   Boo.b)
  #     Coo.c) ...stack
  def self.stack n=4, nth=1, options={}
    ls = options[:caller] || caller(0)[nth..-1]

    n -= 1

    n.downto(1) do |i|
      self.line("|"*(n-i+1), ls[i..-1])
    end

    last = options[:exclamation] ? "!" : "-"

    pipes = "|"*n+"|#{last}"
    self.line pipes, ls, :leave_root=>1

    nil
  end

  # Removes lines from list not matching a pattern and reverses list, always leaving one.
  # Ol.remove_extraneous(["/a/a", "/a/b", "/gems/c"], /^\/a\//).should == ["/a/b", "/a/a"]
  def self.remove_extraneous stack, pattern=/^\/projects\//
    # Cut off until it doesn't match
    first = stack.first
    stack.delete_if{|o| o !~ pattern}
    stack.reverse!
    # Be sure to leave one, if they're all deleted
    stack << first if stack == []
    stack
  end

  # Removes ancestors from stack that are in last_stack.
  # Ol.remove_common_ancestors(["/common/a", "/a/b"], ["/common/a", "/a/x"]).should == [nil, "/a/b"]
  def self.remove_common_ancestors stack, last_stack
    result = []
    # For each stack, copy it over if different, or nil if the same
    stack.each_with_index do |o, i|
      # If it's the last one, don't nil it out
      if i+1 == stack.size
        result << o
        next
      end

      result << (o == last_stack[i] ? nil : o)
    end
    result
  end

  def self.browser html
    path = "/tmp/browser.#{Time.now.usec}.html"
    url = "file://#{path}"
    File.open(path, "w") { |f| f << html }

    `open '#{url}'`
  end


  # Check the first few lines in the stack for a match
  def self.nesting_match stack, roots=nil
    roots ||= @@roots   # Param is for specs to over-ride

    limit = 22
    limit = (stack.length-1) if stack.length <= limit

    # Don't have indenting if recursion.
    # This might cause weirdness!
    #     roots.each{|root| return 0 if root =~ stack[0]}

    # We sure we want to go backwards?!
    #     limit.downto(0) do |i|
    # Start with parent and then go lower, so we find the closest
    #     1.upto(limit) do |i|
    limit.downto(1) do |i|
      roots.each{|root| return i if root =~ stack[i]}
    end
    nil
  end


  # p ("/tmp/a.rb:45:in `speak'" =~ Ol.nesting_match_regexp("/tmp/a.rb:46:in `speak'")).should == 0
  # p Ol.nesting_match_regexp("aa:45:bb").should == /^aa:[0-9]+:bb$/
  def self.nesting_match_regexp txt
    # File.open("/tmp/simple.log", "a") { |f| f << ".nesting_match_regexp: #{txt}\n" }
    # File.open("/tmp/simple.log", "a") { |f| f << "stack: #{caller(0)[2..5].join("\n")}\n" }
    txt = "^#{Regexp.quote txt}$"
    txt.sub! /:[0-9]+:/, ":[0-9]+:"
    Regexp.new(txt)
  end

  # Make this point the "root" in the call tree, so future calls
  # underneath it will show up indented.
  def self.root txt=nil
    line = caller(0)[1]
    self.line txt, line.dup
    regex = self.nesting_match_regexp line
    @@roots << regex if ! @@roots.member? regex
  end

  def self.roots
    @@roots
  end

  # Make sure new >... block is created in the outlog
  def self.clear_pause
    @@last = [Time.now - 1000]
  end

  def self.stub label, value
    line = caller(0)[1]
    txt = "stub(#{label.sub(".", ").")} {#{value.inspect}}"
    self.line txt, line.dup
  end
  def self.mock label, value
    line = caller(0)[1]
    txt = "mock(#{label.sub(".", ").")} {#{value.inspect}}"
    self.line txt, line.dup
  end

  def self.should label, value
    line = caller(0)[1]
    txt = "#{label.sub(".", ").")}.should == #{value.inspect}"
    self.line txt, line.dup
  end

  def self.extract_test
    log = Buffers.txt "ol"
    log.sub! /.+\n\n/m, ''

    txt = ""
    log.scan(/\) (mock.+|stub.+|.+\.should == .+)/) do |line|
      txt << "#{line[0]}\n"
    end
    Tree.quote txt
  end

  def self.grab_value value
    value.sub! /.+?\) ?/, ''   # Remove ...)
    value.sub! /.+?: /, ''   # Remove ...: if there

    # Why were we clearing these > leave them off for a while?
      # Clear value unless it looks like a literal: "foo", 1.1, [1, 2], true, nil, etc.
      #     value = "" if value !~ %r'^[\["{]' && value !~ /^true|false|nil|[0-9][0-9.]*$/

    value
  end

  def self.update_value_comment value
    column = Xiki::View.column
    value = "   #> #{value}"
    Xiki::Line =~ /   #> / ?
      Xiki::Line.sub!(/(.*)   #> .*/, "\\1#{value}") :   # Line could be commented, so don't replace after first "#"
      Xiki::Line.<<(value)
    Xiki::View.column = column
  end

  class << self
    attr_accessor :filter
  end

end

def Ol *args
  txt =
    if args == []
      nil
    elsif args.length == 1   # Just text
      args[0].is_a?(String) ? args[0] : args[0].inspect
    else   # Label and text
      "#{args[0]}: #{args[1].inspect}"
    end

  Ol.line txt, caller(0)[1]
end
