# Meant to log short succinct messages to help with troubleshooting
# while coding.  Log statements hyperlink back to the line that logged it.
class Ol
  @@last = [Time.now - 1000]
  @@timed_last = Time.now

  # For when the caller constructs what to log on its own
  def self.log_directly txt, line, name=nil
    path = name ? "/tmp/#{name}_ol.notes" : self.file_path
    self.write_to_file path, txt
    self.write_to_file_lines path, line
  end

  def self.log txt, l=nil, name=nil, time=nil
    path = name ? "/tmp/#{name}_ol.notes" : self.file_path

    if l.nil?   # If just txt, delegate to line
      self
      return self.line(txt, caller(0)[1])
    end

    # If n seconds passed since last call
    heading = self.pause_since_last?(time) ? "\n>\n" : nil

    if l.is_a?(Array)   # If an array of lines was passed
      result = ""
      result_lines = ""
      if heading
        result << heading
        result_lines << "\n\n"
      end
      l.each_with_index do |o, i|
        next unless o
        h = Ol.parse_line(o)
        result << "#{'  '*i}#{self.extract_label(h)}#{i+1 == l.size ? " #{txt}" : ''}\n"
        result_lines << "#{h[:path]}:#{h[:line]}\n"
      end
      self.write_to_file path, result
      self.write_to_file_lines path, result_lines
      return txt
    end

    # Indent lines if multi-line (except for first)
    txt.gsub!("\n", "\n  ")
    txt.sub!(/ +\z/, '')   # Remove trailing

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
    File.open(path, "a") { |f| f << txt }
  end

  def self.write_to_file_lines path, txt
    File.open("#{path}.lines", "a") { |f| f << txt }
  end

  def self.pause_since_last? time=nil, no_reset=nil
    time ||= @@last
    difference = Time.now - time[0]
    time[0] = Time.now unless no_reset
    difference > 3
  end

  def self.<< txt
    self.line txt, caller(0)[1]
  end

  def self.time nth=1
    now = Time.now
    elapsed = self.pause_since_last? ? nil : (now - @@timed_last)

    self.line "#{elapsed ? "(#{elapsed}) " : ''}#{now.strftime('%I:%M:%S').sub(/^0/, '')}:#{now.usec.to_s.rjust(6, '0')}", caller(0)[nth]
    @@timed_last = now
  end



  def self.line txt=nil, l=nil, indent="", name=nil, time=nil
    l ||= caller(0)[1]
    h = self.parse_line(l)

    txt = txt ? " #{txt}" : ''

    if h[:clazz]
      self.log "#{indent}#{self.extract_label(h)}#{txt}", l, name, time
    else
      display = l.sub(/_html_haml'$/, '')
      display.sub!(/.+(.{18})/, "\\1...")
      self.log "#{indent}- #{display}:#{txt}", l, name, time
    end
  end

  def self.extract_label h
    "- #{h[:clazz]}.#{h[:method]} (#{h[:line]}):"
  end


  def self.parse_line path
    method = path[/`(.+)'/, 1]   # `
    path, l = path.match(/(.+):(\d+)/)[1..2]
    path = File.expand_path path
    clazz = path[/.+\/(.+)\.rb/, 1]
    clazz = self.camel_case(clazz) if clazz
    {:path=>path, :line=>l, :method=>method, :clazz=>clazz}
  end

  def self.file_path
    "/tmp/output_ol.notes"
  end

  def self.camel_case s
    s.gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")
  end

  # Logs short succinct stack trace
  def self.stack n=6, nth=1
    ls ||= caller(0)[nth..(n+nth)]

    self.line "stack...", ls.shift, ""

    ls.each do |l|
      self.line nil, l, "  "
    end
  end

  def self.limit_stack stack, pattern=/^\/projects\//
    # Cut off until it doesn't match
    first = stack.first
    stack.delete_if{|o| o !~ pattern}
    stack.reverse!
    # Be sure to leave one, if they're all deleted
    stack << first if stack == []
    stack
  end

  # Remove parents from stack that are common to last_stack
  def self.remove_redundance stack, last_stack
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

end
