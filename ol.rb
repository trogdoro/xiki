# Meant to log short succinct messages to help with troubleshooting
# while coding.  Log statements hyperlink back to the line that logged it.
class Ol
  @@last = Time.now - 1000
  @@timed_last = Time.now
  def self.log txt, l=nil
    path = self.file_path

    # If just txt, delegate to line
    if l.nil?
      self
      return self.line(txt, caller(0)[1])
    end

    # Indent lines if multi-line (except for first)
    txt.gsub!("\n", "\n  ")
    txt.sub!(/ +\z/, '')   # Remove trailing

    # If n seconds passed since last call
    heading = self.pause_since_last? ? "\n| \n" : nil

    h = Ol.parse_line(l)

    File.open(path, "a") { |f| f << "#{heading}#{txt}\n" }

    l = "#{h[:path]}:#{h[:line]}\n"
    File.open("#{path}.lines", "a") { |f|
      f << "\n\n" if heading
      txt.split("\n", -1).size.times { f << l }
    }

    txt
  end

  def self.pause_since_last?
    difference = Time.now - @@last
    @@last = Time.now
    difference > 3
  end

  def self.<< txt
    self.line txt, caller(0)[1]
  end

  def self.time
    now = Time.now
    elapsed = self.pause_since_last? ? nil : (now - @@timed_last)

    self.line "#{elapsed ? "(#{elapsed}) " : ''}#{now.strftime('%I:%M:%S').sub(/^0/, '')}:#{now.usec.to_s.rjust(6, '0')}", caller(0)[1]
    @@timed_last = now
  end

  def self.line txt=nil, l=nil, indent=""
    l ||= caller(0)[1]
    h = Ol.parse_line(l)

    txt = txt ? " #{txt}" : ''

    if h[:clazz]
      self.log "#{indent}- #{h[:clazz]}.#{h[:method]} (#{h[:line]}):#{txt}", l
    else
      display = l.sub(/_html_haml'$/, '')
      display.sub!(/.+(.{18})/, "\\1...")
      self.log "#{indent}- #{display}:#{txt}", l
    end
  end

  def self.parse_line path
    method = path[/`(.+)'/, 1]
    path, l = path.match(/(.+):(\d+)/)[1..2]
    path = File.expand_path path
    clazz = path[/.+\/(.+)\.rb/, 1]
    clazz = self.camel_case(clazz) if clazz
    {:path=>path, :line=>l, :method=>method, :clazz=>clazz}
  end

  def self.file_path
    "/tmp/output.notes"
  end

  def self.camel_case s
    s.gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")
  end

  # Logs short succinct stack trace
  def self.stack n=3
    ls ||= caller(0)[1..(n+1)]

    self.line "stack...", ls.shift, ""

    ls.each do |l|
      self.line nil, l, "  "
    end

  end
end
