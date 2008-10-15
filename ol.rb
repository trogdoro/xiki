# Meant to log short succinct messages to help with troubleshooting
# while coding.  Log statements hyperlink back to the line that logged it.
class Ol
  @@last = Time.now - 1000

  def self.log txt, l=nil
    path = self.file_path

    # If just txt, delegate to line
    if l.nil?
      self
      return self.line(txt, caller(0)[1])
    end

    # If n seconds passed since last call

    difference = Time.now - @@last
    @@last = Time.now
    heading = difference > 3 ? "\n| \n" : nil

    h = Ol.parse_line(l)

    File.open(path, "a") { |f| f << "#{heading}#{txt}\n" }

    l = "#{h[:path]}:#{h[:line]}\n"
    File.open("#{path}.lines", "a") { |f|
      f << "\n\n" if heading
      txt.split("\n").size.times { f << l }
    }

    txt
  end

  def self.<< txt
    self.line txt, caller(0)[1]
  end

  def self.time
    now = Time.now
    self.line "#{now.strftime('%I:%M:%S')}:#{now.usec.to_s.rjust(6, '0')}", caller(0)[1]
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
