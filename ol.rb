class Ol
  @@last = Time.now - 1000

  def self.log txt, line=nil
    path = self.file_path

    # If just txt, delegate to line
    if line.nil?
      self
      return self.line(txt, caller(0)[1])
    end

    # If n seconds passed since last call

    difference = Time.now - @@last
    @@last = Time.now
    heading = difference > 3 ? "\n| \n" : nil

    h = Ol.parse_line(line)

    File.open(path, "a") { |f| f << "#{heading}#{txt}\n" }

    line = "#{h[:path]}:#{h[:line]}\n"
    File.open("#{path}.lines", "a") { |f|
      f << "\n\n" if heading
      txt.split("\n").size.times { f << line }
    }

    txt
  end

  def self.<< txt
    self.line txt, caller(0)[1]
  end

  def self.line txt=nil, line=nil
    line ||= caller(0)[1]
    h = Ol.parse_line(line)

    txt = txt ? " #{txt}" : ''

    if h[:clazz]
      self.log "- #{h[:clazz]}.#{h[:method]} (#{h[:line]}):#{txt}", line
    else
      display = line.sub(/_html_haml'$/, '')
      display.sub!(/.+(.{18})/, "\\1...")
      self.log "- #{display}:#{txt}", line
    end
  end

  def self.parse_line path
    method = path[/`(.+)'/, 1]
    path, line = path.match(/(.+):(\d+)/)[1..2]
    path = File.expand_path path
    clazz = path[/.+\/(.+)\.rb/, 1]
    clazz = self.camel_case(clazz) if clazz
    {:path=>path, :line=>line, :method=>method, :clazz=>clazz}
  end

  def self.file_path
    "/tmp/log.notes"
  end

  def self.camel_case s
    s.gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")
  end

end
