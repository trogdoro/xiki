require 'text_util'
require 'bookmarks'

class Ol

  @@last = Time.now - 1000

  def self.log txt, line
    path = Bookmarks["$o"]

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

  #   def self.<< txt
  #     line = caller(0)[1]
  #     self.log txt, line
  #   end

  def self.<< txt
    line = caller(0)[1]
    self.line txt, line
  end

  def self.line txt=nil, line=nil
    line ||= caller(0)[1]
    h = Ol.parse_line(line)

    txt = txt ? " #{txt}" : ''

    if h[:clazz]
      self.log "- #{h[:clazz]}.#{h[:method]} (#{h[:line]}):#{txt}", line
    else
      self.log "- #{line}:#{txt}", line
    end
  end

  def self.parse_line path
    path, line, method = path.match(/(.+):(.+):.+`(.+)'/)[1..3]
    clazz = path[/.+\/(.+)\.rb/, 1]
    clazz = TextUtil.camel_case(clazz) if clazz
    {:path=>path, :line=>line, :method=>method, :clazz=>clazz}
  end

  def self.launch
    # TODO: get total_lines - current_line
    distance_to_end = Line.number(View.bottom) - Line.number

    # Go to log.lines and get n from end
    arr = IO.readlines("#{Bookmarks['$o']}.lines")
    line = arr[- distance_to_end]

    path, line = line.split(':')

    #Ol << "path: #{path}"

    View.open path
    View.to_line line.to_i
  end

  def self.enter_log_line
    $el.open_line(1) unless Line.blank?
    View.insert("Ol.line")
  end

end
