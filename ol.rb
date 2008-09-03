require 'text_util'

class Ol
  def self.log txt, line
    h = Ol.parse_line(line)

    path = Bookmarks["$o"]
    File.open(path, "a") { |f| f << "#{txt}\n" }
    File.open("#{path}.lines", "a") { |f| f << "#{h[:path]}:#{h[:line]}\n" }

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
end
