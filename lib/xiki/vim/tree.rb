module Xiki
  class Tree
    def self.<< txt
      line_number, line = Line.number, Line.value
      indent = line[/^ +/]
      txt.split("\n").each_with_index do |line, i|
        $curbuf.append(line_number + i, "#{indent}  #{line}")
      end
    end
  end
end
