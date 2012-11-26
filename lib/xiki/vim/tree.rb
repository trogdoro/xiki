module XikiVim
  class Tree
    attr_accessor :value, :children

    def initialize txt, consume_root = false
      lines = txt.split("\n")

      if consume_root
        @value = lines.shift || ""
      end

      @children = if lines.empty?
                    []
                  else
                    parse_result lines
                  end
    end

    # returns a list of children given a txt, using indentation level
    def parse_result lines
      children   = []

      # 1. from blob to string array
      id_lvl     = lines.first[INDENT_RE, 1].length
      buffer     = ""
      lines.each do |line|
        id = line[INDENT_RE, 1].length

        # not direct children, buffer
        if id > id_lvl
          buffer += line + "\n"
          next
        end

        # back to direct children id level, done buffering
        # add buffered children to their parent (last children on stack)
        if not buffer.empty?
          children.last << "\n" << buffer
          buffer = ""
        end

        children << line
      end
      # retrieve being parsed children
      if not buffer.empty?
        children.last << "\n" << buffer
        buffer = ""
      end

      # 2. from string array to trees
      children.map! do |line|
        Tree.new line, true # create a new tree, consuming root
      end

      children
    end

    def render buffer, indent = nil, i = 0
      if indent.nil?
        indent = buffer.line.nil?? "" : buffer.line[INDENT_RE, 1]
      end

      if not @value.nil?
        buffer.append buffer.line_number + i, indent + @value
        i += 1
      end

      indent += INDENT
      @children.each do |c|
        i = c.render buffer, indent, i
        i += 1
      end

      return i - 1
    end
  end
end
