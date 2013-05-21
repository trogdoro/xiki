module Xiki
  class TreeCursor
    @i = 0
    @txt = ""

    def initialize txt, i=0
      raise "1st parameter to TreeCursor.initialize must be a string" unless txt.is_a? String
      @i = i
      @txt = txt
    end

    def to_s
      "[#{@i}, #{@txt.inspect}]"
    end
    def inspect
      to_s
    end

    def i; @i; end
    def i= num; @i = num; end
    def txt; @txt; end

    def to_a
      @txt.strip.split("\n", -1)
    end

    def length
      @txt.strip.count("\n") + 1
    end

    def line
      to_a[@i]
    end

    def each &block
      @i = 0
      while @i < length
        #     while @i < @txt.split("\n").length
        block.call
        @i += 1
      end
    end

    def at_leaf?
      a = to_a
      following_line = a[@i+1]
      return true if following_line.nil?

      indent = Line.indent a[@i]
      following_indent = Line.indent a[@i+1]
      indent.length >= following_indent.length
    end

    def [] index
      to_a[index]
    end

    def select line
      line = line.sub /^( *)\+ /, "\\1- "
      index = to_a.index{|o| o.sub(/^( *)\+ /, "\\1- ") == line}
      @i = index if index
    end

    def under
      result = ""
      a = to_a
      target_indent = Line.indent(a[@i]).length
      i = @i + 1
      while i < length
        indent = Line.indent(a[i]).length
        break if indent <= target_indent
        result << "#{a[i]}\n"
        i += 1
      end

      result
    end

    def index_after
      to_a[0..@i].join("\n").length
    end

    def << lines
      @txt.insert index_after+1, lines
    end


  end
end
