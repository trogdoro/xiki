module Xiki
  # Wraps @eval, because "code" looks more appropriate
  # in some cases, like:
  #
  # foo/docs/counting/
  #   @code/
  #     | p "123"
  class Code
    def self.menu *args

      options = yield

      # /, so show default message...

      if args == []
        return "
          | # Put some code here and expand to run it
          | a = 1
          | a + a
          "
      end

      if dropdown = options[:dropdown]
        return "~ standalone" if dropdown == []

        # ~ standalone, so run in console...

        File.open("/tmp/standalone.rb", "w") { |f| f << args[0] }

        Shell.async "ruby standalone.rb", :dir=>"/tmp"
        return ""
      end

      options[:no_slash] = 1

      cursor = View.cursor
      Tree.to_parent
      line_number = View.line+1
      View.cursor = cursor

      code = args[-1]

      code.gsub!(/^: /, '') if code =~ /\A: .+\z/   # If single :... line, unquote

      txt, out, exception = Code.eval code, View.file, line_number, :pretty_exception=>1

      return exception if exception

      txt = txt.to_s rescue "couldn't parse result"

      txt = out || txt
      txt = Tree.quote txt if txt.any?   # Don't quote if blank line
      txt
    end
  end
end
