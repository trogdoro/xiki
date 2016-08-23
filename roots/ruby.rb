module Xiki
  # Wraps @eval, because "ruby" looks more appropriate
  # in some cases, like:
  #
  # foo/docs/counting/
  #   @ruby/
  #     | p "123"
  class Ruby
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

      if task = options[:task]
        return "* run in shell" if task == []

        # ~ run in shell, so run in console...

        # return args[0].inspect

        File.open("/tmp/tmp.rb", "w") { |f| f << args[0] }

        # Shell.async "ruby standalone.rb", :dir=>"/tmp"
        DiffLog.quit_and_run "ruby tmp.rb", :dir=>"/tmp"
        return ""
      end

      options[:no_slash] = 1

      # Get line number of first quoted line

      bounds = Tree.sibling_bounds(:must_match=>"\\|")
      line_number = Line.number bounds[0]

      cursor = View.cursor
      Tree.to_parent

      View.cursor = cursor

      code = args[0]

      code.gsub!(/^: /, '') if code =~ /\A: .+\z/   # If single :... line, unquote

      options_in = {:args=>args[1..-1], :items=>options[:items][1..-1]}


      txt, out, exception = Code.eval code, View.file, line_number, {:pretty_exception=>1}, options_in

      return exception if exception

      txt = txt.to_s rescue "couldn't parse result"

      txt = out || txt
      txt = Tree.pipe txt if txt.any?   # Don't quote if blank line
      txt
    end
  end
end

