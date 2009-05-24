class Specs

  class << self
    def method_missing(func, *args, &block)
      test, line = args

      if line   # If there's a line, jump to it
        line = Line.value
        line =~ /([\.\/].+):(\d+)/

        path, line = $1, $2
        path.sub! /^\.\//, Bookmarks['$m']   # Fix relative paths

        View.open path
        View.to_line line.to_i
        return
      end

      clazz = func.to_s
      dir = 'models'   # Assume model

      dir = 'controllers' if clazz =~ /s$/   # If it ends with s, assume controller
      dir = 'helpers' if clazz =~ /_helpers$/

      path = "spec/unit/#{dir}/#{clazz}_spec.rb"

      if Keys.prefix_u   # If U prefix, jump to test
        View.open "#{Bookmarks['$m']}#{path}"
        Keys.clear_prefix
        View.to_highest
        Search.forward " it .#{$el.regexp_quote(test)}"
        Move.to_line_text_beginning
        return
      end

      test.gsub! "'", "\\\\'"   # Escape single quotes

      command = "Spec::Runner::CommandLine.run(Spec::Runner::OptionParser.parse(['#{path}', '-e', '#{test}'], $out_bufr, $out_bufr))"
      txt = RubyConsole[:ml].run(command)

      txt.sub! /^F\n\n1\)\n/, ''   # Remove 1st few lines
      txt.sub! /^\.\n\nFinished in.+\n\n/, ''   # Remove 1st when passing
      txt.sub! /^\(irb\):.+/m, ''   # Remove everything after (irb)

      txt.gsub! /^#{Bookmarks['$m']}/m, './'   # Shorten the paths

      CodeTree.no_search_option + txt.gsub(/^/, '| ')

    end
  end

end
