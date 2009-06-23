class Specs

  class << self
    def method_missing(func, *args, &block)
      test, quote = args

      quote = test if args.size == 1 && test =~ /\n/   # If only 1 arg and it's multiline, it must be a quote

      if quote   # If there's a quote, we're jumping, not running
        quote = Line.value
        quote =~ /([\.\/].+):(\d+)/

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
        if test =~ /^#/
          Search.forward "^ *describe .+#{$el.regexp_quote(test)}"
        else
          Search.forward "^ *it .#{$el.regexp_quote(test)}"
        end
        Move.to_line_text_beginning
        return
      end

      txt = self.run_spec path, test
      txt.sub! /^F\n\n1\)\n/, ''   # Remove 1st few lines
      txt.sub! /^\.+\n\nFinished in.+\n\n/, ''   # Remove 1st when passing
      txt.sub! /^\(irb\):.+/m, ''   # Remove everything after (irb)

      txt.gsub! /^#{Bookmarks['$m']}/m, './'   # Shorten the paths

      # CodeTree.no_search_option +
      txt.gsub(/^/, '| ')

    end
  end

  def self.line_of_test path, test
    found = nil; number = 1
    IO.foreach(Bookmarks["$m/#{path}"]) do |line|
      if line =~ /^ *describe.+#{test}\b/
        break found = number
      end
      number += 1
    end
    found
  end

  def self.run_spec path, test
    params = [path]

    if test =~ /^#/   # If starts with #, find line number
      found = self.line_of_test path, test
      params << "-l" << (found ? "#{found}" : "0")
    elsif test.is_a?(String)
      params << "-e" << test
    end

    command = "Spec::Runner::CommandLine.run(Spec::Runner::OptionParser.parse(#{params.inspect}, $out_bufr, $out_bufr))"
    RubyConsole[:ml].run(command)
  end

  # Pastes in for the line in the clipboard:
  #   Spec.foo "bar"
  def self.enter_as_rspec
    if Line.blank?   # Assume clipboard contains "it..." line
      snippet = Clipboard['=']
      clazz = snippet[/(\w+)_spec\.rb/, 1]
      desc = snippet[/(".+")/, 1]
      View.insert "Specs.#{clazz} #{desc}"
      return
    end

    desc = Line[/  it (".+")/, 1]
    return unless desc

    orig = View.cursor
    Search.backward "^ *[+-] "
    clazz = Line[/(\w+)_spec\.rb/, 1]

    View.cursor = orig

    Line.delete :leave_linebreak
    View.insert "Specs.#{clazz} #{desc}"
    Move.to_axis

  end

  def self.run_spec_in_place
    Keys.clear_prefix
    cursor = View.cursor
    Move.to_end

    Search.backward '^ *it '   # Go up to "it ..." line
    path = View.file.sub /.+\/spec\//, 'spec/'
    test = Line.value[/(["'])(.+)\1/, 2]   # "
    test.gsub! "'", "\\\\'"   # Escape single quotes

    txt = self.run_spec path, test
    Search.forward '^  end$'
    Move.forward
    View.insert "#{txt.gsub(/^/, '  # ').gsub(/^  # $/, '  #')}"

    View.cursor = cursor
  end
end
