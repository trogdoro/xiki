class Kill
  # Called like this:
  # @kill/
  # @kill/2.6   625 craig          /Appli...
  # @kill/xiki/2.6   625 craig          /Appli...

  MENU = %`
    docs/
      | This menu lists process, so you can expand to kill them.
      - filter/
        | If you pass an item it filters which processes are displayed. Example:
        @kill/user/
      - keys/
        | The up key prefix will do a "kill -9", which is stronger
        | than just a regular "kill".
    << ports/
    `

  def self.menu_after output, *args
Ol()

    return if args.any? && output   # If MENU sub-item that menu handled do nothing

    # Grab filter and process args
    filter = args.shift if args[0] !~ /^:/   # If 1st arg isn't pipe-quoted, it's a filter
    process = args[0]

    # /, so list processes, then MENU contents...

    if ! process
      txt = `ps -eo pcpu,pid,user,args | sort -k 1 -r`
      header = txt.slice!(/^ *%.+\n/)
      txt = "#{header}#{txt}"
      txt.gsub! /^/, ': '

      if filter
        # txt = txt.split("\n").grep(/#{Regexp.quote filter}/i).join("\n")
        txt = txt.split("\n").grep(/#{filter}/i).join("\n")
      end

      return "| none found\n#{output}" if txt == ""

      txt.gsub!(/ +$/, '')
      return "> Expand a process to kill it\n#{txt}\n#{output}"
    end

    # /process, so kill it...

    options = yield

    # right click, so show options...

    return "~ kill\n~ sudo kill\n~ force kill" if options[:task] == []

    # Should just continue on
    # return "<! killed" if options[:task] == ["kill"]

    pid = process.split(/ +/)[2]
    command = "kill #{pid}"
    command = "kill -9 #{pid}" if options[:prefix] == :u || options[:task] == ["force kill"]

    # Dash+, so do sudo...

    # if options[:task] == ["sudo kill"]
    #   Console
    #   return "<! fu"
    # end

    if options[:task] == ["sudo kill"] || options[:prefix] == :-
      command = "sudo #{command}"
      Shell.async command
      return
    end

    # if options[:task] == ["force kill"]
    #   command = "kill -9 #{command}"
    #   Shell.async command
    #   return
    # end

    output = `#{command}`
    View.flash "- sent kill!", :times=>1
    column = View.column
    Line.delete
    View.column = column

    nil
  end
end
