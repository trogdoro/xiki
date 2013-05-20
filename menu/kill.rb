class Kill
  # Called like this:
  # @kill/
  # @kill/2.6   625 craig          /Appli...
  # @kill/xiki/2.6   625 craig          /Appli...

  MENU = "
    docs/
      > Summary
      | Lists process, so you can click to kill them.
      > Filter
      | If you pass an item, it filters the which processes are displayed...
      @kill/user/
    << ports/
    "

  def self.menu_after output, *args

    return if args.any? && output   # If MENU sub-item that menu handled do nothing

    # Grab filter and process args
    filter = args.shift if args[0] !~ /^\|/   # If 1st arg isn't pipe-quoted, it's a filter
    process = args[0]

    # /, so list processes, then MENU contents...

    if ! process
      txt = `ps -eo pcpu,pid,user,args | sort -k 1 -r`
      header = txt.slice!(/^ *%.+\n/)
      txt = "#{header}#{txt}"
      txt.gsub! /^/, '| '

      if filter
        txt = txt.grep(/#{Regexp.quote filter}/).join('')
      end

      txt = "| none found" if txt == ""
      return "> Select a process to kill it\n#{txt.strip}\n#{output}"
    end

    # /process, so kill it...

    pid = process.split(/ +/)[2]
    output = `kill #{pid}`

    "- killed #{pid}!"
  end
end
