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
      txt.gsub!(/ +$/, '')
      return "> Expand a process to kill it\n#{txt}\n#{output}"
    end

    # /process, so kill it...

    options = yield

    pid = process.split(/ +/)[2]
    command = "kill #{pid}"
    command = "kill -9 #{pid}" if options[:prefix] == :u
    output = `#{command}`

    "- sent kill to #{pid}!"
  end
end
