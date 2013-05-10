class Kill
  def self.menu process=nil
    # If no process passed in, list all
    if ! process
      txt = `ps -eo pcpu,pid,user,args | sort -k 1 -r`
      header = txt.slice!(/^ *%.+\n/)
      txt = "#{header}#{txt}"
      txt.gsub! /^/, '| '
      return "> Select a process to kill it\n#{txt.strip}\n<< ports/"
    end

    pid = process.split(/ +/)[2]
    output = `kill #{pid}`

    # Kill one that was passed in
    "- killed #{pid}!"
  end
end
