class Man
  def self.menu command=nil
    if command == nil
      return "
        | Type a command to show its man page:
        - example: ls
        - last used: @last/man/
        "
    end

    `man #{command} | col -x -b`.gsub(/^/, '| ').gsub(/^\| $/, '|')
  end
end
