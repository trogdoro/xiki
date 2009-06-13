class Man
  class << self
    def method_missing(func, *args, &block)
      `man #{func} | col -x -b`.gsub(/^/, '| ').gsub(/^\| $/, '|')
      #       `man #{func} | col -b`.gsub(/^/, '| ').gsub(/^\| $/, '|')
    end
  end
end
