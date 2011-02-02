class Computer

  def self.menu
    puts "+ .ip/"
  end

  def self.ip
    txt = `ifconfig`
    puts txt.grep(/\binet\b/)[1][/[\d.]+/]
  end
end
