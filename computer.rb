class Computer

  def self.menu
    puts "- .ip/"
  end

  def self.ip
    puts `ifconfig`.grep(/\binet\b/)
  end
end
