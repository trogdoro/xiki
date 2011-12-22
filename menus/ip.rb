class Ip
  def self.menu
    inet = `ifconfig`.grep(/\binet\b/)

    return "| #{inet[0][/[\d.]+/]}" if inet.length < 2
    "- #{inet[1][/[\d.]+/]}"
  end
end
