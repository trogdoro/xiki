inet = `ifconfig`.split("\n").grep(/\binet\b/)
return "| #{inet[0][/[\d.]+/]}" if inet.length < 2
inet[1][/[\d.]+/]

# "127.1.2.3"
