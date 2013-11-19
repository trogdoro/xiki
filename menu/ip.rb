# return "127.3.3.7"   # Hard-coded IP, for when demoing

inet = `ifconfig`.split("\n").grep(/\binet\b/)

return "| #{inet[0][/[\d.]+/]}" if inet.length < 2
inet[1][/[\d.]+/]
