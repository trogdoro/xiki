dest = ARGV[0]
puts "Putting the 'xiki' shell command at:
  #{dest}"

source = "etc/command/xiki_wrapper"

xiki_dir = Dir.pwd

puts ""   # Blank line

txt = File.read source
txt.sub! /\(xiki_dir\)/, xiki_dir

File.open(dest, "w", 0755) { |f| f << txt }
