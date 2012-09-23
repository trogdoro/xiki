#
# Used to "install" the 'xiki' shell command (copy it to
# /usr/local/bin/) when installing Xiki manually from source
# rather than as a gem.
#
dest = ARGV[0] || "/usr/local/bin/xiki"
puts "Putting the 'xiki' shell command at:

  #{dest}

You can pass a different path if you prefer...

  $ xiki /bin/xiki
"

source = "etc/command/xiki_wrapper"

xiki_dir = Dir.pwd

puts ""   # Blank line

txt = File.read source
txt.sub! /\(xiki_dir\)/, xiki_dir

File.open(dest, "w", 0755) { |f| f << txt }

puts "Finished."
