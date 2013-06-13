#
# Used to "install" the 'xiki' shell command (copy it to
# /usr/local/bin/) when installing Xiki manually from source
# rather than as a gem.
#
dest = ARGV[0]

if ! dest
  puts "Usage (run it from the xiki project dir):\n\n  $ etc/command/copy_xiki_command_to.rb /usr/bin/xiki"
  exit
end

if dest !~ /xiki$/
  puts "The path you pass should end with 'xiki'.  Such as:\n\n  $ etc/command/copy_xiki_command_to.rb /usr/bin/xiki"
  exit
end

puts "Putting the 'xiki' shell command at:

  #{dest}
"

source = "etc/command/xiki_wrapper"

xiki_dir = Dir.pwd

puts ""   # Blank line

txt = File.read source
txt.sub! /\(xiki_dir\)/, xiki_dir

File.open(dest, "w", 0755) { |f| f << txt }

puts "Finished."
