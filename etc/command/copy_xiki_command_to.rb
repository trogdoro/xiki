is_windows = (RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/)

if is_windows
  require 'win32/pipe'
  include Win32
end

#
# Used to "install" the 'xiki' shell command (copy it to
# /usr/local/bin/) when installing Xiki manually from source
# rather than as a gem.
#
dest = ARGV[0]
xiki_dir = Dir.pwd


if is_windows
  puts "plase crate a xiki.bat file somewhere in your PATH with this content

  start ruby \"#{xiki_dir}/bin/xiki\"

thanks"
else
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


  puts ""   # Blank line

  txt = File.read source
  txt.sub! /\(xiki_dir\)/, xiki_dir

  File.open(dest, "w", 0755) { |f| f << txt }

  puts "Finished."
end

