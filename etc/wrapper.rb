require "/projects/xiki/xiki_git/ol.rb"

target = ARGV.shift
path = ARGV.shift
Ol << "target: #{target.inspect}"
Ol << "path: #{path.inspect}"

load target


clazz = target[/\w+/].gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")
Ol << "clazz: #{clazz.inspect}"

output = eval "#{clazz}.menu"
Ol << "output: #{output.inspect}"
puts output

# puts "done"
