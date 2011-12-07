require "/projects/xiki/xiki_git/ol.rb"

target = ARGV.shift
path = ARGV.shift

load target

clazz = target[/\w+/].gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")

output = eval "#{clazz}.menu"
puts output

