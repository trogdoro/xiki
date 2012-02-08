# Gets shelled out to by xiki to delegate call to a no-dependency .rb file.
# Just gets the args passed in and requires and invokes.

require "/projects/xiki/xiki_git/ol.rb"

file = ARGV.shift
path = ARGV.shift

load file

clazz = file[/\w+/].gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")

method = ".menu"
method = path.sub(/\/$/, '') if path =~ /^\./

output = eval "#{clazz}#{method}"
puts output

