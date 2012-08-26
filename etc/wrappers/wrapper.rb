# Gets shelled out to by xiki to delegate call to a no-dependency .rb file.
# Just gets the args passed in and requires and invokes.

# TODO Don't hard-code path - use __FILE__?
# require "/projects/xiki/lib/xiki/ol.rb"

file = ARGV.shift
path = ARGV.shift

load file

clazz = file[/\w+/].gsub(/_([a-z]+)/) {"#{$1.capitalize}"}.sub(/(.)/) {$1.upcase}.gsub("_", "")
clazz = eval clazz

method = "menu"
method = path[/^\.(.+?)\/?$/, 1] if path =~ /^\./
method = method.to_sym

if clazz.respond_to? method
  puts clazz.send method
else
  cmethods = clazz.methods - Class.methods
  puts cmethods.sort.map{|o| "+ .#{o}/"}
end

