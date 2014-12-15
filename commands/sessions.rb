dir = File.expand_path '~/xiki/sessions'

# /, so read in dir, but order by date...

if args == []

  return ": dropdown unimplemented yet...\n~ new" if options[:dropdown]

  files = Dir.entries(dir).select{|o| o !~ /^\./}.sort_by{ |x| File.stat("#{dir}/#{x}").mtime }.reverse
  return files.map{|o| "+ #{o.sub(/\.notes$/, '').gsub('_', ' ')}\n"}.join
end

# /foo/:contents, so navigate to it...

file = "#{dir}/#{args[0].gsub ' ', '_'}.notes"
View.open file

""
