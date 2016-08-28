dir = File.expand_path '~/.xiki/interactions'

file = "#{dir}/#{(args[0]||"").gsub ' ', '_'}.xiki"

if task == []
  return "* save as command/"
elsif task == ["save as command"]
  options[:nest] = 1
  options[:no_task] = 1
  options[:line_found] = 2
  return "| Give your command a name\nfoo"
elsif task && task[0] == "save as command"
  name = task[1].gsub('_', ' ')
  txt = File.read file
  commands_dir = File.expand_path("~/.xiki/roots/")
  FileUtils.mkdir_p commands_dir
  File.open("#{commands_dir}/#{name}.xiki", "w") { |f| f << txt }
  return "<* - saved ~/.xiki/roots/#{name}.xiki"
end

# /, so read in dir, but order by date...

if args == []

  return ": task unimplemented yet...\n* new" if options[:task]

  files = Dir.entries(dir).select{|o| o !~ /^\./}.sort_by{ |x| File.stat("#{dir}/#{x}").mtime }.reverse
  return files.map{|o| "+ #{o.sub(/\.xiki$/, '').gsub('_', ' ')}\n"}.join
end

# /foo/:contents, so navigate to it...

View.open file

""
