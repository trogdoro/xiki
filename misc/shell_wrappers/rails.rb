command = options[:shell_command]

return "<$ #{args.last}" if args.last =~ /^\$ /

dir = options[:dir]

if args[0] =~ /^:  generate  /
  return Shell.sync("rails generate", :dir=>dir).gsub(/^/, ': ') if ! args[1]
  if args[1] =~ /^:   model/
    return "+ examples/\n"+Shell.sync("rails generate model", :dir=>dir).gsub(/^/, ': ') if ! args[2]
    return "$ rails generate model foo name:string details:text quantity:integer price:decimal delivery:boolean purchased_at:datetime" if args[2] == "examples"
  end

end

["console", "server", "dbconsole"].each do |arg|
  if args[1] =~ /^:  #{arg}  /
    Shell.async("rails #{arg}", :dir=>dir)
    return ""
  end
end

"- This line isn't recognized"
