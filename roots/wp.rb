# /, so show message...

return "<? - type wikipedia page name" if args.empty?

name, item = args[0].sub(/^: /, ""), args[1]

# /name and :task, so show right-click options...

if options[:task] # && ! item
  return "
    - source/
    " if ! item
end

# /name, so show in browser...

if ! item
  name.gsub!(/ /, '_')
  name = CGI.escape name

  Menu::Wikipedia.wp name
  return "<*"
end

# /name/source/, so show source...

require 'wikipedia'
page = Wikipedia.find(name)

Tree.pipe "> #{page.title}\n#{page.content}"
