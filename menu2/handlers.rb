dir = Bookmarks["$x/lib/xiki/"]

if args.blank?
  Dir["#{dir}*_handler.rb"].map {|f| "- #{f[/(\w+)_handler/, 1]}/"}.join("\n")
else
  "@open file/#{dir}/#{args[0]}_handler.rb"
end
