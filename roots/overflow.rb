txt = args.join("/").strip

# If no arg, prompt to type something

return "=prompt/Type something to search on stack overflow" if txt.blank?


# Words not quoted, so grab siblings (if it's not on the same line)...

if txt !~ /\n/ && txt !~ /^:/ && Line !~ /google\//
  txt = Tree.siblings.join(" ")
end

txt.sub! /^: /, ''
txt.gsub! "\n", ' '



# If arg, look it up

txt = CGI.escape txt

Browser.url "http://stackoverflow.com/search?q=#{txt}"
nil
