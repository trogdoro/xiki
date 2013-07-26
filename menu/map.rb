# If no arg, prompt to type something...

txt = args.join(" ")
return "@prompt/Type something to search on google maps" if ! txt.any?

# If arg, look it up...

txt.sub!(/ \(.+\)/, '')   # Remove paren groups, like (16th street)

txt = CGI.escape txt

Firefox.url "http://maps.google.com/maps?q=#{txt.gsub "\n", ", "}"
nil
