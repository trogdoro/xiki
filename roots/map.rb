# If no arg, prompt to type something...

txt = args.join(" ")
return "=prompt/Type something to search on google maps" if ! txt.any?

# If arg, look it up...

txt.sub!(/ \(.+\)/, '')   # Remove paren groups, like (16th street)

txt = CGI.escape txt

url = "http://maps.google.com/maps?q=#{txt.gsub "\n", ", "}"

# Task to just show the url

if task = options[:task]
  return "~ url" if task == []
  options[:no_slash] = 1
  return "=#{url}"
end


# /open (as+open), so just show url (google fucks it up in the browser)...
return "=#{url}" if options[:prefix] == "open"

Firefox.url url
nil
