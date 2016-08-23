line = args[0]

# If no arg, prompt to type something

return "=prompt/Type something to search on stack overflow" if ! line

# If arg, look it up

line = CGI.escape line

Firefox.url "http://stackoverflow.com/search?q=#{line}"
nil
