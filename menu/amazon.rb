line = args[0]

# If no arg, prompt to type something

return "@prompt/Type something to search on amazon" if ! line

# If arg, look it up

line = CGI.escape line

Firefox.url "http://www.amazon.com/s?field-keywords=#{line}"
nil
