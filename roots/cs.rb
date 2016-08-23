return "
  | # Type some coffeescript here, to run in the browser
  | alert 'hi'

  > See
  << coffee/
  " if args.empty?

return "=beg/quoted/" if args[0] !~ /\n/

txt = CoffeeScript.to_js args[0]
Browser.js txt

"<! ran in browser!"
