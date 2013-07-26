return "| alert 'hi'  # Type some coffeescript here (to run in the browser)" if args.empty?

return "@beg/quoted/" if args[0] !~ /\n/

txt = CoffeeScript.to_js args[0]
Browser.js txt

"@flash/- ran in browser!"
