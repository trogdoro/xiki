Applescript.exec("Firefox", "activate") if Keys.prefix_u

return "| $('h1').toggle(1000)  // Type some javascript here (to run in the browser)" if args.blank?

return "@beg/quoted/" if args[-1] !~ /\n/

Firefox.js args[-1]
nil
