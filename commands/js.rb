return "| $('h1').toggle(1000)  // Type some javascript here (to run in the browser)" if args.blank?

txt = args[-1]   # => alert(\"1\")"
txt.sub! /\A: /, ''

n = Keys.prefix_n || 1
n.times do
  Browser.js args[-1]
end

nil
