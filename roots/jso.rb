Applescript.exec("Firefox", "activate") if Keys.prefix_u

return "
  > Type a js expression here (to grab from the browser)
  | document.title
  " if args.blank?

txt = args[0]
return "=beg/quoted/" if txt !~ /\n/

txt = args[0]

Tree.quote Firefox.exec txt
