Applescript.exec("Firefox", "activate") if Keys.prefix_u

return "
  | <h1>Type html here</h1>
  | <p>To append to the browser</p>
  " if args.blank?

return "=beg/quoted/" if args[0] !~ /\n/

Firefox.append args[0]
