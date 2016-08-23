# Doesn't add jquery - borrow from @dom

return "=prompt/type a jquery selector here" if args.empty?

selector = args.join("/").sub(/^\| /, '')
Firefox.blink selector
