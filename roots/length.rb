return "=prompt/type a jquery selector here" if args.empty?

selector = args.join "/"

selector = "$(\"#{selector}\").length"
Firefox.exec selector
