# /, so require "$ git" to be there (must be the raw "shell git" menu)...

command = options[:shell_command]
dir = options[:dir] # || Shell.dir

if args.blank?
  options[:no_slash] = 1
  return Tree.quote Shell.sync(command, :dir=>dir)
end

# /$ git/:    log (1st arg only), so run the command...

if args.length == 1 && args[0] =~ /^:    (\w+)/
  command = $1
  command << " --oneline" if command == "log"
  txt = Shell.sync "git #{command}", :dir=>dir

  if command == "diff"
    return Git.format_as_quote txt
  end

  return Tree.quote txt
end

# 75f8540..., so show files in commit...

if args[-1] =~ /^: ([a-f0-9]{7})/
  hash = $1
  return Tree.quote Shell.sync "git show --name-status #{hash}", :dir=>dir
end

# M foo.txt, so show diff for one file...

if args[-1] =~ /^: [MAD]\t(.+)/
  file = $1
  # Pull out hash from higher up
  hash = args.join('/')[/: ([a-f0-9]{7})/, 1]
  if hash
    txt = Shell.sync "git show --oneline -U1 #{hash} -- '#{file}'", :dir=>dir

    Git.format_as_quote txt, :one_file=>1

    return txt#.sub(/.+?^@/m, '@')
  end
end

# /, so return main items...

menu = "
  * hi/
  * init/
  * status/
  * diff/
  * log/
    - programatically add these!
  * more/
    - checkout/
    - bisect/
  "

if args == [] && options[:dropdown].blank?
  return menu if options[:mouse]   # Mouse, so return them all...
  return Tree.children menu, options[:dropdown]   # Keyboard, so just root items...
end

# $ git/something, so if /^   foo/ run it as command...

return "- was normal output, so run it" if options[:path] =~ /^   \w/

# /item/lastitem, so git > $ git lastitem...

"- Unhandled!"
