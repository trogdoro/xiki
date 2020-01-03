# Handle tasks

return "* compile" if task == []
if task == ["compile"]
  return Tree.quote Shell.command "coffee -c --stdio", :stdin=>args[0]
end

# /, so show message...

return "
  | # Type some coffeescript here
  | console.log 'hi'
  " if args == []

# /code, so execute it...

# If coffeescript not installed, give them "gem install" command...

return "
  > First install coffeescript
  % gem install coffeescript
  " if `which coffee` == ""

Tree.quote CoffeeHandler.eval args[0]

