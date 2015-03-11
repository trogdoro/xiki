ancestors = options[:ancestors]
menu_steps = ancestors && ancestors[-1] == "menu steps/"
ancestors.pop if menu_steps

# / and no ancestors, so display message...

if ! ancestors && args == []
  return %`
    : The 'echo' menu is just for experimenting with how
    : paths work. Put some stuff underneath and expand them
    : to see the path. You'll see the value that's passed
    : into menus as the "path" variable.
    - a/
      - b/
    :
    <= options/
    <= source/
    `
end

# =echo/foo, so use foo as the path...

path = args.any? ?
  args.inspect :
  options[:ancestors].inspect

# ancestors/=echo, so use ancestors...

"| #{path}"
