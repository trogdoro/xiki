command, root_output = options[:shell_command], options[:shell_root_output]

# "$ foo arg", so do nothing...

# For this simple example, do nothing unless the command
# has no args
return if command != "foo"

# "$ foo", so put colons before certain commands...

# (Decorate the output when it's the command being called
# with no items indented under it)

if root_output
  # Put colons before "bar" and "bah" lines, to indicated
  # that they can be expanded
  return root_output.gsub! /^\|(  +(bar|bah) )/, ":\\1"
end

# "$ foo/: bar", so do something specific to "bar"...

if args[0] =~ /\|  +bar /
  # In this case, we're just calling "$ foo bar" for them
  return Shell.command("foo bar").gsub(/^/, "| ")
end


