shell_command = options[:shell_command]

# $ docker, so just decorate the output...

if shell_output = options[:shell_output]

  if shell_command == "docker help"
    shell_output.gsub! /^\|     (\w)/, ":     \\1"
    return
  end

  return shell_output.gsub! /^\|(  +(images|ps)  +)/, ":\\1"
end

item = args[0][/^\|     (\w+)/, 1]

# $ docker help, so invoke help...

if shell_command == "docker help"
  return Shell.command("docker help #{item}").gsub(/^/, "| ")
end

# $ docker/:    foo, so just run it

if shell_command == "docker" && args.length == 1
  return Shell.command("docker #{item}").gsub(/^/, ": ")
end

