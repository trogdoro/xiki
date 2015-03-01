file = options[:dir] # || Shell.dir

command = options[:shell_command]

command = "ls -p" if command == "ls"

# Use last arg of command as dir, if it's a dir...

if last_arg = command[/.+ (\w.*)/, 1]
  last_arg = File.expand_path last_arg
  file = "#{last_arg}/" if File.directory? last_arg
  command.sub!(/(.+) .*/, "\\1")
end

# Make whole path...

relative = args.map{|o|
  txt = Path.split(o).map{|q| q.sub(/^: /, '')}.join('/')

  # If ls -l (has two spaces somewhere), remove up to last space (temporary hack)
  txt.sub!(/.+  .+ /, "")

  txt
}.join('/')
file << relative

# file, so open it...

task = options[:task]

if File.file? file

  # Task, so show items...

  return "~ edit/\n~ rename\n~ delete\n~ all\n~ outline\n~ search" if task == []
  if task == ["edit"]
    options[:nest] = 1
    return "+ emacs/\n+ vim/\n+ sublime/"
  elsif task == ["edit", "vim"]
    $el.suspend_emacs "clear\nvim '#{file}'"
    return ""
  elsif task == ["edit", "sublime"]
    Shell.sync "subl '#{file}'"
    return "<! opened in Sublime"
  end

  # Pass in - why :client not being passed?
  return X file, :client=>"editor"
end

# dir, so do another ls...

# task, so render...

return "~ cd and exit\n~ cd\n~ delete\n~ rename\n~ shell command" if task == []

if task == ["cd and exit"]
  file = Files.tilda_for_home file, :escape=>1
  $el.suspend_emacs "clear\ncd #{file}"
  return ""
end

return "- Not a file or a dir: #{file}" unless File.directory? file

txt = Shell.sync command, :dir=>file

return "- empty dir!" if txt == ""   # If empty dir

txt.gsub! /^/, ': '
