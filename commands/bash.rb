# /, so tell them to run stuff

if args == []
  return "
    | # Put some bash code here to run it
    | echo 'something'
    | echo 'something else'
    "
end

# tasks+, so show items

return "
  ~ run as script
  ~ run in bash
  " if task == []

return DiffLog.quit_and_run args[0] if task == ["run in bash"]

if task == ["run as script"]
  # Save to script and run
  File.open("/tmp/tmp.bash", "w") { |f| f << args[0] }
  DiffLog.quit_and_run "bash /tmp/tmp.bash"
end

# /code, so run it

txt = args[0]
File.open("/tmp/bash.sh", "w") { |f| f << txt }
txt = Shell.command "bash /tmp/bash.sh"#, :stdin=>"foo"
Tree.quote txt

