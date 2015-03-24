# /, so tell them to run stuff

if args == []
  return "
    | # Put some bash code here to run it
    | echo 'something'
    | echo 'something else'
    "
end

# tasks+, so show items

return "~ exit and run" if task == []

return DiffLog.quit_and_run args[0] if task == ["exit and run"]

# /code, so run it

txt = args[0]
File.open("/tmp/bash.sh", "w") { |f| f << txt }
txt = Shell.command "bash /tmp/bash.sh"#, :stdin=>"foo"
Tree.quote txt

