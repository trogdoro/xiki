# No path, so show all labels from :t...

prefix = Keys.prefix

if args == []

  # Get latest "notes.notes" and "links.notes" contents...

  todo = Notes.extract_markers :file=>"^n", :limit=>100

  # Remove dups
  todo = todo.split("\n", -1).uniq.join("\n")

  txt = todo

  options[:hotkey] = 1
  options[:omit_slashes] = 1

  return txt
end

# Item passed, so navigate or open...

task = options[:task]

# Root task, so show items...

return "* navigate\n* run" if task == []

# Open or run, so navigate to it...

View.kill if ! task && View.name == "run labels/"

# files/..., so pull it off
launch_options = {}
if args.length == 2 and args[0] == "files"
  args.shift
  launch_options[:bookmark] = "^links"
end

launch_options[:label] = args[0]
launch_options[:go] = 1 if task == ["navigate"]

Launcher.do_last_launch launch_options
