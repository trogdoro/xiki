# No path, so show all labels from :t...

prefix = Keys.prefix

if args == []

  # Get latest "todo.notes" and "nav.notes" contents...

  todo = Notes.extract_paren_labels :file=>":t", :limit=>(prefix == :u ? 100 : 12)
  files = Notes.extract_paren_labels :file=>":n", :limit=>100, :indent=>"  "

  # Remove dups
  todo = todo.split("\n", -1).uniq.join("\n")
  files = files.split("\n", -1).uniq.join("\n")

  txt = "#{todo}- files/\n#{files}"

  options[:hotkey] = 1
  options[:omit_slashes] = 1

  return txt
end

# Item passed, so navigate or open...

task = options[:task]

# Root task, so show items...

return "~ navigate\n~ run" if task == []

# Open or run, so navigate to it...

View.kill if ! task && View.name == "yours/"

# files/..., so pull it off
launch_options = {}
if args.length == 2 and args[0] == "files"
  args.shift
  launch_options[:bookmark] = ":n"
end

launch_options[:label] = args[0]
launch_options[:navigate] = 1 if task == ["navigate"]

Launcher.do_last_launch launch_options
