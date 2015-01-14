# No path, so show all labels from :t...

prefix = Keys.prefix

if args == []

  # Get latest "tasks.notes" and "files.notes" contents...

  tasks = Notes.extract_paren_labels :file=>":t", :limit=>(prefix == :u ? 100 : 12)
  files = Notes.extract_paren_labels :file=>":f", :limit=>100, :indent=>"  "

  # Remove dups
  tasks = tasks.split("\n", -1).uniq.join("\n")
  files = files.split("\n", -1).uniq.join("\n")

  txt = "#{tasks}- files/\n#{files}"

  options[:letter] = 1
  options[:omit_slashes] = 1

  return txt
end

# Item passed, so navigate or open...

dropdown = options[:dropdown]

# Root dropdown, so show items...

return "~ navigate\n~ run" if dropdown == []

# Open or run, so navigate to it...

View.kill if ! dropdown && View.name == "yours/"

# files/..., so pull it off
launch_options = {}
if args.length == 2 and args[0] == "files"
  args.shift
  launch_options[:bookmark] = ":f"
end

launch_options[:label] = args[0]
launch_options[:navigate] = 1 if dropdown == ["navigate"]

Launcher.do_last_launch launch_options
