# /, so list labels from current file

file = options[:dir].sub(/\/$/, '')

if args == []
  labels = Notes.extract_markers :file=>file, :limit=>100, :indent=>"  "
  labels = labels.split("\n", -1).uniq.join("\n")
  options[:omit_slashes] = 1
  return labels
end

# ~ option, so maybe navigate

return "* navigate\n* run" if task == []


# /label, so open the file and navigate to it

View.open file
View.to_top
Notes.jump_to_label args[0]
Line.next

return "" if task == ["navigate"]

Tree.collapse
Launcher.launch

""
