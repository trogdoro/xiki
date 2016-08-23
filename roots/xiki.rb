# /, so return all topics...

if args == []
  files = Topic.list.map{|o| "#{o}\n"}

  ["notes", "links"].each do |move_down|
    found = files.delete("#{move_down}\n")
    files << found if found
  end

  return files.join('')
end


# /topic, so replace parent and expand...

return "
  * to top
  * just this
" if task == []

if task == ["just this"]

  Tree.to_parent
  Tree.collapse
  Line.sub! /.*/, args[0]
  return ""

elsif task == ["to top"]
  xiki_file = Topic.topic_to_filename args[0]

  FileUtils.touch xiki_file

  # Move it to the top
  line = Line.delete
  Tree.to_parent
  Move.down
  View >> "#{line}\n"

  return ""
end


# Opened with list+xiki or "$ xsh -", so open in new view
view = View.name
if view == "xiki/" || view == "xsh"

  # New behavior > open in new view

  Launcher.open arg1   #> |
  ""

else
  # Inserted in existing view, so replace parent

  "<<< #{args[0]}"

end

# Maybe todo > Only open in new view when > We are in the only view, and it's "xiki/" > Otherwise replace parent



# Old behavior > Replace parents

