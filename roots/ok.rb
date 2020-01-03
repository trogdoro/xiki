# At left margin, so close this view

if Line.indent == ""
  View.close
end

# Indented under something, so jump to parent and collapse

# Go to parent
Tree.to_parent

# Collapse
Tree.collapse


# "* task", so collapse
if Line.value =~ /^ *\*/

  indent = Line.indent

  # Delete option item
  Line.delete
  # "~ option item" at left margin, so leave cursor there
  if indent == ""
    View >> "\n"
  else
    Line.previous
  end
end


""
