# /, so show the list...

files = DiffLog.file_list :tree_format=>1
return "- No files edited recently in xsh" if files == []

txt = files.join "\n"
