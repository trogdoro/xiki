# /, so show the list...

files = DiffLog.file_list :tree_format=>1
txt = files.join "\n"
