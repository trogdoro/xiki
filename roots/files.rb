file = Bookmarks["^links"]

# Grab first few files from :n, regardless of how we proceed...

limit = 100
list, dir = [], []
IO.foreach(file) do |line|

  indent = Tree.indent_size line

  # .../, so append as dir...

  if line =~ /^[^:|#]*\/$/
    dir = dir[0..indent]
    next dir[indent] = line
  end

  filename = line[/^ *[+-] (.+\w)$/, 1]
  next if ! filename

  # It's a filename, so add dir/file to output...

  dir.compact!

  dir.map!{|o| Line.without_label :line=>o.strip}
  dir = dir[0..indent]
  full = dir
  full[indent] = filename
  full = full.join
  full = Bookmarks[full]

  next if list.member? full   # Skip if we've already added this one

  list << full
  limit -= 1
  break if limit < 1
end

# /, so list most recent files from :n...

if args == []
  # Do hotkeys if launched by key shortcut (which we can tell because of the view name)
  options[:hotkey] = 1 if View.name == "files/"

  return list.map{|o| "+ #{o.sub(/.+\//, '')}"}.join "\n"
end

# /foo.txt, so find file path from list and go to it...

# Handle tasks

return "* show in links\n* to top" if task == []

if task == ["show in links"]
  View.open "^links"
  Search.forward "^[ +-]* #{Search.quote_elisp_regex args[0]}", :from_top=>1
  return ""

elsif task == ["to top"]

  # Jump to this file in the nav
  View.open "^links"
  Search.forward "^[ +-]* #{Search.quote_elisp_regex args[0]}", :from_top=>1

  # Jump up to the heading
  Notes.to_block :up=>1

  # Move heading to the top
  modified = View.modified?   # Remember if nav was modified
  Notes.move_block_to_top :no_fade=>1
  DiffLog.save :no_diffs=>1 if ! modified   # Save changes (unless it was modified)

  # Re-display the list (so it'll be at the top)
  Launcher.open("files/")

  return "" #"- todo > implement . ~ to top"

end

found = list.find{|o| o =~ /\/#{Regexp.escape args[0]}$/}

# /file, so find file from :n and open it...

View.open found

nil

