file = Bookmarks[":n"]

# Grab first few files from :n, regardless of how we proceed...

limit = 50
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

  next if list.member? full   # Skip if we've already added this one

  list << full
  limit -= 1
  break if limit < 1
end

# /, so list most recent files from :n...

if args == []
  options[:hotkey] = 1
  return list.map{|o| "+ #{o.sub(/.+\//, '')}"}.join "\n"
end

# /foo.txt, so find file path from list...

args[0]
found = list.find{|o| o =~ /\/#{Regexp.escape args[0]}$/}

# /file, so find file from :n and open it...

View.open found

nil

