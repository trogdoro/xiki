# Get file we're nested under

options[:no_slash] = 1

verb = options[:copy] ? "copied" : "renamed"

source_file = Tree.file

FileTree.extract_filters! source_file

return "- File doesn't exist!" if ! File.exists?(source_file)

is_dir = File.directory?(source_file)

dest_file = args.join("/")

# Renaming to :bookmark, so use bookmark...

if dest_file =~ /^:/
  dest_file = Bookmarks[dest_file]

  if File.directory?(dest_file)
    dest_file.sub! /\/$/, ''
    dest_file = "#{dest_file}/#{File.basename source_file}"
  end

  if options[:copy]
    FileUtils.copy_file(source_file, dest_file)
    return "| Copied #{source_file} to:\n= #{dest_file}"
  else
    File.rename(source_file, dest_file)
    return "| Renamed #{source_file} to:\n= #{dest_file}"
  end
end

# If dest dir is /... and is dir, put stem of source on end of dest

dest_file_full = dest_file =~ /^\// ?
  dest_file :
  "#{File.dirname source_file}/#{dest_file}"

# Rename the file...


  options[:copy] ?
    FileUtils.copy_file(source_file, dest_file_full) :
    File.rename(source_file, dest_file_full)


# Kill the line we're on, and go back one line

line = Line.value
column_from_right = line.length - View.column
column_from_right = [column_from_right, line[/\/(.+)/, 1].length].min   # Make max of movement be length of filename

Line.delete #if ! options[:copy]
Line.previous

# If absolute path, don't do it
return "<! #{options[:copy]}" if dest_file =~ /^\//


# Change the line to have the new name

dest_file << "/" if is_dir

options[:copy] ?
  Line.sub!(/(^ *([+-] )?)(.+)/, "\\1\\3\n\\1#{dest_file}") :
  Line.sub!(/(^ *([+-] )?).+/, "\\1#{dest_file}")

# Move line back to column

Move.to_end
Move.backward column_from_right

""

