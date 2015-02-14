# Get file we're nested under

options[:no_slash] = 1

source_file = Tree.file

FileTree.extract_filters! source_file

return "- File doesn't exist!" if ! File.exists?(source_file)

is_dir = File.directory?(source_file)

dest_file = args[0]

dest_file_full = "#{File.dirname source_file}/#{dest_file}"

# Renaming to :bookmark, so use bookmark...

if dest_file =~ /^:/
  dest_file = Bookmarks[dest_file]
  dest_file = File.directory?(dest_file) ?
    # Use bookmark as dir (if it's a dir) > ":bookmark/filepart of source"
    "#{dest_file}#{File.basename source_file}" :
    # Use bookmark as full dest (if it's a file) > ":bookmark"
    dest_file

  File.rename source_file, dest_file

  return "<! renamed to: #{dest_file}"
end

# Rename the file

File.rename source_file, "#{File.dirname source_file}/#{dest_file}"

# Kill the line we're on, and go back one line

line = Line.value
column_from_right = line.length - View.column
column_from_right = [column_from_right, line[/\/(.+)/, 1].length].min   # Make max of movement be length of filename

Line.delete
Line.previous

# Change the line to have the new name

dest_file << "/" if is_dir

Line.sub! /(^ *([+-] )?).+/, "\\1#{dest_file}"

# Move line back to column

Move.to_end
Move.backward column_from_right

""

