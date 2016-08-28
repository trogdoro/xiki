# Used by > list+active

options[:no_slash] = 1

file, quotes = args.partition{|i| i !~ /^:/}
file = file.delete_if{|o| o =~ /^[#*]/}   # Remove any ##... or **... lines
target_file = Files.tilde_for_home file.join("/")

nav = Bookmarks["^links"]

# /file/:foo, so navigate or handle tasks...

if quotes.any?

  quote = quotes[-1]
  quote.sub! /../, ''   # Remove ": "

  # Handle tasks...

  return "* show in links\n* highlight and show" if task == []
  if task == ["show in links"]
    View.open nav
    View.to_snippet quote
    return ""
  end

  # No tasks, so navigate to file path...

  View.open target_file, :stay_in_bar=>1
  View.to_snippet quote

  if task == ["highlight and show"]
    Color.mark "red"
  end

  return ""
end

# /file, so show quotes from ^links for this file...

target_file_expanded = File.expand_path target_file

limit = 35
result, dir, filename, filename_indent = "", [], nil, nil
heading = nil

IO.foreach(nav) do |line|

  line.sub! "\n", ''

  indent = Tree.indent_size line

  # .../, so append as dir...

  if line =~ /^[^:|#]*\/$/
    dir = dir[0..indent]
    next dir[indent] = line
  end

  # Filename found, so add to path if it's ours...

  if like_filename = line[/^ *[+-] (.+\w)$/, 1]

    # Construct full path

    full = dir.compact
    full.map!{|o| Line.without_label :line=>o.strip}
    full = full[0..indent]
    full[indent] = like_filename
    full = full.join
    full = File.expand_path full

    # Not target file, so clear out filename and skip

    if full == target_file_expanded
      if heading
        result << "#{heading}\n"
        heading = nil
      end
      filename = full
      filename_indent = line[/^ */]
    else
      filename = nil
    end

    next

  end

  # Heading, so remember it...

  if line =~ /^>/
    heading = line
    heading = nil if heading =~ /^> ?:?$/
    next
  end

  next if ! filename

  # Blank line, so just add blanks (duplicate blanks will be removed later)...

  if line.blank?
    result << "\n"
    next
  end

  next if line !~ /^ /   # Ignore stuff at beginning of line that isn't a dir

  # Filename exists, so add this line...

  # Subtract off of indenting the amount the file was indented
  line.sub!(/^#{filename_indent}  /, '')

  result << "#{line}\n"

  limit -= 1
  break if limit < 1
end

result.gsub! /\n\n\n+/, "\n\n"   # Don't allow mustiple linebreaks
result.gsub! /\n(\n *[:+-])/, "\\1"

options[:filter_dont_collapse] = 1

result

