Ol "args", args   # => ["comments"]

extract_options = {}

# links/methods/, so only extract methods
if args[0] =~ /^[a-z]+$/

  extract_options[:filter] = {
    "methods"=>/^[ :]*def /,
    "routes"=>/^[ :]*((get|post)|# Route) /,
    "comments"=>/^[ :]*(#|\/\/|;+) /,
    "outlog"=>/^[ :]*Ol[. ]/,
  }[args[0]]

  args.shift

end


# links/, so only extract all links

links = Notes.extract_links extract_options

# /, so show all links...

if args == []
  result, last_file = "", nil
  links.each do |o|
    if o[0] =~ /^>/
      next result << "#{o[0]}\n"
    end

    result << "+ #{o[0].sub(/.+\//, '')}\n" if o[0] != last_file
    result << "  #{o[1]}\n"
    last_file = o[0]
  end
  options[:filter_dont_collapse] = 1
  options[:filter_number_counts_only_quotes] = 1
  return result
end

# /file/:quote, so navigate to it...

target_file = args.slice! 0
target_quote = args[-1]
# target_file: "notes.rb"
# target_quote: ":     def self.foo"

return "* show in links\n* to top\n* lower\n* backwards\n* forwards\n* delete" if task == []

if task == ["show in links"]
  Links.show_in_nav target_file, target_quote
  return ""

elsif task && task.any?

  # Show it in the nav, and go up to heading

  Links.show_in_nav target_file, target_quote
  Notes.to_block :up=>1
  modified = View.modified?   # Remember if nav was modified

  if task == ["to top"]

    # Move heading to the top
    Notes.move_block_to_top :no_fade=>1

  elsif task == ["lower"]

    # Move heading down > to a few lower
    Notes.move_block :prefix=>4

  elsif task == ["backwards"] || task == ["forwards"]

    backwards = task == ["backwards"]

   # Move up to heading
    # Move it up or down
    backwards ? Notes.move_block(:up=>1) : Notes.move_block
    # Notes.move_block backwards ? :backwards : nil

  elsif task == ["delete"]
   # Move up to heading

    Notes.cut_block :no_clipboard

  end

  # Save changes and redisplay (unless it was modified)...

  if ! modified
    DiffLog.save :no_diffs=>1
    Launcher.open("links/")
  end

  return ""


end


# Iterate through args until we find quote and path file stem...
links.each do |file, quote|
  next if file !~ /#{Regexp.quote target_file}$/   # => nil
  next if target_quote && quote.strip.sub(/^- /, '') != target_quote.strip

  # - foo, so jump to it in ^links...

  # if target_quote =~ /^\w.*:$/
  if target_quote && target_quote !~ /^:/
    View.open "%links"
    View.to_top
    Search.forward $el.regexp_quote(target_quote)
    Line.to_words
    #View.to_quote target_quote.sub(/^: /, '')
    break
  end

  View.open file

  if target_quote =~ /^:/   # : Foo, so jump to it in file
    View.to_snippet target_quote.sub(/^: /, '')
  end
  break
end

""
