dir = File.expand_path '~/xiki/bookmarks'

# /, so read in dir, but order by date...

if args == []

  names = Dir.entries(dir).select{|o| o !~ /^\./}.sort_by{ |x| File.stat("#{dir}/#{x}").mtime }.reverse

  names.map! do |o|
    name = o.sub(/\..+/, '').gsub('_', ' ')
    contents = File.read("#{dir}/#{o}").strip
    "- #{name}\n  : #{contents}\n"
  end

  names.push "\n| Optional\n"
  names.push *Bookmarks.bookmarks_optional.to_a.map{|o| "- #{o[0]}\n  : #{o[1]}\n"}
  names.push "\n| Required\n"
  names.push *Bookmarks.bookmarks_required.to_a.map{|o| "- #{o[0]}\n  : #{o[1]}\n"}
  return names.join

end

# ~, so show or run ~ navigate item...

return "~ navigate" if options[:dropdown] == []

if options[:dropdown] == ["navigate"]
  file = Bookmarks[":hx/bookmarks/#{args[0]}.notes"]

  if ! File.exists? file
    options[:no_slash] = 1
    return "
      | This bookmark is defined in bookmarks.rb
      =:xiki/lib/xiki/core/bookmarks.rb
        : @@bookmarks_required = {
        : @@bookmarks_optional = {
      "
  end

  View.open file
  return nil
end

# /foo, so jump to bookmark...

View.open ":#{args[0]}"

""
