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

file_path = args[1] ? args[1].sub(/^: /, '') : nil

if options[:task] == []
  txt = "~ navigate\n~ delete bookmark"
  txt << "\n~ exit and cd" if file_path && File.directory?(file_path)
  return txt
end

if options[:task] == ["navigate"]
  file = Bookmarks[":xh/bookmarks/#{args[0]}.notes"]

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

elsif options[:task] == ["exit and cd"]
  dir = Bookmarks[file_path]
  Shell.exit_and_cd dir
  return nil

elsif options[:task] == ["delete bookmark"]
  file = Bookmarks[":xh/bookmarks/#{args[0]}.notes"]
  FileUtils.rm file
  return "<! deleted"

end

# /foo, so jump to bookmark...

View.open ":#{args[0]}"

""
