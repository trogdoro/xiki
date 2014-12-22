# /, so show options

if args == []

  name = options[:save_as_name] || ""
  txt = "
    | #{name}
    - session
    - in current directory
    - more/
    "
  return txt
end

if args == ["more"]
  return "
    - in other directory
    - note
    - command
    - task?
    "
end

name = Tree.siblings[0].sub(/^\| /, '')

dir = {"session"=>"~/xiki/sessions/", "more/note"=>"~/xiki/notes/", "more/command"=>"~/xiki/commands/"}[args.join('/')]
dir = File.expand_path dir

FileUtils.mkdir_p dir   # Make sure it exists
file = "#{dir}/#{name}"

file = Files.unique_name file   # Don't conflict with any existing file

# Close this tile first > so it'll go back to the right place...

buffer_to_save = $el.elvar.buffer_to_save
View.kill   # Close the menu tile and jump back to the original buffer
View.to_buffer buffer_to_save

$el.write_file file

""
