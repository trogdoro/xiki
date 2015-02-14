# /, so list all functions...
return "| Expand to see the docs...\n#{Tree.quote File.read('/Users/craig/xiki/notes/elisp/functions.notes')}" if ! args[0]

# /foo, so show source or dropdown...

func = args[0].sub(/^: /, "")
func = TextUtil.snake_case(func).to_sym

return "~ docs\n~ source" if dropdown == []

# ~ source, so jump to it

if dropdown == ["source"]
  result = $el.find_definition_noselect func, nil
  buffer, pos = result
  View.to_buffer buffer
  View.cursor = pos
  return
end

# /foo, so show source

$el.with(:save_window_excursion) do
  Tree.pipe $el.describe_function(func)+"\n\n"
end
