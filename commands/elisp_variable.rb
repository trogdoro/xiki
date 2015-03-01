# /, so show example...
return "| Expand to see the docs...\n#{Tree.quote File.read(File.expand_path '~/xiki/notes/elisp/variables.notes')}" if ! args[0]

# /foo, so show source

var = args[0].sub(/^: /, "")
var = TextUtil.snake_case(var).to_sym

$el.with(:save_window_excursion) do
  Tree.pipe $el.describe_variable(var)+"\n\n"
end
