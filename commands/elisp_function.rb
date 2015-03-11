# /, so list all functions...

if ! args[0]

  txt = File.read "/tmp/elisp_functions.notes" rescue nil

  if ! txt   # Cached file not found, so create it
    View.flash "caching in /tmp/...", :times=>1
    txt ||= $el.el4r_lisp_eval %`
    (let ((txt ""))
      (mapatoms (lambda (x)
        (when (fboundp x)
          (setq txt (concat txt (pp-to-string x) "\n"))
          )))
      txt)
    `
    txt = txt.split("\n").sort.join("\n")
    File.open("/tmp/elisp_functions.notes", "w") { |f| f << txt }
  end

  return "| Expand to see the docs...\n#{Tree.quote txt}"
end


# /foo, so show source or tasks...

func = args[0].sub(/^: /, "")
func = TextUtil.snake_case(func).to_sym

return "~ docs\n~ source" if task == []

# ~ source, so jump to it

if task == ["source"]
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
