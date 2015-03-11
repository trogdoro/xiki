# /, so show example...

if ! args[0]

  txt = File.read "/tmp/elisp_variables.notes" rescue nil

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
    File.open("/tmp/elisp_variables.notes", "w") { |f| f << txt }
  end

  return "| Expand to see the docs...\n#{Tree.quote txt}"
end

# /foo, so show source

var = args[0].sub(/^: /, "")
var = TextUtil.snake_case(var).to_sym

$el.with(:save_window_excursion) do
  Tree.pipe $el.describe_variable(var)+"\n\n"
end
