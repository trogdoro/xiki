# ~ or ~ manual, so handle

return "* manual" if task == []
if task == ["manual"]
  $el.info "elisp"
  return ""
end

# /, so give sample lisp...

return "
  | ; Provide some elisp to eval, like this
  | (set 'a 2)
  | (+ 1 a)
  " if args == []

# /code, so eval it...

# Try inspecting by default
result = $el.eval $el.read "(progn #{args[0]})"

return if ! result

result = $el.pp_to_string result

Tree.quote result.to_s
