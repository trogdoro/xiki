# /, so give sample lisp...

return "
  | ; Provide some elisp to eval, like this
  | (set 'a 2)
  | (+ 1 a)
  " if args == []

# /code, so eval it...
result = $el.eval $el.read "(progn #{args[0]})"

return "<!" if ! result

Tree.quote result.to_s
