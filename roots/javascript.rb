# /, so show message...

if args == []
  return "
    | // Type some javascript here (to evaluate)
    | return 1 + 2
    "
end

# /code, so eval it...

txt = args[0]

txt = "print = p = console.log;\n#{txt}"

Tree.quote JavascriptHandler.eval(txt)
