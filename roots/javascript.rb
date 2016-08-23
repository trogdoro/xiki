# /, so show message...

if args == []
  return "
    | // Type some javascript here (to evaluate)
    | 1 + 2
    "
end

# /code, so eval it...

txt = args[0]
txt.gsub!('"', '\"')
txt.gsub!("\n", '\n')

txt = "p = print; print(eval(\"#{txt}\"))"

txt = Tree.quote Shell.run "js -", :sync=>true, :stdin=>txt

txt.sub! /: undefined\n\z/, ''

txt
