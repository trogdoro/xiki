# /, so show sample...

return "| $('h1').toggle(1000)  // Type some javascript here (to run in the browser)" if args == []

# /~, so handle options...

return "* show output" if task == []

txt = args[-1]
txt.sub! /\A: /, ''


if task == ["show output"] || txt =~ /^return /

  txt = "\n#{txt.strip}"
  txt.sub!(/.*\n/m, "\\0return ") if txt !~ /^ *return\b/
  txt << "\n"
  txt.sub!(/\A\n/, "")
  result = Browser.js txt

  return Tree.pipe result
end


n = Keys.prefix_n || 1
n.times do
  Browser.js txt
end

nil
