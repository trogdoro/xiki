%`
| Xiki's MENU_PATH environment variable.  Dirs that Xiki looks for menus in.
<< menu path/
#{
ENV['PATH'].split(":").map{|o| "@#{FileTree.add_slash_maybe o}\n"}.join("")
}
`
