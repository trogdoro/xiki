txt = Search.launched *args

# If /, tell them they can pass a bookmark.

txt = "| Pass a bookmark to narrow down\n#{txt}" if args == []

txt
