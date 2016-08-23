ENV['PATH'].
  split(":").
  map{|o| "=#{FileTree.add_slash_maybe o}"}.
  join("\n")
