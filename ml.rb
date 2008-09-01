class Ml
  def self.<< txt
    File.open(Bookmarks["$o"], "a") { |f| f << "#{txt}\n" }
    txt
  end

  def self.line
    l = caller(0)[1]

    path, line, method = l.match(/(.+):(.+):.+`(.+)'/)[1..3]
    clazz = path[/.+\/(.+)\.rb/, 1]
    if clazz
      clazz = TextUtil.camel_case( path[/.+\/(.+)\.rb/, 1] )
      self << "- #{clazz}.#{method} (line): #{line}, #{path}"
    else
      self << "- (line): #{l}"
    end

  end

end
