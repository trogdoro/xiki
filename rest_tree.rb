class RestTree

  def self.launch options={}
    line = options[:path].join('')

    # Remove any crap before url
    line.sub! /.*GET (http:\/\/)/, "\\1"
    txt = Net::HTTP.get(URI.parse(line))

    # Add linebreak at end if none
    txt.gsub! "\cm", ''
    txt = "#{txt}\n" unless txt =~ /\n\z/
    #txt = JSON[txt].to_yaml
    FileTree.insert_under txt #, :escape=>''
  end

  def self.handles? list
    list.any? {|i| i =~ /^ *GET http:\/\//}
  end

end
