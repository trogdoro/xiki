class RestTree

  def self.launch options={}
    line = options[:path].join('')

    txt = Net::HTTP.get(URI.parse(line[/!(.+)/, 1]))
    FileTree.insert_under txt#, :escape=>'!'
  end

  def self.handles? list
    list.first =~ /^ *!+http:\/\//
  end

end
