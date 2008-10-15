class RestTree

  def self.launch options={}

    FileTree.plus_to_minus_maybe

    path = options[:path]

    # If current line starts with PUT
    if path.last =~ /^(POST|PUT|DELETE)/
      verb = $1

      url = path[0..-2].join('')
      url.sub! /.*GET (http:\/\/)/, "\\1"
      json = path[-1].sub /^#{verb} ?/, ''
      json = nil if json.blank?
      url = URI.parse(url)
      Net::HTTP.start(url.host, url.port) do |http|
        if verb == 'POST'
          http.post(url.path, json) { |txt| FileTree.insert_under txt }
        elsif verb == 'PUT'
          http.put(url.path, json) { |txt| FileTree.insert_under "xx#{txt}" }
        elsif verb == 'DELETE'
          http.delete(url.path) { |txt| FileTree.insert_under txt }
        end
      end

      return
    end

    url = path.join('')

    # Remove any crap before url
    url.sub! /.*GET (http:\/\/)/, "\\1"
    txt = Net::HTTP.get(URI.parse(url))

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
