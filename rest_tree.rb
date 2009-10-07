class RestTree

  def self.launch_inner path, children

    self.remove_before_root path

    # Extract verbs from root and current line
    root_verb = self.extract_root_verb path
    verb = self.extract_verb path.last

    body = nil
    # If line had verb
    if verb
      if ! path.last.empty?   # If line isn't blank, use as body
        body = path.last
        path.pop
      else
        # Remove quoted children
        children = children ? children.select{|e| e !~ /^\|/} : nil
        if children and children.any?   # If line blank and has children, use them as body
          body = children.map{|i| "#{i.sub(/^\s+/, '')}\n"}.join('')
        end
      end
    end

    verb ||= root_verb

    [verb || root_verb, path.join(''), body]
  end

  def self.launch options={}

    FileTree.plus_to_minus_maybe
    verb, url, body = self.launch_inner options[:path], CodeTree.children

    result = self.request verb, url, body
    result = "#{result}\n" unless result =~ /\n\z/
    result.gsub! "\cm", ''
    FileTree.insert_under result

  end

  # Tell LineLauncher whether we're in a rest tree
  def self.handles? list
    list.any? {|i| i =~ /^ *(GET|PUT|POST|DELETE) http:\/\//}
  end

  # Pull out root
  def self.extract_root_verb path
    path.first.sub! /^ *(GET|PUT|POST|DELETE) (http:\/\/)/, "\\2"
    $1
  end

  # Pull out verb
  def self.extract_verb line
    line.sub! /^ *(GET|PUT|POST|DELETE) ?(.*)/, "\\2"
    $1
  end

  def self.remove_before_root list
    # Delete from beginning until root is found
    while list.first !~ /^ *(GET|PUT|POST|DELETE) (http:\/\/)/
      break unless list.any?
      list.shift
    end
    list
  end

  def self.request verb, url, body=nil
    begin
      net_http_class = Net::HTTP.const_get(verb.capitalize)
      url.gsub!('"', '%22')
      uri = URI.parse(url)

      req = net_http_class.new(uri.request_uri)
      req.body = body
      res = Net::HTTP.start(uri.host, uri.port) {|http|
        http.request(req)
      }
      (res.code == '200' ? '' : "#{res.code} ") + res.body
    rescue Exception=>e
      e.message
    end

  end

  #   def handle_error(req, res)
  #     e = RuntimeError.new("#{res.code}:#{res.message}\nMETHOD:#{req.method}\nURI:#{req.path}\n#{res.body}")
  #     raise e
  #   end
end

