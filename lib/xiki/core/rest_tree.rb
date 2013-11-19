module Xiki
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

    # Tell Launcher whether we're in a rest tree
    def self.handles? list
      list.index{|i| i =~ /^(GET|PUT|POST|DELETE)/}
    end

    # Pull out root
    def self.extract_root_verb path
      path.first.sub! /^ *(GET|PUT|POST|DELETE) /, "\\2"
      $1
    end

    # Pull out verb
    def self.extract_verb line
      line.sub! /^ *(GET|PUT|POST|DELETE) ?(.*)/, "\\2"
      $1
    end

    def self.remove_before_root list
      # Delete from beginning until root is found
      while list.first !~ /^ *(GET|PUT|POST|DELETE) /
        break unless list.any?
        list.shift
      end
      list
    end

    def self.request url, options={}
      verb = options[:verb] || "GET"

      begin
        net_http_class = Net::HTTP.const_get(verb.capitalize)
        url.gsub!('"', '%22')
        uri = URI.parse(url)

        req = net_http_class.new(uri.request_uri)
        req.body = options[:body]
        res = Net::HTTP.start(uri.host, uri.port) {|http|
          http.request(req)
        }
        options[:code] = res.code
        (res.code == '200' ? '' : "#{res.code} #{res.header["location"]}...\n") + res.body
      rescue Exception=>e
        e.message
      end
    end

    def self.xiki_url url
      url.sub! /^xiki:\/\//, 'http://'
      url.gsub! ' ', '+'
      url = "http://#{url}" if url !~ /\Ahttp:\/\//

      options = {}
      txt = self.request url, options

      # If 404, grab root
      if options[:code] == "404"
        root_url = url[%r`.+?//.+?/`]
        txt = self.request(root_url) if root_url
      end

      # If options[:code] == ""
      # TODO: This is assuming we're at the root
      #   rewrite so it starts at lowest path and climbs until it's not 301

      path = url[%r`.+?//.+?/(.+)`, 1] || ""

      if result = self.embedded_menu(txt, path)
        txt = result
      elsif txt =~ /\A.+\/$/
        # Don't quote
      else
        txt = Tree.quote txt
      end

      # If not found, back up to the root to check for a
      # comment-embedded menu

      # Only do if there's a sub-path
      #       if options[:code] == "404"
      # Ol["try again!"]
      #       end

      txt.sub!(/\A\| 301 (.+)\.\.\.$/){ "\n<@ #{$1.sub(/^http/, 'xiki')}" }

      txt
    end

    # Called by .xiki_url.  If found, grabs menus from
    # comments like the following and routes (calls
    # Tree.children).
    #
    # <!--
    #   - about/
    #     This is whatever
    #   - install/
    def self.embedded_menu txt, path

      # For now, require it to be in the whole file, or a comment...

      if txt !~ /\A[>|:+-] /
        comment = txt[/<!--\n *[>:|+-] .+?-->/m]
        return nil if ! comment   # Return existing if there's no tree
        txt = comment.sub(/ *<!--/, '').sub(/ *-->/, '')
        txt.unindent!
      end

      path.gsub! "+", ' '

      Tree.children txt, path

      # Returns nothing, so uses whole tree if no match found

    end

  end
end
