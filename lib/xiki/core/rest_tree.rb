require 'net/http'

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

    def self.post url, options={}
      options[:verb] = 'POST'
      self.request url, options
    end
    def self.get url, options={}
      self.request url, options #.merge(:verb=>'GET')
    end

    # Examples:
    # RestTree.request "http://localhost:4717", :verb=>'POST', :body=>"heyy"
    #   You posted: heyy
    def self.request url, options={}
      verb = options[:verb] || "GET"

      begin
        net_http_class = Net::HTTP.const_get(verb.capitalize)
        url.gsub!('"', '%22')
        uri = URI.parse(url)

        req = net_http_class.new(uri.request_uri)

        body = options[:body]

        # :body is a hash, so convert to json!
        if body.is_a?(Hash)
          body = JSON.pretty_generate body
        end

        req.body = body

        # Set header vars
        (options[:headers]||{}).each do |key, val|
          req[key] = val
        end

        request_options = uri.port == 443 ? {:use_ssl=>true} : {}
        res = Net::HTTP.start(uri.host, uri.port, request_options) {|http|
          http.request(req)
        }

        options[:code] = res.code
        options[:response] = res

        body = res.body

        body

      rescue Exception=>e

        raise e if options[:raise]
        e.message
      end
    end

    def self.xiki_url url, options={}

      request_options = {:headers => {"User-Agent"=>"Xsh"}}

      items = options[:items]

      # "url/| Quote", so pull off last item as text to post

      post_txt = nil

      split = Path.split options[:path]

      if split[-1] =~ /\n/
        post_txt = split[-1]
        url = Path.join split[0..-2]
      end

      post_txt ||= options[:post]

      url.sub! /^xiki:\/\//, 'http://'
      url.gsub! ' ', '+'
      url = "http://#{url}" if url !~ /\Ahttp:\/\//

      if post_txt
        txt = RestTree.post url, request_options.merge(:body=>post_txt)
      else
        txt = self.get url, request_options
      end


      # If 404, grab root
      if options[:code] == "404"
        root_url = url[%r`.+?//.+?/`]
        txt = self.request(root_url, request_options) if root_url
      end

      # If options[:code] == ""
      # TODO: This is assuming we're at the root
      #   rewrite so it starts at lowest path and climbs until it's not 301

      path = url[%r`.+?//.+?/(.+)`, 1] || ""

      if txt =~ /\A\s*{/ || txt =~ /\A\s*<(\w|!--|!DOCTYPE)/ || txt =~ /\A.*\n\s*<(\w|!--|!DOCTYPE)/
        # Html, so quote
        txt = Tree.pipe txt
      else
        # Don't quote

        # Remove consecutive groups of blank lines
        txt.gsub! /\n\n\n+/, "\n\n"

      end

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
