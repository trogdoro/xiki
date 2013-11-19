module Xiki
  class Browser
    #     def self.menu
    #       "
    #       - .url/
    #       - .reload/
    #       - .tabs/
    #       - see/
    #         <@ web development/
    #         <@ firefox/
    #       - api/
    #         | This class lets you choose a default browser.
    #         |
    #         | Calls to Browser.url etc. will be delegated to the default browser.
    #       - docs/
    #         > Keys
    #         | do+load+browser:  Reload the browser.
    #         |
    #         > See
    #         | Many things have yet to be pulled out of firefox.rb and made generic.
    #         << firefox/
    #         |
    #       "
    #     end

    def self.url url, options={}
      Firefox.url url, options
    end

    def self.html html, options={}
      Firefox.html html, options
    end

    def self.append html
      Firefox.append html
    end

    def self.js txt, options={}
      Firefox.exec txt, options
    end

    def self.open_in_browser

      path = Tree.path[0]

      if path =~ /^(\w+)\.bootstrap\//
        name = $1
        self.url "http://localhost:8161/#{name}"
        return
      end

      if path =~ %r"^source://"
        path.sub! /^source/, 'http'
        self.url path
        return
      end

      # If file path, bullet, quote, or ends in slash, use tree instead of current file...

      file_exists = path && File.exists?(path)

      if file_exists || Line =~ /(^ *[|+-]|\/$)/
        # Treat as menu if not file path or "up" prefix

        is_menu = ! file_exists || Keys.prefix_u

        # If file path and is menu ("up" prefix), make sure double-slash at end
        if file_exists &&  is_menu
          path.sub! /\.\w+$/, ''   # remove extension
          path = "@#{path}"
          path.sub! /\/?$/, "//"
        end

        # Open as http://xiki/...

        if is_menu
          path.gsub! ' ', '-'
          url = "http://localhost:8161/#{path}"
        else
          url = "file://#{path}"
        end

        # Maybe make Dash+ show as web view of nested dirs with contents
        # Commented out for now, until we can figure out how to deal with
        # fact that it'll peg the cpu if there are a lot of dirs
        #         # If it's a dir, use @dtail
        #         url = "http://xiki/dtail/#{path}" if File.directory? path

        return self.url url
      end

      # Otherwise, just open current file

      # Put this somewhere wher it works in file tree as well
      return Browser.html(self.markdown_render View.txt) if View.extension == "md"   # If .markdown, render it
      file = View.file

      # Optionally turn into local url, accounding url_mappings.menu...

      mappings = Menu.menu_to_hash Bookmarks["~/menu/url_mappings.menu"] rescue {}
      result = nil
      mappings.each do |k, v|
        break file.sub!(v, "#{k}/") if file.start_with? v
      end

      # If path starts with any of the mappings, apply mapping

      if file =~ /^\//   # If no ^/, must be a url, so use http://
        file = "file://#{file}"
      else
        file = "http://#{file}"
        file.sub! /\/index.html/, '/'
      end

      self.url file
    end

    def self.markdown_render txt
      require "#{Xiki.dir}menu/markdown.rb" if ! defined? Markdown
      Markdown.render txt
    end

    def self.tabs *urls
      Firefox.tabs *urls
    end

    def self.reload
      Firefox.reload
    end

    def self.source *url
      return "@prompt/Pass me a url" if url == []
      url = url.join '/'

      `curl -A "Mozilla/5.0" #{url}`
    end

  end
end
