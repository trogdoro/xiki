module Xiki
  class Browser

    def self.url_via_os url

      os = Environment.os

      if os == "osx"
        return Shell.command "open \"#{url}\""
      end

      # The xdg-open opens url's on some linux distributions
      if Shell.command("type -P xdg-open") =~ /^\//
        return Shell.command "xdg-open \"#{url}\""
      end

      raise "Not sure how to open a browser on your OS. You don't have the 'open' or 'xdg-open' command installed."

    end

    def self.url url=nil, options={}

      return self.url_via_os(url) if options[:os_open]

      # No url specified, so return it...

      if ! url
        return SeleniumWrapper.js("return window.location.href")
      end

      # Url specified, so go there...

      # Todo > Make this configurable
      #   - maybe > conditionally use localhost jquery or remote one?
      #     - do this if > the browser won't include a localhost one into a page on the internet
      # Old > uses OS to navigate
      # return Shell.command("open #{url}") #if ! browser

      SeleniumWrapper.url url

      # Firefox.url url, options

      "<* opened in browser"
    end

    def self.html html, options={}

      name = options[:name] || "tmp"
      File.open("/tmp/#{name}.html", "w") { |f| f << html }

      if options[:via_os]
        self.url_via_os "file:///tmp/#{name}.html"
      else
        self.url "file:///tmp/#{name}.html"
      end

      nil
    end

    def self.append html
      raise "is this used somewhere?"
      Firefox.append html
    end

    def self.js txt, options={}
      SeleniumWrapper.js txt, options
    end

    def self.open_in_browser

      path = Tree.path[0]

      if path =~ /^(\w+)\.bootstrap\//
        name = $1
        self.url "http://localhost:8163/#{name}"
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
          url = "http://localhost:8163/#{path}"
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

      mappings = Command.menu_to_hash Bookmarks["~/.xiki/roots/url_mappings.menu"] rescue {}
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
      require "#{Xiki.dir}roots/markdown.rb" if ! defined? Markdown
      Markdown.render txt
    end

    def self.tabs *urls
      Firefox.tabs *urls
    end

    def self.reload
      SeleniumWrapper.js "window.location.reload()"
    end

    def self.source *url
      return "=prompt/Pass me a url" if url == []
      url = url.join '/'

      `curl -A "Mozilla/5.0" #{url}`
    end

  end
end
