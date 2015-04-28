module Xiki
  class Browser

    @@jquery_url = "http://code.jquery.com/jquery.min.js"
    @@browser = {}

    def self.browser

      begin
        require "selenium-webdriver"
      rescue LoadError
        return nil   # Callers shauld do something else if nil
      end

      kind = (Conf.get("browser", "browser") || "firefox").to_sym
      begin
        @@browser[kind] ||= Selenium::WebDriver.for kind
        # Do something trivial, so it'll error if not connected
        @@browser[kind].execute_script("return 1")
      rescue Selenium::WebDriver::Error::WebDriverError=>e
        # The old one was bad, so create entirely new connection
        @@browser[kind] = Selenium::WebDriver.for kind
      end

      @@browser[kind]

    end

    def self.url url, options={}

      browser = self.browser

      return Shell.command "open #{url}" if ! browser

      # Firefox.url url, options
      self.browser.navigate.to url

      "<! opened in browser"
    end

    def self.html html, options={}

      name = options[:name] || "tmp"
      File.open("/tmp/#{name}.html", "w") { |f| f << html }

      self.url "file:///tmp/#{name}.html" #, :reload=>1

      nil
    end

    def self.append html
      raise "is this used somewhere?"
      Firefox.append html
    end

    def self.js txt, options={}

      browser = self.browser

      raise "
        > A gem is required
        | The selenium-webdriver gem must be installed to send js
        | to the browser. To jump back to your shell and install
        | it as a xiki-local gem, type ^G on this line:
        |
        $ xikigem install selenium-webdriver
        " if ! browser

      begin
        browser.execute_script txt
      rescue Exception=>e

        # "$ is not defined" error, so load jquery and try again...

        if e.message =~ /\$ is not defined/
          browser.execute_script "
            var s=document.createElement('script');
            s.setAttribute('src', '#{@@jquery_url}'); s.setAttribute('id', 'jqid');
            document.getElementsByTagName('head')[0].appendChild(s);

            var s=document.createElement('script');
            s.setAttribute('src', 'http://xiki.org/javascripts/util.js');
            document.getElementsByTagName('head')[0].appendChild(s);
            ".unindent

          wait = Selenium::WebDriver::Wait.new(:timeout => 10) # seconds
          wait.until { browser.execute_script("return typeof( $ )") != "undefined" }

          browser.execute_script txt
        end
      end

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

      mappings = Menu.menu_to_hash Bookmarks["~/xiki/commands/url_mappings.menu"] rescue {}
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
      require "#{Xiki.dir}commands/markdown.rb" if ! defined? Markdown
      Markdown.render txt
    end

    def self.tabs *urls
      Firefox.tabs *urls
    end

    def self.reload
      raise "is this used somewhere?"
      Firefox.reload
    end

    def self.source *url
      return "=prompt/Pass me a url" if url == []
      url = url.join '/'

      `curl -A "Mozilla/5.0" #{url}`
    end

  end
end
