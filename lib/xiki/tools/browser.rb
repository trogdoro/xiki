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

    def self.html html
      Firefox.html html
    end

    def self.append html
      Firefox.append html
    end

    def self.js txt, options={}
      Firefox.exec txt, options
    end

    def self.open_in_browser

      path = Tree.path.join("\n")

      prefix = Keys.prefix
      line = Line.value
      use_tree_at_cursor = line =~ /(^ *[|+-]|\/$)/   # If ends in slash or bullet or quote
      use_tree_at_cursor = nil if prefix == :u   # If C-u, always use the surrounding file

      # If Dash+, open in browser with "//" on end (as menufied)
      if prefix == :-
        use_tree_at_cursor = true
        path.sub! /\.\w+$/, ''   # remove extension
        path = "@#{path}"
        path.sub! /\/?$/, "//"
      end

      if use_tree_at_cursor   # Open as http://xiki/...
        path.gsub! ' ', '-'

        url = "http://xiki/#{path}"

        # If it's a dir, use @dtail
        url = "http://xiki/dtail/#{path}" if File.directory? path

        # If it's a file, put "@" at beginning so sinatra doesn't fuck it up
        url = "http://xiki/@#{path}" if File.file? path

        return self.url url
      end

      if FileTree.handles?
        file = Tree.construct_path
      else
        # Put this somewhere wher it works in file tree as well
        return Browser.html(self.markdown_render View.txt) if View.extension == "markdown"   # If .markdown, render it
        file = View.file
      end


      mappings = Menu.menu_to_hash Bookmarks["~/menu3/url_mappings.menu"]


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

  end
end
