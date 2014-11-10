module Xiki
  class Snippet
    def self.insert
      line = Line.value

      prefix = Keys.prefix

      # If line is blank, treat as enter+snippet...

      if FileTree.handles?
        Search.enter_search
        return
      end

      if line.blank?
        View << "@" if Line =~ /^ /
        Launcher.insert("snippet/")   # Until we port to unified
        return
      end

      # If line has something on it, treat as enter+source...

      # If url, try to grab dir

      if line =~ /^(xiki|http):\/\//
        server_name = line.sub(/^.+?\/\//, '')
        server_name.sub!(/\/$/, '')
        mappings = Menu.menu_to_hash Bookmarks["~/xiki/commands/url_mappings.menu"] rescue {}
        file = mappings[server_name]
        file = file ? "@#{file}" : "> Not found, add here\n@url mappings/"

        Tree.<< file, :no_slash=>1, :no_search=>1
        Launcher.launch
        return
      end

      if line =~ /^xiki:\/\//
        Tree.<< "hi", :no_slash=>1

        return
      end


      # This leaves it up to the menu.
        # Or do we want to take over > and delegate to =source?
          # so it shows the source file tree underneath?

      if ! line.blank?
        Keys.prefix = "source"
        Launcher.launch
        return
      end

    end
  end
end
