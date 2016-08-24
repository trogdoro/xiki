module Xiki
  class CommandSuggester

    def self.expands? options

      # Ol "Say to create a command?!"
      return if options[:name].blank?   # If no name probably begins with a dot

      # Only interject if no launcher or just pre launcher
      return if options[:expanders].any? || options[:expanders] == [PrePattern]

      return if ! options[:name]   # If menu name, will either auto-complete or suggest creating

      # Add ourself as handler if it looks like a command.  When Command.expand
      # tries to run, we'll know whether it actually is a menu.
      (options[:expanders] ||= []).push(self)
    end

    def self.expand options

      return if options[:output] || options[:halt]

      name, task, items = options[:name], options[:task], options[:items]

      return if ! name   # Don't suggest if it's not menu-like

      # :foo, so suggest creating as notes file...

      if options[:path] =~ /\A^:/

        if ! items#.length == 1
          return options[:output] = "
            > Sample heading
              | These notes don't exist yet.
              | Edit this text and type Ctrl+T to create
              | (or type Ctrl+T and select 'clear').
              |
            "
        end


        # :foo/bar, so create...

        # ~, so show option

        return options[:output] = "* create\n* clear" if !task || task == []

        # if items.length > 1

        # ~ create, so create the file for the first time!

        if task == ["create"]

          heading, content = items

          name = options[:name]
          name.gsub!(/\A:/, '')

          file = File.expand_path "~/xiki/#{name}.xiki"
          content = Tree.unquote content
          File.open(file, "w") { |f| f << "#{heading}\n#{content}" }

          return options[:output] = "<* created!"
        end

        if task == ["clear"]
          options[:line_found], options[:column_found], options[:no_search] = 1, 2, true

          return options[:output] = "=replace/siblings/\n  | \n  |"
        end

        # end

      end


      # Not notes, so suggest creating a command...

      if extension = options[:extension]
        options[:no_slash] = 1

        if items = options[:items]
          txt = items[0]
          if txt =~ /\A: .+\z/
            return options[:output] = "- Use pipe quotes, not colons!"
          end
          if items.length != 1
            return options[:output] = "| Should either be no items or one quoted item"
          end
          file = "~/.xiki/roots/#{Command.format_name name}#{extension}"
          file = File.expand_path file
          File.open(file, "w") { |f| f << txt }
          return options[:output] = "<* - saved!"
        end

        require "#{Xiki.dir}roots/sample_menus/sample_menus_index.rb" # if !defined?(SampleMenus)

        txt = SampleMenus.by_extension(extension)
        txt ||= SampleMenus.by_extension(".txt") if extension != "."   # Assume text if there's an extension but had no match

        if ! txt
          return options[:output] = "| Doesn't exist yet...\n"+Command.handlers_with_samples.map{|o| "<< #{name}.#{o}\n"}.join("")
        end

        return options[:output] = Tree.quote(txt, :char=>"|")

      end

      # Try finding completions...

      if options[:path] !~ /\/$/   # Don't try to complete if line ends with a slash

        # If found any completions, return them...

        if(list = Command.completions(name)).any?
          if list.length == 1
            return options[:output] = "<< #{list[0]}/"
          end

          # if editor, add "..." at the end of the line
          if options[:client] =~ /^editor\b/
            Line << "..."
          end

          options[:no_slash] = 1   # If auto-completed, do no slash
          options[:output] = list.map{|o| "<< #{o}/\n"}.join("")
          return
        end
      end

      # No completions or existing menu, so suggest creating via samples (@sample_menus)...

      expand_options = {}
      if options[:client] == "web"

        options[:dont_html_format_items] = 1

        # If they posted the form to create the menu, do it...
        if create_kind = options[:create_kind]

          file = File.expand_path "~/.xiki/roots/#{name.gsub(/[ -]/, '_')}#{options[:create_extension]}"
          txt = options[:create_txt]
          txt.gsub! "\r\n", "\n"

          File.open(file, "w") { |f| f << txt }

          # If they posted, create, and continue on to view the menu...

          options[:output] = "<script>window.location = '/"+options[:path].gsub(' ', '-')+"';</script>"
          return
        end

        expand_options[:original_name] = TextUtil.camel_case options[:name]
        sample_menus = "web sample menus"
      else
        sample_menus = "sample menus"
      end

      # We might need to pass more options in here... - :client, for example?
      txt = Expander.expand sample_menus, options[:items], expand_options   # Will handle if no items or a sample menu item

      if txt
        if txt =~ /<name>/i
          txt = "<< ip/"
        end

        return options[:output] = txt
      end

      # User had items that weren't in @sample

      # Non-existant items were created, so suggest making a new menu out of them...
      if options[:client] =~ /^editor\b/
        options[:no_slash] = 1

        # Any item or ~ on a non-existant command, so show tasks...

        return options[:output] = ": Route '#{name}' not found." # if task == []

      end
    end

  end
end
