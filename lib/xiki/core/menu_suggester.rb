module Xiki
  class MenuSuggester

    def self.expands? options

      return if options[:name].blank?   # If no name probably begins with a dot

      # Only interject if no launcher or just pre launcher
      return if options[:expanders].any? || options[:expanders] == [PrePattern]

      return if ! options[:name]   # If menu name, will either auto-complete or suggest creating

      # Add ourself as handler if it looks like a menu.  When Menu.expand
      # tries to run, we'll know whether it actually is a menu.

      (options[:expanders] ||= []).push(self)
    end

    def self.expand options

      return if options[:output] || options[:halt]

      name = options[:name]

      return if ! name   # Don't suggest if it's not menu-like
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
          file = "~/xiki/commands/#{Menu.format_name name}#{extension}"
          file = File.expand_path file
          File.open(file, "w") { |f| f << txt }
          return options[:output] = "<! saved!"
        end

        require "#{Xiki.dir}commands/sample_menus/sample_menus_index.rb" # if !defined?(SampleMenus)

        txt = SampleMenus.by_extension(extension)
        txt ||= SampleMenus.by_extension(".txt") if extension != "."   # Assume text if there's an extension but had no match

        if ! txt
          return options[:output] = "| Doesn't exist yet...\n"+Menu.handlers_with_samples.map{|o| "<< #{name}.#{o}\n"}.join("")
        end

        return options[:output] = Tree.quote(txt, :char=>"|")

      end

      # Don't try to complete if line ends with a slash...

      if options[:path] !~ /\/$/

        # If found any completions, return them...

        if(list = Menu.completions(name)).any?
          if list.length == 1
            options[:output] = "<<< #{list[0]}/"
            return
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

          file = File.expand_path "~/xiki/commands/#{name.gsub(/[ -]/, '_')}#{options[:create_extension]}"
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
          txt = "<<< ip/"
        end

        return options[:output] = txt
      end

      # User had items that weren't in @sample

      # Non-existant items were created, so suggest making a new menu out of them...
      if options[:client] =~ /^editor\b/
        options[:no_slash] = 1

        # Note: If you update this text, be sure update the code in @save menu/
        # that deletes it when saving.
        options[:output] = "
          > Make this into a menu?
          =save menu/
          | Creates a new '#{options[:name]}' menu with these items.
          "
      end
    end

    # For now, just make map of possible menus.
    # Eventually, move them out into other files somewhere probably, to make it easier to add them.
    SUGGESTIONS_WHEN_BLANK = [
      ["config/application.rb", "rails"],
      [".git", "git"],
      [/\.png\/?$/, "scale"],
      [:dir, "chmod"],
      [:dir, "chown"],
      [:file, "specials"],
      [:dir, "dtail"],

      [:file_or_dir, "edits"],
      [:file_or_dir, "ln"],
      [:file_or_dir, "ls"],
      [:file, "tail"],
      [:file, "head"],

      [/\.rb$/, "| You can add '//' at the end of the file to run it as a menu - like this:"],
      [/\.haml$/, "| You can add '//' at the end of the file to run it as a menu - like this:"],
      [/^\/$/, "disk"],
      [/^\/$/, "ip"],
      [/^\/$/, "recent files"],
      [/^\/$/, "servers"],
      [/^\/$/, "network"],
    ]

    # Called when user launches on just an "@" sign
    def self.blank_at ancestors

      # Only worry about last ancestors for now
      ancestor = ancestors[-1]

      result = ""

      is_dir = File.directory? "/tmp/"
      is_file = File.file? "/tmp"
      exists = is_dir || is_file

      SUGGESTIONS_WHEN_BLANK.each do |pattern, menu|
        case pattern
        when :file
          next if ! is_file
        when :dir
          next if ! is_dir
        when String
          next if Dir["#{ancestor}#{pattern}"].blank?
        when Regexp
          next if ancestor !~ pattern
        end

        result << "<< #{menu}/\n"
      end

      result

      # Other menus to add to the list
      # all/
      #   - if rakefile?
      #   << bundler/
      #   - if png file, will it recognize? - make it
      #   << chess/

      #   - how does this compare to double-slash at end?
      #     - should there be a tie-in between them?
      #       - maybe just show explanation in-line about what you can do with that file
      #         - with example of // at end
      #         - or not in-line?
      #       - maybe have "<< //" if it's launchable
      #         | /tmp/
      #         |   + hi.rb
      #         |     @
      #         |       <<< //
      #       - maybe just worry about directories for now?

      #   - maybe when url?
      #   << tail/
      #   << head/

      #   - add "see" for this?
      #   << headers/

      #   - create @browse, and show when .html page?
      #   << browse/
      #   - create these, and also show when a url?
      #   << browse/
      #   << content/
      #   << ip/

      #   - if menu?
      #   << source/
      #   << options/

      #   - or make it just view?
      #   << markdown/

      #   - if svg (would have to make it work on the ancestor file)?
      #   << svg edit/

      #   - how about what finder does?
      #   | Maybe show inline description - of what app the finder would open it with - probably too obvious?

    end

  end
end
