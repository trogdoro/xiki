module Xiki
  class MenuSuggester

    def self.expands? options

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

      return nil if ! name   # Don't suggest if it's not menu-like

      # Don't try to complete if line doesn't end with a slash...

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

      txt = Expander.expand "sample menus", options[:items]   # Will handle if no items or a sample menu item

      if txt
        txt.gsub! "<name>", name
        txt.gsub! "<Name>", TextUtil.camel_case(name)
        options[:output] = txt
        return
      end

      # User had items that weren't in @sample

      # Non-existant items were created, so suggest making a new menu out of them...
      if options[:client] =~ /^editor\b/
        options[:no_slash] = 1

        # Note: If you update this text, be sure update the code in @save menu/
        # that deletes it when saving.
        options[:output] = "
          > Make this into a menu?
          @save menu/
          | Creates a new '#{options[:name]}' menu with these items.
          "
      end

      # Shelved for now
      #     return "@back up/1/#{name}...\n#{completions}"

    end

  end
end
