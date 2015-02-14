module Menu

  class MakeXshAvailable

    def self.bash_conf_file
      ["~/.bash_profile", "~/.bash_login", "~/.profile"].find{|o| File.exists? File.expand_path(o)}# || "fu"
    end

    def self.zsh_conf_file
      ["~/.zshrc"].find{|o| File.exists? File.expand_path(o)}
    end


    def self.files_to_update

      # Start with files that exist (plus always .bashrc)...

      files = [
        "~/.bashrc",
        self.bash_conf_file,
        self.zsh_conf_file,
      ].compact

      # Remove ones that exist and contain "source ~/.xsh" already

      files = files.select do |file|
        txt = File.read(File.expand_path file) rescue nil
        next true if ! txt   # Only .bashrc might not exist, and we still want to create it, so leave it in the list
        next false if txt =~ /^source ~\/\.xsh\b/   # It's already there, so do nothing
        true
      end

      files
    end

    def self.files_to_update_preview
      files = self.files_to_update

      txt = files.map do |file|
        "
        #{file}
          :+source ~/.xsh
        ".unindent.strip
      end.join("\n")

      # Add if > .xsh exists:
      if File.exists? File.expand_path("~/.xsh")
        txt << "\n"+%`
          ~/.xsh
            :?Will be updated with new configuration
        `.unindent.strip
      end

      txt.strip
    end

    MENU = %`
      | Explore these menu items to find the setup that's right
      | for you. (Use the arrow keys and type Ctrl+X to expand
      | and collapse each of them).
      |
      + the 'xsh' command, and remapped key shortcuts/
        | Enable the 'xsh' shell command. Also remap the following
        | key shortcuts in your existing shell, to make them quickly
        | switch to xsh:
        |
        |   Ctrl+X  Expand a shell shell command in xsh
        |   Ctrl+D  Dropdown of options for a shell command
        |   Ctrl+G  Grab shell commands to and from Xiki
        |   Escape+Tab     Auto-complete
        |   Escape+Ctrl+R  Shell history
        |
        | This is recommended if you (and any other people who use this
        | account) don't use Ctrl+X, Ctrl+D, and Ctrl+G in your shell.
        |
        + enable this configuration
          | Expand 'save' below, and the following changes will be made:
          |\n#{self.files_to_update_preview.gsub(/^/, '          ')}
          |
          + .save
          |
        |
        | (You can use the arrow keys and Ctrl+X to select the above item.)
        |
      - xsh 'xsh' command, and safer key shortcuts/
        | This is like the previous option, but creates key shortcuts
        | that don't conflict with your shells default shortcuts:
        |
        |   Escape+Ctrl+X  Expand a shell shell command in xsh
        |   Escape+Ctrl+D  Dropdown of options for a shell command
        |   Escape+Ctrl+G  Grab shell commands to and from Xiki
        |   Escape+Tab     Auto-complete
        |   Escape+Ctrl+R  Shell history
        |
        + enable this configuration
          | Expand 'save' below, and the following changes will be made:
          |\n#{self.files_to_update_preview.gsub(/^/, '          ')}
          |
          + .save
          |
        |
        | (You can use the arrow keys and Ctrl+X to select the above item.)
        |
      + just the 'xsh' command/
        | Just install the 'xsh' command, and don't add any key
        | shortcuts in your existing shell.
        |
        + enable this configuration
          | Expand 'save' below, and the following changes will be made:
          |\n#{self.files_to_update_preview.gsub(/^/, '          ')}
          |
          + .save
          |
        |
    `

    def self.save choice, confirmed=nil

      options = yield
      options[:no_slash] = 1

      txt = %`
        #!/bin/sh

        # Make the 'xsh' command available in all shells
        export PATH=$PATH:#{Xiki.dir}bin
      `.unindent

      txt << "\n"

      case choice
      when /remapped key shortcuts/
        txt << %`
          # Define some key shortcuts for quickly switching from your shell to xsh
          xiki_open_key="\\C-x"        # Ctrl+X to expand in xsh
          xiki_dropdown_key="\\C-d"    # Ctrl+D to show a dropdown menu for a command
          xiki_grab_key="\\C-g"        # Ctrl+G to grab commands to and from xsh
          xiki_tab_key="\\e\\C-i"       # Escape+Tab to do autocomplete
          xiki_reverse_key="\\e\\C-r"   # Escape+Ctrl+R to search shell history

          # Some examples, in case you want to update this file:
          # xiki_open_key="\\C-x"      # Ctrl+X
          # xiki_open_key="\\e\\C-X"    # Escape+Ctrl+X

          # Enable the key shortcuts and the xsh wrapper function
          source #{Xiki.dir}bin/.xsh
        `.unindent
      when /safer key shortcuts/
        txt << %`
          # Define some key shortcuts for quickly switching from your shell to xsh
          xiki_open_key="\\e\\C-x"        # Escape+Ctrl+X to expand in xsh
          xiki_dropdown_key="\\e\\C-d"    # Escape+Ctrl+D to show a dropdown menu for a command
          xiki_grab_key="\\e\\C-g"        # Escape+Ctrl+G to grab commands to and from xsh
          xiki_tab_key="\\e\\C-i"       # Escape+Tab to do autocomplete
          xiki_reverse_key="\\e\\C-r"   # Escape+Ctrl+R to search shell history

          # Some examples, in case you want to update this file:
          # xiki_open_key="\\C-x"      # Ctrl+X
          # xiki_open_key="\\e\\C-X"    # Escape+Ctrl+X

          # Enable the key shortcuts and the xsh wrapper function
          source #{Xiki.dir}bin/.xsh
        `.unindent
      else
        txt << %`
          # Commented out, since user chose no key shortcuts:
          #
          # Define some key shortcuts for quickly switching from your shell to xsh
          # xiki_open_key="\\C-x"        # Ctrl+X to expand in xsh
          # xiki_dropdown_key="\\C-d"    # Ctrl+D to show a dropdown menu for a command
          # xiki_grab_key="\\C-g"        # Ctrl+G to grab commands to and from xsh
          # xiki_tab_key="\\e\\C-i"       # Escape+Tab to do autocomplete
          # xiki_reverse_key="\\e\\C-r"   # Escape+Ctrl+R to search shell history
          #
          # Some examples, in case you want to update this file:
          # xiki_open_key="\\C-x"      # Ctrl+X
          # xiki_open_key="\\e\\C-X"    # Escape+Ctrl+X
          #
          # Enable the key shortcuts and the xsh wrapper function
          # source #{Xiki.dir}bin/.xsh
        `.unindent
      end

      txt << "\n"

      dot_xsh_path = File.expand_path("~/.xsh")

      # Make backup if it exists
      if File.exists? dot_xsh_path
        FileUtils.cp dot_xsh_path, "#{dot_xsh_path}.backup.#{rand 9999}"
      end

      # Write the new file...

      File.open(dot_xsh_path, "w") { |f| f << txt }

      # Include it from the other files...


      files = self.files_to_update

      files.each do |file|
        file = File.expand_path file
        txt = File.read(file) rescue ""

        File.open(file, "w") { |f| f << "#{txt.sub(/\n+\z/, '')}\n\n\nsource ~/.xsh\n\n" }
      end

      Xiki.kill   # So the forker will reload later, in a process that has bin in its path

      "
        =replace/siblings/3/
          |+Success!
          |
          | Your shell conf files have been updated. You can now type
          | 'xsh' from any directory.
          |
          | Note: be sure to close any existing shell sessions before
          | trying it, since config updates won't be reflected in them.
          |
          | Xsh is alpha at this point. Experiment, but be careful!
          |
          | Expand this item for help with using Xiki:
          |
          << help
      ".unindent

    end
  end
end
