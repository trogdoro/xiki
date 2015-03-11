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

    MENU_HIDDEN = "
      - */
        - .save these changes/
      "

    MENU = %`
      | Explore these menu items to find the setup that's right for
      | you. Then enable one of them. (Use the arrow keys and type
      | Ctrl+X to expand and collapse the items below).
      |
      + the 'xsh' command, and some shell key shortcut remappings/
        | Enables the 'xsh' shell command. Also remaps the following
        | key shortcuts in your existing shell, to make them quickly
        | switch to xsh:
        |
        |   Ctrl+X   Expand a shell command in xsh
        |   Ctrl+T   Menu of all tasks for a command or dir
        |   Ctrl+G   Grab shell commands to and from Xiki
        |   Ctrl+R   Recent commands (like existing Ctrl+R)
        |   Esc+Tab  Xiki command menu
        |
        + enable this configuration
          =replace/siblings/
            | Expand 'save these changes', to save the following changes.
            |
            + save these changes
            |
#{self.files_to_update_preview.gsub(/^/, '            ')}
            |
        |
        | This is recommended if you (and any other people who use this
        | account) don't use Ctrl+X, Ctrl+T, and Ctrl+G in your shell.
        |
        | (You can use the arrow keys and Ctrl+X to select the above item.)
        |
      - the 'xsh' command, and safer key shortcuts/
        | This is like the previous option, but creates key shortcuts
        | that don't conflict with your shells default shortcuts:
        |
        |   Esc, Ctrl+X  Expand a shell command in xsh
        |   Esc, Ctrl+T  Menu of possible tasks for a command or dir
        |   Esc, Ctrl+G  Grab shell commands to and from Xiki
        |   Esc, Ctrl+R  Recent commands (like existing Ctrl+R)
        |   Esc+Tab     Auto-complete
        |
        + enable this configuration
          =replace/siblings/
            | Expand 'save these changes', to save the following changes.
            |
            + save these changes
            |
#{self.files_to_update_preview.gsub(/^/, '            ')}
            |
        |
        | (You can use the arrow keys and Ctrl+X to select the above item.)
        |
      + just the 'xsh' command/
        | Just install the 'xsh' command, and don't add any key
        | shortcuts in your existing shell.
        |
        + enable this configuration
          =replace/siblings/
            | Expand 'save these changes', to save the following changes.
            |
            + save these changes
            |
#{self.files_to_update_preview.gsub(/^/, '            ')}
            |
        |
    `

    def self.save_these_changes choice

      options = yield
      options[:no_slash] = 1

      txt = %`
        #!/bin/sh

        # Make the 'xsh' command available in all shells
        export PATH=#{Xiki.dir}bin:$PATH
      `.unindent+"\n"
      txt << File.read("#{Xiki.dir}misc/install/.xsh.default")
      txt << %`
        # Enable the key shortcuts and the xsh wrapper function
        source #{Xiki.dir}bin/.xsh
      `.unindent

      case choice
      when /some shell key shortcut remappings/
        # Do nothing
      when /safer key shortcuts/
        # Change \C to \e\C

        txt.sub! '\C-x', '\e\C-x'
        txt.sub! '\C-t', '\e\C-t'
        txt.sub! '\C-g', '\e\C-g'
        txt.sub! '\C-r', '\e\C-r'

      else   # just the 'xsh' command
        # Comment everything out

        txt.gsub!(/^[xs]/, "# \\0")

        txt.gsub!("# Define", "\n# Commented out, since user chose no key shortcuts:\n\n\\0")
      end

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

      %`
        =replace/siblings/2/
          |+Success!
          |
          | Run ". ~/.bashrc" (or the appropriate command) in any
          | currently open shell sessions to reload the conf, or start
          | new sessions. You can now type 'xsh' from any directory.
          |
          | Xsh is alpha at this point. Experiment, but be careful!
          |
          | Expand this item for help with using Xiki:
          |
          << help
      `.unindent

    end
  end
end
