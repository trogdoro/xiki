module Xiki
  class SaveConfig
    def self.save_config xiki_dir, choice

      # Ol "tmp!!!"
      # return "- tmp!!!"

      xiki_dir.sub!(/\/$/, '')   # Remove dir at end

      # puts "SaveConfig.save_config!!!"

      txt = %`
        #!/bin/sh

        # Make the 'xsh' command available in all shells
        export PATH=#{xiki_dir}/bin:$PATH
      `.strip.gsub(/^ */, '')+"\n\n"
      txt << File.read("#{xiki_dir}/misc/install/.xsh.default")
      txt << %`
        # Enable the key shortcuts and the xsh wrapper function
        source #{xiki_dir}/bin/.xsh
      `.strip.gsub(/^ */, '')+"\n\n"

      case choice
      when "default"
        # Do nothing
      when "safer key shortcuts"
        # Change \C to \e\C

        txt.sub! '\C-x', '\e\C-x'
        txt.sub! '\C-s', '\e\C-s'
        txt.sub! '\C-w', '\e\C-w'
        txt.sub! '\C-o', '\e\C-o'
        txt.sub! '\C-g', '\e\C-g'
        txt.sub! '\C-r', '\e\C-r'

      else "just the xsh command"
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

    def self.bash_conf_file
      file = ["~/.bash_profile", "~/.bash_login", "~/.profile"].find{|o| File.exists? File.expand_path(o)}
      file || "~/.bash_profile"   # In case none exists, create this one
    end

    def self.zsh_conf_file
      ["~/.zshrc"].find{|o| File.exists? File.expand_path(o)}
    end

  end
end
