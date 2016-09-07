module Xiki
  class SetupXsh
    def self.action args, options

      options[:no_search] = 1

      if args == []
        return "
          | Welcome to xsh (Xiki Shell)! Choose the default setup or an
          | alternate setup.
          |
          1. Use the up and down arrow keys to move your cursor to an item below.
          2. Then type Ctrl+O to open (it will perform the setup).
          |
          + default setup
          |
          | The default setup remaps several key shortcuts in bash (or zsh),
          | enables mouse support, downloads some default xiki actions to
          | get you started, and enables searching on xiki.com.
          |
          + details about default setup
          + alternate setups
        ".unindent
      end

      if args == ["default setup"]
        return self.default_setup options

      elsif args == ["details about default setup"]
        return %`
          | This installs the 'xsh' shell command. And it remaps these
          | key shortcuts in bash (or zsh).
          |
          |   Ctrl+O   Opens xsh (when blank prompt). Or type a shell command
          |            and Ctrl+O runs it in xsh.
          |   Ctrl+X   Lists your Xiki files (when blank prompt). Or type a shell
          |            command or topic and Ctrl+X lists its xiki notes and actions.
          |   Ctrl+G   Go to previous spot in xsh (when blank prompt). Or type a
          |            topic and ^G runs its go action.
          |   Ctrl+T   Shows tasks for the current dir. Or type a shell command
          |            and Ctrl+T shows tasks for it.
          |   Ctrl+S   Search notes and actions shared by others to xikihub (xiki.com).
          |   Alt+R    External shell history, like Ctrl+R (type Option+R on the mac)
          |
          | Mouse support will be enabled, so be careful where you click and
          | double-click. Specifically, be aware that when you double-click on a word,
          | it causes an action to take place, rather than the standard behavior of
          | selecting a word. Also, clicking on a "+" sometimes causes actions to run.
          |
          | The default setup downloads and installs several default actions to get
          | you started.
          |
          | Be sure anyone who uses this account knows that the behavior of the above
          | keys has been altered. Note that "stty -ixon" will be set in bash to make
          | Ctrl+S remappable and "stty discard undef" (on mac only) to make Ctrl+O
          | remappable. This changes the behavior of bash slightly, and may have side
          | effects depending on how you use bash. If you're not confident about this,
          | consider using one of the alternate setups.
          |



        `.unindent

      elsif args == ["alternate setups"]

        # Add setup to:
        # | Make ^S optional (and other web connections)?

        return "
          - Todo > add alternate setups!
        ".unindent
      end

      ""
    end


    def self.default_setup options

      require "#{Xiki.dir}misc/install/save_config.rb"

      options[:no_slash] = 1

      SaveConfig.save_config Xiki.dir, "default"

      # Show messages and download from xikihub
      View.message "Updating config... done\nDownloading default notes..."
      View.refresh

      self.download_default_notes
      View.message " "

      Xiki.kill   # So the forker will reload later, in a process that has bin in its path

      return %`
        |
        | > Xsh setup was successful!
        |
        | Do the following steps to reload the shell config in any open
        | terminal windows. Then you'll be able to type 'xsh' at any
        | shell prompt.
        |
        1. Type Ctrl+Q to quit xsh
        2. Follow the steps that appear
        |
        | Xsh is alpha at this point. Experiment, but be careful!
        |
        |
      `.unindent
    end

    def self.download_default_notes

      url = "#{XikihubClient.url}/_activity/install"
      txt = XikihubClient.get url

      txt.split("\n").each do |topic|
        file = File.expand_path("~/xiki/#{topic.snake_case}.xiki")
        # Only download it if not there yet
        next if File.exists? file
        url = "#{XikihubClient.url}/@xiki/#{Notes.heading_to_path topic, :url=>1}"
        txt = XikihubClient.post url, :body=>'{"get": 1}'
        txt.gsub!(/^> /, "> @xiki ")
        File.open(file, "w") { |f| f << txt }
      end

      # Set dates to move these to the top > tutorial, get_more_from_xikihub, creating_notes

      dir = File.expand_path("~/xiki")

      special_files = ["xsh.xiki", "tutorial.xiki", "creating_notes.xiki", "get_more_from_xikihub.xiki", "examples.xiki", "sample_project.xiki", "ls.xiki", "ps.xiki"]
      Dir.entries(dir).select{|o| o !~ /^\.+/}.sort.each_with_index do |file, nth|
        if index = special_files.index(file)
          # Special file, so set date via order
          FileUtils.touch "#{dir}/#{file}", :mtime=>(Time.now - index*60)
        else
          # Other file, so set date after them
          FileUtils.touch "#{dir}/#{file}", :mtime=>(Time.now - (special_files.length + nth)*60)
        end
      end

      ""
    end

  end
end
