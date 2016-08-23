module Xiki::Menu
  class Gem

    MENU = "
      - */
        - */
          - .readme/
          - .source/
          - .uninstall/
      - .environment/
      - upgrade rubygems/
        = % gem update --system
        = % gem update --system 1.8.24
      "

    def self.menu_after output, *args

      # =commit/gems > made menu nestable, show gems in menu root.

      # output from MENU, so do nothing...

      return nil if output && args.any?

      dir = Tree.dir || "/tmp/"
      name = args[0]

      # /, so prepend list of gems...

      if ! name
        version = Shell.sync "gem -v", :dir=>dir
        gem_list = Shell.sync "gem list", :dir=>dir
        gem_list = gem_list.gsub(/ \(.+/, "").gsub(/.+/, "+ \\0/")
        gem_list = "|- no gems installed!\n" if gem_list == "\n"
        return "| rubygems #{version}#{gem_list}| Options:\n#{output}"
      end

      # /gem, so list versions...

      txt = Shell.sync "gem list #{name}", :dir=>dir
      versions = txt[/\((.+)\)/, 1]
      versions = versions.split ", "

      versions.map{|o| "- #{o}/\n"}.join
    end

    def self.gem_dir name
      # TODO: use .find_all_by_name instead, and grab the appropreate version
      # Probably require version as a param to this method.
      ::Gem::Specification.find_by_name(name).gem_dir+"/"
    end

    def self.readme name, version

      dir = Gem.gem_dir name

      entries = Dir.new(dir).entries
      file = entries.find{|o| o =~ /^readme/i}

      return "| No readme file found in\n= #{dir}" if ! file

      path = "#{dir}#{file}"

      Tree << "= #{path}"

      Launcher.enter_all
      ""
    end

    def self.source name, version
      dir = Gem.gem_dir name

      "= #{dir}"
    end

    def self.uninstall name, version
      # txt = "sudo gem uninstall #{name} -v #{version}"
      txt = "gem uninstall #{name} -v #{version}"
      Shell.run txt, :dont_move=>1

      "=flash/- Running gem uninstall command in shell..."
    end

    def self.environment
      dir = Tree.dir || "/tmp/"
      Shell.sync("gem environment", :dir=>dir).strip.gsub(/^/, '| ')
      ""
    end

  end
end
