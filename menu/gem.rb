module Xiki::Menu
  class Gem
    MENU = "
      - .list/
        - */
          - */
            - .readme/
            - .source/
            - .uninstall/
      - .environment/
      "

    def self.list name=nil

      # /..., so list noames

      if name.nil?
        gem_list = Console.sync("gem list")
        return gem_list.gsub(/ \(.+/, "").gsub(/.+/, "- \\0/")
      end

      # /fooo, so list versions...

      txt = Console.sync("gem list #{name}")
      versions = txt[/\((.+)\)/, 1]
      versions = versions.split ", "

      return versions.map{|o| "#{o}/"}

    end

    def self.uninstall name, version
      txt = "sudo gem uninstall #{name} -v #{version}"
      Console.run txt, :dont_move=>1

      "@flash/- Running gem uninstall command in shell..."
    end

    def self.gem_dir name
      # TODO: use .find_all_by_name instead, and grab the appropreate version
      # Probably require version as a param to this method.
      ::Gem::Specification.find_by_name(name).gem_dir+"/"
    end

    def self.source name, version
      dir = Gem.gem_dir name

      "@#{dir}"
    end

    def self.readme name, version

      dir = Gem.gem_dir name

      entries = Dir.new(dir).entries
      file = entries.find{|o| o =~ /^readme/i}

      return "| No readme file found in\n@#{dir}" if ! file

      path = "#{dir}#{file}"

      Tree << "@#{path}"

      Launcher.enter_all
      nil
    end

    def self.environment
      Console.sync("gem environment").strip.gsub(/^/, '| ')
    end

  end
end
