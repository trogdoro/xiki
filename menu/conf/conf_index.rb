module Xiki::Menu
  class Conf

    include Xiki

    MENU = "
      > options
      - docs/
        > __
      "

    # Can be called 2 ways, nested or not:
    # conf/
    #   - foo/
    #     | content
    # foo/
    #   @conf/
    #     | content
    #     - docs/
    def self.menu_after output=nil, *args  # arg1=nil, arg2=nil

      return if output && args.any?   # MENU handled everything, if there was a path and autput
      options = yield

      # Use parent as menu name if foo/@conf...

      menuish_parent = Xiki.menuish_parent options

      # Don't use parent if there's a foo/@conf/bar item that overrides
      args.unshift menuish_parent if menuish_parent && (args.blank? || args[0] =~ /^\|/)
      name, content = args

      # /..., so list all conf-able menus...

      return self.menus(output) if ! name

      # /foo/... or /foo/|content, so show conf or save...

      options[:no_search] = 1

      item = args[-1]

      # TODO: remove Tree.txt editor dependency?
      ConfHandler.txt name, (content ? Tree.txt : nil), item, options
    end


    def self.menus output

      menus = []

      Xiki.menu_path_dirs.each do |path_dir|

        Dir["#{path_dir}/*"].each do |menu_dir|
          #Ol "menu_dir", menu_dir
          conf_path = "#{menu_dir}/default.conf"
          next unless File.exists? conf_path

          menus << "- #{File.basename menu_dir}/"
        end
      end

      Dir["#{Xiki.menu_path_custom_dir}/conf/*.conf"].each do |conf_file|
        menus << "- #{File.basename(conf_file, ".*")}/"
      end

      menus.sort!
      menus.uniq!
      menus *= "\n"

      "#{menus}\n#{output}"
    end

end; end
