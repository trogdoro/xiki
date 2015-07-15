module Xiki::Menu
  class Conf

    include Xiki

    # Can be called 2 ways, nested or not:
    # conf/
    #   - foo/
    #     | content
    # foo/
    #   =conf/
    #     | content
    #     - docs/
    def self.menu_after output=nil, *args  # arg1=nil, arg2=nil

      return if output && args.any?   # MENU handled everything, if there was a path and autput
      options = yield

      # Use ancestor (or ancestor) as menu name if foo/=conf...

      if ancestors = options[:ancestors]
        menuish_ancestor = ancestors[-1][/[\w ]+/]
      end

      # menuish_ancestor = Xiki.menuish_parent options

      # Don't use ancestor it's a herring/=conf/legit situation (if child is item-ish, use it)
      args.unshift menuish_ancestor if menuish_ancestor && (args.blank? || args[0] =~ /\n/)
      name, content = args

      # /..., so list all conf-able menus...

      return self.menus(output) if ! name

      # /foo/... or /foo/|content, so show conf or save...

      options[:no_search] = 1

      item = args[-1]

      # Content seems sufficient
      ConfHandler.txt name, content, item, options
    end


    def self.menus output

      menus = []

      Xiki.menu_path_dirs.each do |path_dir|

        Dir["#{path_dir}/*"].each do |menu_dir|
          conf_path = "#{menu_dir}/default.conf"
          next unless File.exists? conf_path

          menus << "+ #{File.basename menu_dir}/"
        end
      end

      Dir["#{Xiki.menu_path_custom_dir}/conf/*.conf"].each do |conf_file|
        menus << "+ #{File.basename(conf_file, ".*")}/"
      end

      menus.sort!
      menus.uniq!
      menus *= "\n"

      "#{menus}\n#{output}"
    end

end; end
