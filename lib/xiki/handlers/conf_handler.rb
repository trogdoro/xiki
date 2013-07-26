module Xiki
  class ConfHandler
    def self.handle options

      # When expanding foo/@conf, ~/menu/conf/conf_index.rb will take over

      source = options[:ex]['conf']

      return if options[:output] || options[:halt]   # Do nothing if something already handled
      return if ! source || options[:ex].length > 1   # Only intercede for crud if they're launching a .conf and nothing else

      # Handle crud when expanding ~/menu/conf/foo...

      name, txt = options[:items] # if options[:items]

      # TODO: remove Tree.txt editor dependency?
      options[:output] = self.txt name, (txt ? Tree.txt : nil)
    end

    def self.txt name, content=nil

      # If no content, get it
      custom_conf = "#{Xiki.menu_path_custom_dir}/conf/#{name}.conf"

      if ! content
        return Tree.quote(File.read(custom_conf)) if File.exists?(custom_conf)

        options = {:name=>name}
        Menu.root_sources_from_path_env options
        default_conf = "#{options[:menufied]}/default.conf"

        return Tree.quote(File.read(default_conf)) if File.exists?(default_conf)

        return "- No conf could be found!"
      end

      # Else, save config...

      self.create_custom_conf_dir   # In case it's not there yet

      File.open(custom_conf, "w") { |f| f << content }

      "@flash/- saved!"
    end


    def self.create_custom_conf_dir

      custom_conf_dir = "#{Xiki.menu_path_custom_dir}/conf/"

      # Do nothing if already there
      return if File.directory? custom_conf_dir

      Dir.mkdir custom_conf_dir

      # Handlers will get control without this
      File.open("#{custom_conf_dir}/conf_index.rb", "w") { |f| f << %`
        # This file proxies control to the core conf menu
        # (when @conf/foo/ and conf/foo.conf doesn't exist yet).
        load "\#{Xiki.menu_path_core_dir}/conf/conf_index.rb"

        # This is here so we'll be expanded as a class
        module Xiki::Menu
          class Conf
          end
        end
        `.unindent }


      # TODO: create this file when creating ~/menu/conf/
      # ~/menu/conf/
      #   - index.rb
      #     : # This file proxies control to the core conf menu
      #     : load "#{Xiki.menu_path_core_dir}/conf/conf_index.rb"

    end
  end
end
