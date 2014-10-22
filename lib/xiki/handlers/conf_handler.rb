module Xiki
  class ConfHandler
    def self.handle options

      # When expanding foo/=conf, ~/xiki/commands/conf/conf_index.rb will take over

      source = options[:handlers]['conf']

      return if options[:output] || options[:halt]   # Do nothing if something already handled
      return if ! source || options[:handlers].length > 1   # Only intercede for crud if they're launching a .conf and nothing else

      # Handle crud when expanding ~/xiki/commands/conf/foo...

      name, item = options[:items] # if options[:items]

      # TODO: remove Tree.txt editor dependency?
      options[:output] = self.txt name, item, item, options
    end

    def self.txt name, content=nil, item=nil, options={}

      prefix = options[:prefix]

      custom_conf = "#{Xiki.menu_path_custom_dir}/conf/#{name}.conf"

      # as+open...

      if prefix == "open"
        item.sub! /^\| /, ''

        View.open custom_conf, :to=>"^#{$el.regexp_quote item}$"   # "
        return ""
      end

      # No content, so get it...

      if ! content
        return Tree.quote(File.read(custom_conf), :char=>"|") if File.exists?(custom_conf)

        options = {:name=>name}
        Menu.root_sources_from_path_env options
        default_conf = "#{options[:menufied]}/default.conf"

        return Tree.quote(File.read(default_conf), :char=>"|") if File.exists?(default_conf)

        return "- No conf could be found!"
      end

      # Else, save config...

      self.create_custom_conf_dir   # In case it's not there yet

      File.open(custom_conf, "w") { |f| f << content }

      "<! saved!"
    end


    def self.create_custom_conf_dir

      custom_conf_dir = "#{Xiki.menu_path_custom_dir}/conf/"

      # Do nothing if already there
      return if File.directory? custom_conf_dir

      Dir.mkdir custom_conf_dir

      # Handlers will get control without this
      File.open("#{custom_conf_dir}/conf_index.rb", "w") { |f| f << %`
        #
        # This file proxies control to the core conf menu
        #

        # (when =conf/foo/ and conf/foo.conf doesn't exist yet).
        load "\#{Xiki.menu_path_core_dir}/conf/conf_index.rb"

        # This is here so we'll be expanded as a class
        module Xiki::Menu
          class Conf
          end
        end
        `.unindent }


      # TODO: create this file when creating ~/xiki/commands/conf/
      # ~/xiki/commands/conf/
      #   - index.rb
      #     : # This file proxies control to the core conf menu
      #     : load "#{Xiki.menu_path_core_dir}/conf/conf_index.rb"

    end
  end
end
