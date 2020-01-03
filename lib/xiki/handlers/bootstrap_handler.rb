module Xiki
  class BootstrapHandler

    def self.handle options
      source = options[:handlers]['bootstrap']

      return if ! source || options[:output] || options[:halt]

      txt = File.read "#{options[:enclosing_source_dir]}#{source}"

      if options[:client] == "web"
        require "#{Xiki.dir}roots/bootstrap.rb" if ! defined? Xiki::Menu::Bootstrap
        txt = Xiki::Menu::Bootstrap.render txt
      end
      options[:output] = txt
    end
  end
end
