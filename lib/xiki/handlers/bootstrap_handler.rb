module Xiki
  class BootstrapHandler

    def self.handle options
      source = options[:ex]['bootstrap']

      return if ! source || options[:output] || options[:halt]

      txt = File.read "#{options[:enclosing_source_dir]}#{source}"

      if options[:client] == "web"
        require "#{Xiki.dir}menu/bootstrap.rb" if ! defined? Xiki::Menu::Bootstrap
        txt = Xiki::Menu::Bootstrap.render txt
      end
      options[:output] = txt
    end
  end
end
