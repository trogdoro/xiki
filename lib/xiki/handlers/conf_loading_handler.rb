module Xiki
  class ConfLoadingHandler
    def self.handle options

      # Always try loading options[:conf] (from /index.conf or ~/xiki/commands/conf/foo.conf...

      self.load_conf options   # should we avoid loading conf when we're the conf? - probably doesn't matter, since conf/index.conf doesn't exist
    end

    # Populates options[:conf] based on /index.conf or ~/xiki/commands/conf/foo.conf.
    def self.load_conf options

      # Load user conf, if there is any...

      user_conf = File.expand_path "~/xiki/commands/conf/#{options[:name]}.conf"
      if File.file? user_conf
        txt = File.read user_conf
        return options[:conf] = txt
      end

      # None found, so look for default.conf...

      default_conf = "#{options[:menufied]}/default.conf"

      if File.exists? default_conf
        txt = File.read default_conf
        return options[:conf] = txt
      end

    end

  end
end
