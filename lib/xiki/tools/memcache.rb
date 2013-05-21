gem 'memcached'
require 'memcached'
require 'net/telnet'

module Xiki
  class Memcache
    def self.menu
      txt =
        "
        - .keys/
        - .start/
        "
      txt << "- .server/\n" if View.buffer_open? "*memcached"
      txt
    end

    def self.start
      buffer = "*memcached"
      if View.buffer_open? buffer   # If already open
        View.flash "- '*memcached' is already open!", :times=>4
        return self.server
      end
      Console.run('memcached -vv -p 11211 -m 64', :buffer=>buffer)
    end

    def self.server
      View.to_after_bar
      View.to_buffer "*memcached"
      nil
    end

    def self.suggest_if_not_running e

      if e.message =~ /Connection refused/ || e.message =~ /SystemError: Errno 22:/ || e.message =~ /Errno 22/

        return "
          | > Not installed
          | Memcached doesn't appear to be installed.
          |
          | > Install instructions
          | On a mac
          @$ port install memcached
          |
          | On ubuntu
          @$ sudo apt-get install memcached
          |
          | > Other platforms and more info
          @http://code.google.com/p/memcached/wiki/NewStart
          |
          " if `which memcached`.empty?

        return "
          > Not running
          | Memcached doesn't appear to be running.
          - start it) @memcache/start/"
      end
      nil
    end

    def self.keys *args

      begin

        # If nothing passed, show all keys

        if args.blank?

          # Hack to get all (usually) keys
          con = Net::Telnet::new("Host"=>"127.0.0.1", "Port"=>11211, "Prompt" => /END/)
          items = con.cmd "stats items\n"
          keys = []
          items.scan(/^STAT items:(\d+):number/).each do |i|
            cachedump = con.cmd "stats cachedump #{i[0]} 1000"
            cachedump.scan(/^ITEM (.+?) /).each {|i| keys << i[0]}
          end
          return "
            | > No keys exist
            | Create some:
            - foo/
              | aa
            - bar/
              | b: bb
            " if keys.empty?
          return keys.map{|k| "#{k}/"}
        end

        value = args.pop if Line =~ /^ *\|/
        key = args.join "/"

        # If just key, show the value

        if value.nil?
          return self.connection.get(key).to_yaml.sub(/\A--- \n?/, '').gsub(/^/, "| ")
        end

        if Keys.prefix == 0
          self.connection.delete key
          View.flash "- Deleted!"
          return
        end

        value = YAML::load Tree.siblings(:string=>1)
        self.connection.set key, value

        View.flash "- Saved!"

      rescue Exception=>e
        return "- Not found!\n| sample value" if e.message == "Memcached::NotFound"

        result = self.suggest_if_not_running(e) and return result
        raise e
      end
    end

    def self.connection
      Memcached.new 'localhost:11211' #, :hash=>:default
    end
  end
end
