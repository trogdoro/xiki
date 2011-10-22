gem 'memcached'
require 'memcached'
require 'net/telnet'

class Memcached
  def self.start
    buffer = "*memcached"
    return View.message("*memcached already open", :beep=>1) if View.buffer_open? buffer
    Console.run('memcached -vv -p 11211 -m 64', :buffer=>buffer)
  end

  def self.keys *args
    if args.blank?   # If nothing passed, show all keys

      con = Net::Telnet::new("Host"=>"127.0.0.1", "Port"=>11211, "Prompt" => /END/)
      items = con.cmd "stats items\n"
      keys = []
      items.scan(/^STAT items:(\d+):number/).each do |i|
        cachedump = con.cmd "stats cachedump #{i[0]} 100"
        cachedump.scan(/^ITEM (.+?) /).each {|i| keys << i[0]}
      end
      return keys.map{|k| "#{k}/"}
    end

    value = args.reverse.find{|o| o =~ /\A---/}
    args.pop if value

    key = args.join "/"


    if value.nil?   # If no value yet, show the value
      return self.connection.get(key).to_yaml.gsub /^/, "| "
    end

    if Keys.prefix == 0
      self.connection.delete key
      return "- deleted!"
    end

    value = YAML::load value
    self.connection.set key, value

    "- saved!"
  end

  def self.connection
    Memcached.new 'localhost:11211' #, :hash=>:default
  end

  def self.menu
    "
    - .keys/
    - .start/
    - help/
      | > Why?
      | Access memcached.
      |
      | > List keys
      - @memcached/.keys/
      |
      | > Show one key
      - @memcached/.keys/foo/
    - help2/
      | > Why?
      | Access memcached.
      |
      | > List keys
      | - @memcached/.keys/
      |
      | > Save one key
      | - @memcached/.keys/foo/
      |   | ---
      |   | a: aa
      |   | b: bb
    "
  end
end

# Launcher.remove "memcached_tree"
Launcher.add "memcached"

