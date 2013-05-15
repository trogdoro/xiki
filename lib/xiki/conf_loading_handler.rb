class ConfLoadingHandler
  def self.handle options

    # Always try loading options[:conf] (from /index.conf or ~/menu/conf/foo.conf...

    self.load_conf options   # should we avoid loading conf when we're the conf? - probably doesn't matter, since conf/index.conf doesn't exist
  end

  # Populates options[:conf] based on /index.conf or ~/menu/conf/foo.conf.
  def self.load_conf options
    conf = options[:sources][0].find{|o| o =~ /\.conf$/}  # Grab just the root index.conf
    conf = "#{File.dirname(options[:menufied])}/#{conf}"

    conf = File.file?(conf) ? File.read(conf) : nil

    conf = self.parse conf

    user_conf = Xiki.menu_path_dirs[0]

    user_conf = "~/menu/conf/#{options[:name]}.conf"
    user_conf = File.expand_path user_conf
    user_conf = File.file?(user_conf) ? File.read(user_conf) : ""

    user_conf = self.parse user_conf

    conf.merge! user_conf

    options[:conf] = conf if conf.any?
  end

  def self.parse txt

    result = {}
    return result if ! txt
    Tree.traverse txt do |array, string|
      item = array[0]
     next if ! item
     next if item =~ /^[|>]/   # Do nothing if doesn't start with bullet

      # Don't worry about nesting for now
      match = item.match(/^([+-] )?(.+?): (.+)/)
      next if ! match
      key, val = match[2..3]
      result[key] = val
    end
    # Ol.stack
    result
  end

end
