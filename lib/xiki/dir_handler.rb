class DirHandler
  def self.handle options, ex

    # This will soon be refactored to be called by hash
    return if ! ex['/'] || options[:output] || options[:halt]

    items = Dir.new("#{options[:last_source_dir]}#{ex['/']}").entries.grep /^[^.]/

    items.delete "default.conf"   # ignore default conf

    items.map!{|o| File.basename(o, ".*")}   # no extensions

    items.uniq!   # no dups
    items.delete "index"   # no indexes
    items.map!{|o| "+ #{o}/\n"}   # make + foo/\n

    return if items.empty?
    items = items.join("")
    options[:output] = items

  end
end
