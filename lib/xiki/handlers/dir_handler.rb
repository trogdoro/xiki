module Xiki
  class DirHandler
    def self.handle options

      source = options[:ex]['/']
      # This will soon be refactored to be called by hash
      return if ! source || options[:output] || options[:halt]

      items = Dir.new("#{options[:last_source_dir]}#{source}").entries.grep /^[^.]/

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
end
